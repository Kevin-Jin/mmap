#include <R.h>
#include <Rinternals.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>

#include <assert.h>
#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS
#include <stdint.h>

#include "mmap.h"

#ifndef WIN32
#ifdef HAVE_MMAP
#  include <sys/mman.h>
#endif
#else
#include <ctype.h>
#include <stdio.h>
#include <windows.h>
#undef HAVE_MADVISE
#endif

/*
Use the union initializer list hack in the absence of `reinterpret_cast` to type
 pun, i.e. "cast" between two same-size types by preserving the bit pattern
 rather than the conceptual value.
It is better supported by strict-aliasing compilers than the `*(as_type*)&x`
 hack, despite both having undefined behavior, and it can be done in one line
 whereas the `*(as_type*)&x` hack cannot because we cannot take the address of
 an rvalue in the manner of `*(typeof(x)*)&__builtin_bswap32(*(uint32_t*)&(x))`.
The most well-defined approach would be to create a temporary uintN_t variable
 and memcpy `x` to it, but this cannot be done in one line. Optimizing compilers
 will likely have an intrinsic for this pattern and manage to no-op it though.
*/
#define bitw_cast(as_type, x) ((union{typeof(x) y; as_type z;}){x}.z)
#define swapb16(x) (bitw_cast(typeof(x),__builtin_bswap16(bitw_cast(uint16_t,x))))
#define swapb32(x) (bitw_cast(typeof(x),__builtin_bswap32(bitw_cast(uint32_t,x))))
#define swapb64(x) (bitw_cast(typeof(x),__builtin_bswap64(bitw_cast(uint64_t,x))))
/*
__builtin_bswap32() and __builtin_bswap64() were defined in GCC 4.2.0 but
 __builtin_bswap16() wasn't defined until GCC 4.8.0. The earliest supported
 version of R should be 2.6.0 since it was built with GCC 4.2.1 before moving to
 GCC 4.5.2 in R 2.12.0 and GCC 4.6.3 in R 2.14.2.
*/
#if __GNUC__ < 4 || __GNUC__ == 4 && __GNUC_MINOR__ < 8
static inline uint16_t __builtin_bswap16(uint16_t x){return(x<<8)|(x>>8);}
#endif

// There are some platforms out there where `char` is not synonymous with octet.
// Since by definition, byte is synonymous with char and `sizeof(char) == 1`,
//  a byte would not be 8-bits so `SMODE_CBYTES` and `memcpy` calls will break.
#if CHAR_BIT != 8
#error "`char` is not 8-bits"
#endif

// For the sake of allowing uninterrupted counting from 0 to overflow, NA values
//  for unsigned types should be equivalent to UINTn_MAX.
#define NA_UINT8   ((uint8_t)  UINT8_C(0xFF))
#define NA_UINT16 ((uint16_t) UINT16_C(0xFFFF))
#define NA_UINT32 ((uint32_t) UINT32_C(0xFFFFFFFF))
// For consistency with NA_INTEGER, NA values for signed types should be
//  equivalent to INTn_MIN. This is convenient because the two's complement
//  negation of any positive number is always representable and because any
//  operation that overflows should result in NA anyway; the operation
//  `INTn_MAX + 1`, which typically overflows to `INTn_MIN`, is no exception.
// Technically, whether the bit pattern is kept in the below casts is compiler
//  implementation defined, but the C99 standard defines that intN_t must be
//  two's complement so gcc/mingw would very likely behave as we would expect.
#define NA_INT8     ((int8_t)   INT8_C(0x80))
#define NA_INT16   ((int16_t)  INT16_C(0x8000))
#define NA_INT32   ((int32_t)  INT32_C(0x80000000))
#define NA_INT64   ((int64_t)  INT64_C(0x8000000000000000))
// Signaling NaN (quiet is 0x7FC0...) with Ross Ihaka's birth year as a payload.
// Usually sNaN is 0x7FA0... to avoid setting the quiet bit yet also avoiding
//  a payload of 0 (which represents +Inf), but `1954 != 0` so 0x7F80... is OK.
#define NA_FLOAT  (bitw_cast(float,(uint32_t)(UINT16_C(1954)|UINT32_C(0x7F800000))))
// Signaling NaN (quiet is 0x7FF8...) with Ross Ihaka's birth year as a payload.
// Usually sNaN is 0x7FF4... to avoid setting the quiet bit yet also avoiding
//  a payload of 0 (which represents +Inf), but `1954 != 0` so 0x7FF0... is OK.
#define NA_DOUBLE (bitw_cast(double,(uint64_t)(UINT16_C(1954)|UINT64_C(0x7FF0000000000000))))
// These are useful when comparing the payloads of two NaN values for equality
//  since `NaN == NaN` is defined to always be false.
#define bitw_eq32(x, y) (bitw_cast(uint32_t, x) == bitw_cast(uint32_t, y))
#define bitw_eq64(x, y) (bitw_cast(uint64_t, x) == bitw_cast(uint64_t, y))

/*
The "mmap" package for R is designed to provide a
low level interface to the POSIX mmap C function
call.  Additional work has been done to make the
R interface friendly enough to the R user, but at
the same time transparent enough to allow for
the underlying system documentation to be used
to manage the memory map functionality.

This package implements all mmap-related calls:

  mmap
  munmap
  msync
  mprotect

The conversion mechnisms to deal with translating
raw bytes as returned by mmap into R level SEXP are
abstracted from the user but handled in the C code.
At present he may read data as R types: "raw", 
"integer", and "double".

Future work will entail support for more on-disk
types converted into R SEXP upon extraction, as well
as the addition of a smart finalizer.

Comments, criticisms, and concerns should be directed
to the maintainer of the package.
*/

/* initialize bitmask for bitset() type {{{*/
int bitmask[32];
int nbitmask[32];

void create_bitmask (void){
  int i;
  /* little-endian for now */
  for(i=0; i<32; i++) {
     bitmask[i] = 1 << i;
    nbitmask[i] = ~bitmask[i];
  }
}

SEXP make_bitmask () {
  create_bitmask();
  return R_NilValue;
} /*}}}*/

/* mmap_mkFlags {{{ */
SEXP mmap_mkFlags (SEXP _flags) {
  char *cur_string;
  int len_flags = length(_flags);
  int flags_bit = 0x0;
  int i;

  for(i=0; i < len_flags; i++) {
    cur_string = (char *)CHAR(STRING_ELT(_flags,i));
    if(strcmp(cur_string,"PROT_READ")==0) {
      flags_bit = flags_bit | PROT_READ; continue;
    } else
    if(strcmp(cur_string,"PROT_WRITE")==0) {
      flags_bit = flags_bit | PROT_WRITE; continue;
    } else
    if(strcmp(cur_string,"PROT_EXEC")==0) {
      flags_bit = flags_bit | PROT_EXEC; continue;
    } else
    if(strcmp(cur_string,"PROT_NONE")==0) {
      flags_bit = flags_bit | PROT_NONE; continue;
    } else
    if(strcmp(cur_string,"MS_ASYNC")==0) {
      flags_bit = flags_bit | MS_ASYNC; continue;
    } else
    if(strcmp(cur_string,"MS_SYNC")==0) {
      flags_bit = flags_bit | MS_SYNC; continue;
    } else
    if(strcmp(cur_string,"MS_INVALIDATE")==0) {
      flags_bit = flags_bit | MS_INVALIDATE; continue;
    } else
    if(strcmp(cur_string,"MAP_SHARED")==0) {
      flags_bit = flags_bit | MAP_SHARED; continue;
    } else
    if(strcmp(cur_string,"MAP_PRIVATE")==0) {
      flags_bit = flags_bit | MAP_PRIVATE; continue;
    } else
    if(strcmp(cur_string,"MAP_FIXED")==0) {
      flags_bit = flags_bit | MAP_FIXED; continue;

#ifdef HAVE_MADVISE
    } else
    if(strcmp(cur_string,"MADV_NORMAL")==0) {
      flags_bit = flags_bit | MADV_NORMAL; continue;
    } else
    if(strcmp(cur_string,"MADV_RANDOM")==0) {
      flags_bit = flags_bit | MADV_RANDOM; continue;
    } else
    if(strcmp(cur_string,"MADV_SEQUENTIAL")==0) {
      flags_bit = flags_bit | MADV_SEQUENTIAL; continue;
    } else
    if(strcmp(cur_string,"MADV_WILLNEED")==0) {
      flags_bit = flags_bit | MADV_WILLNEED; continue;
    } else
    if(strcmp(cur_string,"MADV_DONTNEED")==0) {
      flags_bit = flags_bit | MADV_DONTNEED; continue;
#endif

    } else {
      warning("unknown constant: skipped");
    }
  }
#ifdef WIN32
  flags_bit = PAGE_READWRITE;
  if(flags_bit & PROT_READ & PROT_WRITE & MAP_SHARED)
    flags_bit = PAGE_READWRITE;
  else if(flags_bit & MAP_PRIVATE)
    flags_bit = PAGE_WRITECOPY;
  else if(flags_bit & PROT_READ)
    flags_bit = PAGE_READONLY;
#endif
  return ScalarInteger(flags_bit);
} /*}}}*/

/* mmap_munmap {{{ */
#ifdef WIN32
SEXP mmap_munmap (SEXP mmap_obj) {
  int ret;
  char *data = MMAP_DATA(mmap_obj);
  HANDLE fd = (HANDLE)MMAP_FD(mmap_obj);
  HANDLE mh = (HANDLE)MMAP_HANDLE(mmap_obj);

  if(data == NULL)
    error("invalid mmap pointer");

  ret = UnmapViewOfFile(data);
  CloseHandle(mh);
  CloseHandle(fd);
  R_ClearExternalPtr(findVar(install("data"),mmap_obj));
  return(ScalarInteger(ret));
}
#else
SEXP mmap_munmap (SEXP mmap_obj) {
  char *data = MMAP_DATA(mmap_obj);
  int fd = MMAP_FD(mmap_obj);

  if(data == NULL)
    error("invalid mmap pointer");

  int ret = munmap(data, MMAP_SIZE(mmap_obj));
  close(fd); /* should be moved back to R */
  //R_ClearExternalPtr(VECTOR_ELT(mmap_obj,0));
  R_ClearExternalPtr(findVar(install("data"),mmap_obj));
  /*R_ClearExternalPtr(MMAP_DATA(mmap_obj));*/
  return(ScalarInteger(ret)); 
} /*}}}*/

/*
void mmap_finalizer (SEXP mmap_obj) {
  Rprintf("mmap_finalizer called\n");
  mmap_munmap((SEXP)R_ExternalPtrAddr(mmap_obj));
}
*/
#endif

/* mmap_mmap AND mmap_finalizer {{{ */
#ifdef WIN32
SEXP mmap_mmap (SEXP _type, SEXP _fildesc, SEXP _prot,
                SEXP _flags, SEXP _len, SEXP _off, SEXP _pageoff) {
  char *data;
  struct stat st;
  SYSTEM_INFO sSysInfo;
  GetSystemInfo(&sSysInfo);

  stat(CHAR(STRING_ELT(_fildesc,0)), &st);

  HANDLE hFile=CreateFile(CHAR(STRING_ELT(_fildesc,0)),
                  GENERIC_READ|GENERIC_WRITE,
                  FILE_SHARE_READ|FILE_SHARE_WRITE,NULL,OPEN_EXISTING,0,NULL);

  HANDLE hMap=CreateFileMapping(hFile,NULL,PAGE_READWRITE,0,0,NULL);
  DWORD dwFileSize=GetFileSize(hFile,NULL);
  data = (char *)MapViewOfFile(hMap,FILE_MAP_WRITE,0,0,dwFileSize);
  /* advance ptr to byte offset from page boundary - shouldn't we do this above?? JR */
  data = data + asInteger(_off) + asInteger(_pageoff); 


  SEXP mmap_obj;
  PROTECT(mmap_obj = allocSExp(ENVSXP));
  SET_FRAME(mmap_obj, R_NilValue);
  SET_ENCLOS(mmap_obj, R_NilValue);
  SET_HASHTAB(mmap_obj, R_NilValue);
  SET_ATTRIB(mmap_obj, R_NilValue);
  defineVar(install("data"), R_MakeExternalPtr(data, R_NilValue, R_NilValue),mmap_obj);
  //defineVar(install("bytes"), ScalarReal(asReal(_len)-asInteger(_off)-asInteger(_pageoff)),mmap_obj);
  defineVar(install("bytes"), _len,mmap_obj);
  defineVar(install("filedesc"), ScalarInteger((int)hFile),mmap_obj);
  defineVar(install("storage.mode"), _type,mmap_obj);
  defineVar(install("pagesize"), ScalarReal((double)sSysInfo.dwPageSize),mmap_obj);
  defineVar(install("handle"), ScalarInteger((int)hMap),mmap_obj);
  defineVar(install("dim"), R_NilValue ,mmap_obj);
  UNPROTECT(1);
  return(mmap_obj);
}
#else
SEXP mmap_mmap (SEXP _type, SEXP _fildesc, SEXP _prot,
                SEXP _flags, SEXP _len, SEXP _off, SEXP _pageoff) {
  int fd;
  char *data;
  struct stat st;

  stat(CHAR(STRING_ELT(_fildesc,0)), &st);
  fd = open(CHAR(STRING_ELT(_fildesc,0)), O_RDWR);
  if(fd < 0)
    error("unable to open file");
  data = mmap((caddr_t)0, 
              (size_t)REAL(_len)[0], 
              INTEGER(_prot)[0], 
              INTEGER(_flags)[0], 
              fd, 
              INTEGER(_off)[0]);

  if(data == MAP_FAILED)
    error("unable to mmap file");
  data = data + asInteger(_pageoff); /* advance ptr to byte offset from page boundary */
  
  SEXP mmap_obj;
  PROTECT(mmap_obj = allocSExp(ENVSXP));
  SET_FRAME(mmap_obj, R_NilValue);
  SET_ENCLOS(mmap_obj, R_NilValue);
  SET_HASHTAB(mmap_obj, R_NilValue);
  SET_ATTRIB(mmap_obj, R_NilValue);
  defineVar(install("data"), R_MakeExternalPtr(data, R_NilValue, R_NilValue),mmap_obj);
  //defineVar(install("bytes"), ScalarReal(asReal(_len)-asInteger(_off)-asInteger(_pageoff)),mmap_obj);
  defineVar(install("bytes"), _len,mmap_obj);
  defineVar(install("filedesc"), ScalarInteger(fd),mmap_obj);
  defineVar(install("storage.mode"), _type,mmap_obj);
  defineVar(install("pagesize"), ScalarReal((double)sysconf(_SC_PAGE_SIZE)),mmap_obj);
  defineVar(install("dim"), R_NilValue ,mmap_obj);
  UNPROTECT(1);
  return(mmap_obj);
} /*}}}*/
#endif

/* mmap_pagesize {{{ */
#ifdef WIN32
SEXP mmap_pagesize () {
  SYSTEM_INFO sSysInfo;
  GetSystemInfo(&sSysInfo);
  return ScalarInteger((int)sSysInfo.dwPageSize);
}
#else
SEXP mmap_pagesize () {
  return ScalarInteger((int)sysconf(_SC_PAGE_SIZE));
}
#endif
/*}}}*/

/* mmap_is_mmapped {{{ */
SEXP mmap_is_mmapped (SEXP mmap_obj) {
  char *data = MMAP_DATA(mmap_obj);
  if(data == NULL)
    return(ScalarLogical(0));

  return(ScalarLogical(1));
} /*}}}*/

#ifdef WIN32
/* {{{ mmap_msync */
SEXP mmap_msync (SEXP mmap_obj, SEXP _flags) {
  char *data;
  data = MMAP_DATA(mmap_obj);
  FlushViewOfFile((void *)data, (size_t)MMAP_SIZE(mmap_obj));
  return 0;
}
#else
SEXP mmap_msync (SEXP mmap_obj, SEXP _flags) {
  char *data;
  data = MMAP_DATA(mmap_obj);
  int ret = msync(data, MMAP_SIZE(mmap_obj), INTEGER(_flags)[0]);
  return ScalarInteger(ret);
}/*}}}*/
#endif

/* {{{ mmap_madvise */
SEXP mmap_madvise (SEXP mmap_obj, SEXP _len, SEXP _flags) {
  /* function needs to allow for data to be an offset, else
     we can't control anything of value... */
  char *data;
  data = MMAP_DATA(mmap_obj);
#ifdef HAVE_MADVISE
  int ret = madvise(data, INTEGER(_len)[0], INTEGER(_flags)[0]);
#else
  int ret = -1;
#endif
  return ScalarInteger(ret);
}/*}}}*/

/* {{{ mmap_mprotect */
SEXP mmap_mprotect (SEXP mmap_obj, SEXP index, SEXP prot) {
  int i, LEN;
  size_t ival, upper_bound;
  char *data, *addr;

  data = MMAP_DATA(mmap_obj);
  LEN = length(index);

  SEXP ret; PROTECT(ret = allocVector(INTSXP, LEN));
  int pagesize = MMAP_PAGESIZE(mmap_obj);
  
  upper_bound = (MMAP_SIZE(mmap_obj)-sizeof(int));
  for(i=0;i<LEN;i++) {
    ival = (INTEGER(index)[i]-1)*sizeof(int);
    if( ival > upper_bound || ival < 0 )
      error("'i=%i' out of bounds", i);
    
/* Rprintf("offset: %i\n",(ival/pagesize)*pagesize); */
    addr = &(data[(int)((ival/pagesize)*pagesize)]);
    INTEGER(ret)[i] = mprotect(addr, ((ival/pagesize)*pagesize)*2, INTEGER(prot)[0]);
  }
  UNPROTECT(1);
  return ret;
}/*}}}*/

void logical_extract(unsigned char *data, SEXP dat, int LEN, int *index_p, int mmap_len, int record_size, int offset, SEXP smode, int swap) {
  int i, u;
  int *lgl_dat;
  int8_t bytebuf;
  int32_t intbuf;
  div_t word;
  
  lgl_dat = LOGICAL(dat);
  if(strcmp(SMODE_CTYPE(smode), "bitset") == 0) { /* bitset */
    for(i = 0; i < LEN; i++) {
      u = index_p[i] - 1;
      // Note that we can store 32 values in each word.
      if(u >= mmap_len * 32 || u < 0)
        error("'i=%i' out of bounds", u + 1);
      
      // word.quot == u / 32 && word.rem == u % 32.
      word = div(u, 32);
      memcpy(&intbuf, &data[word.quot * record_size + offset], sizeof(int32_t));
      // Endianness takes on a very convoluted meaning here. Still, this should
      //  be portable as long as we are consistent in using 32-bit chunks.
      if(swap)
        intbuf = swapb32(intbuf);
      lgl_dat[i] = !!(intbuf & bitmask[word.rem]);
    }
  } else {
    switch(SMODE_CBYTES(smode)) {
    case 1: /* bool8 */
      for(i = 0; i < LEN; i++) {
        u = index_p[i] - 1;
        if(u >= mmap_len || u < 0)
          error("'i=%i' out of bounds", u + 1);
        
        bytebuf = data[u * record_size + offset];
        if((uint8_t)bytebuf == NA_UINT8)
          lgl_dat[i] = NA_LOGICAL;
        else
          lgl_dat[i] = (uint8_t)bytebuf;
      }
      break;
    case 4: /* bool32 */
      for(i = 0; i < LEN; i++) {
        u = index_p[i] - 1;
        if(u >= mmap_len || u < 0)
          error("'i=%i' out of bounds", u + 1);
        
        memcpy(&intbuf, &data[u * record_size + offset], sizeof(int32_t));
        if(swap)
          intbuf = swapb32(intbuf);
        assert(NA_LOGICAL == NA_INT32);
        lgl_dat[i] = (int32_t)intbuf;
      }
      break;
    default:
      error("'logi' types must be either 8 or 32 bit");
      break;
    }
  }
}

void integer_extract(unsigned char *data, SEXP dat, int LEN, int *index_p, int mmap_len, int record_size, int offset, SEXP smode, int swap) {
  int i, u;
  int *int_dat;
  int8_t bytebuf;
  int16_t sbuf;
  int32_t intbuf;
  
  int_dat = INTEGER(dat);
  switch(SMODE_CBYTES(smode)) {
  case 1:
    if(SMODE_SIGNED(smode)) { /* int8 */
      for(i = 0; i < LEN; i++) {
        u = index_p[i] - 1;
        if(u >= mmap_len || u < 0)
          error("'i=%i' out of bounds", u + 1);
        
        bytebuf = data[u * record_size + offset];
        if((int8_t)bytebuf == NA_INT8)
          int_dat[i] = NA_INTEGER;
        else
          int_dat[i] = (int8_t)bytebuf;
      }
    } else { /* uint8 */
      for(i = 0; i < LEN; i++) {
        u = index_p[i] - 1;
        if(u >= mmap_len || u < 0)
          error("'i=%i' out of bounds", u + 1);
        
        bytebuf = data[u * record_size + offset];
        if((uint8_t)bytebuf == NA_UINT8)
          int_dat[i] = NA_INTEGER;
        else
          int_dat[i] = (uint8_t)bytebuf;
      }
    }
    break;
  case 2:
    if(SMODE_SIGNED(smode)) { /* int16 */
      for(i = 0; i < LEN; i++) {
        u = index_p[i] - 1;
        if(u >= mmap_len || u < 0)
          error("'i=%i' out of bounds", u + 1);
        
        memcpy(&sbuf, &data[u * record_size + offset], sizeof(int16_t));
        if(swap)
          sbuf = swapb16(sbuf);
        if((int16_t)sbuf == NA_INT16)
          int_dat[i] = NA_INTEGER;
        else
          int_dat[i] = (int16_t)sbuf;
      }
    } else { /* uint16 */
      for(i = 0; i < LEN; i++) {
        u = index_p[i] - 1;
        if(u >= mmap_len || u < 0)
          error("'i=%i' out of bounds", u + 1);
        
        memcpy(&sbuf, &data[u * record_size + offset], sizeof(uint16_t));
        if(swap)
          sbuf = swapb16(sbuf);
        if((uint16_t)sbuf == NA_UINT16)
          int_dat[i] = NA_INTEGER;
        else
          int_dat[i] = (uint16_t)sbuf;
      }
    }
    break;
  case 4: /* int32 */
    for(i = 0; i < LEN; i++) {
      u = index_p[i] - 1;
      if(u >= mmap_len || u < 0)
        error("'i=%i' out of bounds", u + 1);
      
      memcpy(&intbuf, &data[u * record_size + offset], sizeof(int32_t));
      if(swap)
        intbuf = swapb32(intbuf);
      assert(NA_INTEGER == NA_INT32);
      int_dat[i] = (int32_t)intbuf;
    }
    break;
  default:
    error("unknown data type");
    break;
  }
}

void double_extract(unsigned char *data, SEXP dat, int LEN, int *index_p, int mmap_len, int record_size, int offset, SEXP smode, int swap) {
  int i, u;
  double *real_dat;
  int64_t longbuf;
  float floatbuf;
  double realbuf;
  
  real_dat = REAL(dat);
  switch(SMODE_CBYTES(smode)) {
  case 4: /* real32 */
    for(i = 0; i < LEN; i++) {
      u = index_p[i] - 1;
      if(u >= mmap_len || u < 0)
        error("'i=%i' out of bounds", u + 1);
      
      memcpy(&floatbuf, &data[u * record_size + offset], sizeof(float));
      if(swap)
        floatbuf = swapb32(floatbuf);
      if(bitw_eq32(floatbuf, NA_FLOAT))
        real_dat[i] = NA_REAL;
      else
        real_dat[i] = (float)floatbuf;
    }
    break;
  case 8:
    if(strcmp(SMODE_CTYPE(smode), "int64") == 0) { /* int64 */
      /* casting from int64 to R double to minimize precision loss */
      for(i = 0; i < LEN; i++) {
        u = index_p[i] - 1;
        if(u >= mmap_len || u < 0)
          error("'i=%i' out of bounds", u + 1);
        
        memcpy(&longbuf, &data[u * record_size + offset], sizeof(int64_t));
        if(swap)
          longbuf = swapb64(longbuf);
        if((int64_t)longbuf == NA_INT64)
          real_dat[i] = NA_REAL;
        else
          real_dat[i] = (int64_t)longbuf;
      }
    } else { /* real64 */
      for(i = 0; i < LEN; i++) {
        u = index_p[i] - 1;
        if(u >= mmap_len || u < 0)
          error("'i=%i' out of bounds", u + 1);
        
        memcpy(&realbuf, &data[u * record_size + offset], sizeof(double));
        if(swap)
          realbuf = swapb64(realbuf);
        assert(bitw_eq64(NA_REAL, NA_DOUBLE));
        real_dat[i] = (double)realbuf;
      }
    }
    break;
  default:
    break;
  }
}

void complex_extract(unsigned char *data, SEXP dat, int LEN, int *index_p, int mmap_len, int record_size, int offset, int swap) {
  int i, u;
  Rcomplex *complex_dat;
  double doublepairbuf[2];
  Rcomplex Rcomplexbuf;
  
  complex_dat = COMPLEX(dat);
  for(i = 0; i < LEN; i++) {
    u = index_p[i] - 1;
    if(u >= mmap_len || u < 0)
      error("'i=%i' out of bounds", u + 1);
    
    // On all platforms that R compiles on, consecutive doubles in a struct
    //  should be aligned such that there is no padding inbetween. Still, it is
    //  more portable to memcpy first to an array which is guaranteed to store
    //  consecutive doubles in contiguous memory (no padding), and then copy the
    //  two doubles one-by-one to the struct. If R is ever indeed compiled on a
    //  16-byte-aligned platform, then `as.mmap.complex()` will have to be
    //  rewritten because `writeBin()` unsafely memcpys to and from the struct.
    // Note that the order of array/struct members must be preserved in memory,
    //  so it is only within each double does endianness play a part. That means
    //  that if swap is true, we have to rearrange (R8, ..., R1, I8, ... I1),
    //  NOT (I8, ..., I1, R8, ..., R1), to the order (R1, ..., R8, I1, ..., I8).
    memcpy(&doublepairbuf, &data[u * record_size + offset], 2 * sizeof(double));
    Rcomplexbuf.r = doublepairbuf[0];
    Rcomplexbuf.i = doublepairbuf[1];
    if(swap) {
      Rcomplexbuf.r = swapb64(Rcomplexbuf.r);
      Rcomplexbuf.i = swapb64(Rcomplexbuf.i);
    }
    complex_dat[i] = (Rcomplex)Rcomplexbuf;
  }
}

void character_extract(unsigned char *data, SEXP dat, int LEN, int *index_p, int mmap_len, int record_size, int offset, SEXP smode) {
  int i, u, fieldCbytes, hasnul;
  char *str;
  
  fieldCbytes = SMODE_CBYTES(smode);
  hasnul = !!SMODE_NUL_TERM(smode);
  if(hasnul) {
    for(i = 0; i < LEN; i++) {
      u = index_p[i] - 1;
      if(u >= mmap_len || u < 0)
        error("'i=%i' out of bounds", u + 1);
      
      str = (char *)&data[u * record_size + offset];
      if(str[0] == 0 && str[1] != 0)
        SET_STRING_ELT(dat, i, NA_STRING);
      else
        SET_STRING_ELT(dat, i, mkChar((const char *)str));
    }
  } else {  /* nul-padded char array */
    for(i = 0; i < LEN; i++) {
      u = index_p[i] - 1;
      if(u >= mmap_len || u < 0)
        error("'i=%i' out of bounds", u + 1);
      
      str = (char *)&data[u * record_size + offset];
      if(str[0] == 0 && str[1] != 0)
        SET_STRING_ELT(dat, i, NA_STRING);
      else
        SET_STRING_ELT(dat, i, mkCharLen((const char *)str, strnlen(str, fieldCbytes)));
    }
  }
}

void raw_extract(unsigned char *data, SEXP dat, int LEN, int *index_p, int mmap_len, int record_size, int offset) {
  int i, u;
  Rbyte *raw_dat;

  raw_dat = RAW(dat);
  for(i = 0; i < LEN; i++) {
    u = index_p[i] - 1;
    if(u >= mmap_len || u < 0)
      error("'i=%i' out of bounds", u + 1);
    
    raw_dat[i] = (unsigned char)data[u * record_size + offset];
  }
}

/* {{{ mmap_extract */
SEXP mmap_extract (SEXP index, SEXP field, SEXP dim, SEXP mmap_obj, SEXP swap_byte_order) {
/*SEXP mmap_extract (SEXP index, SEXP field, SEXP mmap_obj) {*/
  long v, fi;
  int P = 0;
  unsigned char *data; /* unsigned int and values */

  PROTECT(index = coerceVector(index, INTSXP)); P++;
  PROTECT(field = coerceVector(field, INTSXP)); P++;
  SEXP vec_smode, smode = MMAP_SMODE(mmap_obj);
  int LEN = length(index);
  int mode = TYPEOF(smode);
  int Cbytes = SMODE_CBYTES(smode);
  int mmap_len = MMAP_SIZE(mmap_obj) / Cbytes; /* length.mmap() */

  data = MMAP_DATA(mmap_obj);
  if(data == NULL)
    error("invalid mmap pointer");

  SEXP dat; /* dat is either a column, or list of columns */
  if(mode == VECSXP)
    PROTECT(dat = allocVector(VECSXP, length(field)));
  else
    PROTECT(dat = allocVector(mode, LEN));
  P++;

  int *index_p = INTEGER(index);

  int offset;
  SEXP vec_dat;

  switch(mode) {
  case LGLSXP: /* {{{ */
    logical_extract(data, dat, LEN, index_p, mmap_len, Cbytes, 0, smode, asLogical(swap_byte_order));
    break; /* }}} */
  case INTSXP: /* {{{ */
    integer_extract(data, dat, LEN, index_p, mmap_len, Cbytes, 0, smode, asLogical(swap_byte_order));
    break; /* }}} */
  case REALSXP: /* {{{ */
    double_extract(data, dat, LEN, index_p, mmap_len, Cbytes, 0, smode, asLogical(swap_byte_order));
    break; /* }}} */
  case CPLXSXP: /* {{{ */
    complex_extract(data, dat, LEN, index_p, mmap_len, Cbytes, 0, asLogical(swap_byte_order));
    break; /* }}} */
  case STRSXP: /* {{{ */
    character_extract(data, dat, LEN, index_p, mmap_len, Cbytes, 0, smode);
    break; /* }}} */
  case RAWSXP: /* {{{ */
    raw_extract(data, dat, LEN, index_p, mmap_len, Cbytes, 0);
    break; /* }}} */
  case VECSXP:  /* corresponds to C struct for mmap package {{{ */
    for(fi = 0; fi < length(field); fi++) {
      v = INTEGER(field)[fi] - 1;
      offset = SMODE_OFFSET(smode, v);
      vec_smode = VECTOR_ELT(smode, v);
      switch(TYPEOF(vec_smode)) {
      case LGLSXP:
        PROTECT(vec_dat = allocVector(LGLSXP, LEN));
        logical_extract(data, vec_dat, LEN, index_p, mmap_len, Cbytes, offset, vec_smode, LOGICAL(swap_byte_order)[fi]);
        SET_VECTOR_ELT(dat, fi, vec_dat);
        UNPROTECT(1);
        break;
      case INTSXP:
        PROTECT(vec_dat = allocVector(INTSXP, LEN));
        integer_extract(data, vec_dat, LEN, index_p, mmap_len, Cbytes, offset, vec_smode, LOGICAL(swap_byte_order)[fi]);
        SET_VECTOR_ELT(dat, fi, vec_dat);
        UNPROTECT(1);
        break;
      case REALSXP:
        PROTECT(vec_dat = allocVector(REALSXP, LEN));
        double_extract(data, vec_dat, LEN, index_p, mmap_len, Cbytes, offset, vec_smode, LOGICAL(swap_byte_order)[fi]);
        SET_VECTOR_ELT(dat, fi, vec_dat);
        UNPROTECT(1);
        break;
      case CPLXSXP:
        PROTECT(vec_dat = allocVector(CPLXSXP, LEN));
        complex_extract(data, vec_dat, LEN, index_p, mmap_len, Cbytes, offset, LOGICAL(swap_byte_order)[fi]);
        SET_VECTOR_ELT(dat, fi, vec_dat);
        UNPROTECT(1);
        break;
      case STRSXP:
        PROTECT(vec_dat = allocVector(STRSXP, LEN));
        character_extract(data, vec_dat, LEN, index_p, mmap_len, Cbytes, offset, vec_smode);
        SET_VECTOR_ELT(dat, fi, vec_dat);
        UNPROTECT(1);
        break;
      case RAWSXP:
        PROTECT(vec_dat = allocVector(RAWSXP, LEN));
        raw_extract(data, vec_dat, LEN, index_p, mmap_len, Cbytes, offset);
        SET_VECTOR_ELT(dat, fi, vec_dat);
        UNPROTECT(1);
        break;
      default:
        error("unimplemented type");
        break;
      }
    } /* }}} */
    break;
  default:
    error("unsupported type");
    break;
  }
  if(!isNull(dim))
    setAttrib(dat, R_DimSymbol, dim);
  UNPROTECT(P);
  return dat;
}/*}}}*/

void logical_replace(unsigned char *data, SEXP value, int LEN, int *index_p, int mmap_len, int record_size, int offset, SEXP smode, int swap) {
  int i, u;
  int *lgl_value;
  int32_t intbuf;
  div_t word;
  
  lgl_value = LOGICAL(value);
  if(strcmp(SMODE_CTYPE(smode), "bitset") == 0) {  /* bitset */
    for(i = 0; i < LEN; i++) {
      u = index_p[i] - 1;
      // Note that we can store 32 values in each word.
      if(u >= mmap_len * 32 || u < 0)
        error("'i=%i' out of bounds", u + 1);
      
      // word.quot == u / 32 && word.rem == u % 32.
      word = div(u, 32);
      memcpy(&intbuf, &data[word.quot * record_size + offset], sizeof(int32_t));
      // Endianness takes on a very convoluted meaning here. Still, this should
      //  be portable as long as we are consistent in using 32-bit chunks.
      if(swap)
        intbuf = swapb32(intbuf);
      if(lgl_value[i] == NA_LOGICAL) {
        warning("NA values treated as FALSE in coercion to bitset");
        intbuf = intbuf & nbitmask[word.rem];
      } else if(lgl_value[i]) {
        intbuf = intbuf | bitmask[word.rem];
      } else {
        intbuf = intbuf & nbitmask[word.rem];
      }
      if(swap)
        intbuf = swapb32(intbuf);
      memcpy(&data[word.quot * record_size + offset], &intbuf, sizeof(int32_t));
    }
  } else {
    switch(SMODE_CBYTES(smode)) {
    case 1: /* bool8 */
      for(i = 0; i < LEN; i++) {
        u = index_p[i] - 1;
        if(u >= mmap_len || u < 0)
          error("'i=%i' out of bounds", u + 1);
        
        // R doesn't normalize logicals when assigned a value other than 1
        //  through C code. Any non-zero values will display as TRUE. The
        //  behavior is evident when calling `as.integer(x)` or `x == TRUE`
        //  where `x` is returned by the following C code:
        //    SEXP x;
        //    PROTECT(x = allocVector(LGLSXP, 1));
        //    LOGICAL(x)[0] = 2;
        //    UNPROTECT(1);
        //    return x;
        // There may actually be precision loss here if such behavior is
        //  exploited intentionally, but most likely the user would not notice
        //  so there is no need to give a warning. However, we do need to ensure
        //  that non-zero values are never truncated to zero and that non-NA
        //  values are never truncated to NA.
        if(lgl_value[i] == NA_LOGICAL)
          data[u * record_size + offset] = NA_UINT8;
        else if((uint8_t)lgl_value[i] == NA_UINT8
                  || ((uint8_t)lgl_value[i] == 0 && lgl_value[i] != 0))
          data[u * record_size + offset] = 1;
        else
          data[u * record_size + offset] = (uint8_t)lgl_value[i];
      }
      break;
    case 4: /* bool32 */
      for(i = 0; i < LEN; i++) {
        u = index_p[i] - 1;
        if(u >= mmap_len || u < 0)
          error("'i=%i' out of bounds", u + 1);
        
        assert(NA_LOGICAL == NA_INT32);
        intbuf = (int32_t)lgl_value[i];
        if(swap)
          intbuf = swapb32(intbuf);
        memcpy(&data[u * record_size + offset], &intbuf, sizeof(int32_t));
      }
      break;
    default:
      error("'logi' types must either 8 or 32 bit");
      break;
    }
  }
}

void integer_replace(unsigned char *data, SEXP value, int LEN, int *index_p, int mmap_len, int record_size, int offset, SEXP smode, int swap) {
  int i, u;
  int *int_value;
  int16_t sbuf;
  int32_t intbuf;
  
  int_value = INTEGER(value);
  switch(SMODE_CBYTES(smode)) {
  case 1:
    if(SMODE_SIGNED(smode)) { /* int8 */
      for(i = 0; i < LEN; i++) {
        u = index_p[i] - 1;
        if(u >= mmap_len || u < 0)
          error("'i=%i' out of bounds", u + 1);
        
        // Give a warning and store NA if there is precision loss or if the
        //  integer value unluckily matches the representation of NA in int8.
        if(int_value[i] == NA_INTEGER) {
          data[u * record_size + offset] = NA_INT8;
        } else if((int8_t)int_value[i] == NA_INT8
                    || (int)(int8_t)int_value[i] != int_value[i]) {
          warning("NAs introduced by coercion to int8 range");
          data[u * record_size + offset] = NA_INT8;
        } else {
          data[u * record_size + offset] = (int8_t)int_value[i];
        }
      }
    } else { /* uint8 */
      for(i = 0; i < LEN; i++) {
        u = index_p[i] - 1;
        if(u >= mmap_len || u < 0)
          error("'i=%i' out of bounds", u + 1);
        
        // Give a warning and store NA if there is precision loss or if the
        //  integer value unluckily matches the representation of NA in uint8.
        if(int_value[i] == NA_INTEGER) {
          data[u * record_size + offset] = NA_UINT8;
        } else if((uint8_t)int_value[i] == NA_UINT8
                    || (int)(uint8_t)int_value[i] != int_value[i]) {
          warning("NAs introduced by coercion to uint8 range");
          data[u * record_size + offset] = NA_UINT8;
        } else {
          data[u * record_size + offset] = (uint8_t)int_value[i];
        }
      }
    }
    break;
  case 2:
    if(SMODE_SIGNED(smode)) { /* int16 */
      for(i = 0; i < LEN; i++) {
        u = index_p[i] - 1;
        if(u >= mmap_len || u < 0)
          error("'i=%i' out of bounds", u + 1);
        
        // Give a warning and store NA if there is precision loss or if the
        //  integer value unluckily matches the representation of NA in int16.
        if(int_value[i] == NA_INTEGER) {
          sbuf = NA_INT16;
        } else if((int16_t)int_value[i] == NA_INT16
                    || (int)(int16_t)int_value[i] != int_value[i]) {
          warning("NAs introduced by coercion to int16 range");
          sbuf = NA_INT16;
        } else {
          sbuf = (int16_t)int_value[i];
        }
        if(swap)
          sbuf = swapb16(sbuf);
        memcpy(&data[u * record_size + offset], &sbuf, sizeof(int16_t));
      }
    } else { /* uint16 */
      for(i = 0; i < LEN; i++) {
        u = index_p[i] - 1;
        if(u >= mmap_len || u < 0)
          error("'i=%i' out of bounds", u + 1);
        
        // Give a warning and store NA if there is precision loss or if the
        //  integer value unluckily matches the representation of NA in uint16.
        if(int_value[i] == NA_INTEGER) {
          sbuf = NA_UINT16;
        } else if((uint16_t)int_value[i] == NA_UINT16
                    || (int)(uint16_t)int_value[i] != int_value[i]) {
          warning("NAs introduced by coercion to uint16 range");
          sbuf = NA_UINT16;
        } else {
          sbuf = (uint16_t)int_value[i];
        }
        if(swap)
          sbuf = swapb16(sbuf);
        memcpy(&data[u * record_size + offset], &sbuf, sizeof(uint16_t));
      }
    }
    break;
  case 4: /* int32 */
    for(i = 0; i < LEN; i++) {
      u = index_p[i] - 1;
      if(u >= mmap_len || u < 0)
        error("'i=%i' out of bounds", u + 1);
      
      assert(NA_INTEGER == NA_INT32);
      intbuf = (int32_t)int_value[i];
      if(swap)
        intbuf = swapb32(intbuf);
      memcpy(&data[u * record_size + offset], &intbuf, sizeof(int32_t));
    }
    break;
  default:
    error("unknown data type");
    break;
  }
}

void double_replace(unsigned char *data, SEXP value, int LEN, int *index_p, int mmap_len, int record_size, int offset, SEXP smode, int swap) {
  int i, u;
  double *real_value;
  int64_t longbuf;
  float floatbuf;
  double realbuf;
  
  real_value = REAL(value);
  switch(SMODE_CBYTES(smode)) {
  case 4: /* real32 */
    for(i = 0; i < LEN; i++) {
      u = index_p[i] - 1;
      if(u >= mmap_len || u < 0)
        error("'i=%i' out of bounds", u + 1);
      
      // As opposed to the integer case, there is no need to substitute NA_FLOAT
      //  and give a warning when there is precision loss.
      if(ISNA(real_value[i]))
        floatbuf = NA_FLOAT;
      else
        floatbuf = (float)real_value[i];
      if(swap)
        floatbuf = swapb32(floatbuf);
      memcpy(&data[u * record_size + offset], &floatbuf, sizeof(float));
    }
    break;
  case 8:
    if(strcmp(SMODE_CTYPE(smode), "int64") == 0) { /* int64 */
      for(i = 0; i < LEN; i++) {
        u = index_p[i] - 1;
        if(u >= mmap_len || u < 0)
          error("'i=%i' out of bounds", u + 1);
        
        // `2^63` is perfectly representable in IEEE 754 since 63 is well within
        //  the normalized exponent range of [-(2^(11-1)-1), 2^(11-1)-1] and the
        //  significand is exactly 1. Therefore, the floating point value can
        //  encode `NA_INT64` as a regular number. The floating point value can
        //  also encode numbers of either sign with normalized exponents as high
        //  as 2047, so we can also lose precision with these large numbers.
        // `INT64_MIN == -2^63` so it can be perfectly represented as a double.
        //  If there weren't the complication with `NA_INT64`, there would be no
        //  loss of precision when `real_value[i] == (double)INT64_MIN`.
        // However, `INT64_MAX == 2^63-1` so it rounds up to `2^63` (the nearest
        //  representable number) when casted to double. Therefore, there is
        //  still a loss of precision when `real_value[i] == (double)INT64_MAX`.
        if(ISNA(real_value[i])) {
          longbuf = NA_INT64;
        } else if(real_value[i] == (double)NA_INT64
                    || real_value[i] < (double)INT64_MIN
                    || real_value[i] >= (double)INT64_MAX) {
          warning("NAs introduced by coercion to int64 range");
          longbuf = NA_INT64;
        } else {
          longbuf = (int64_t)real_value[i];
        }
        if(swap)
          longbuf = swapb64(longbuf);
        memcpy(&data[u * record_size + offset], &longbuf, sizeof(int64_t));
      }
    } else { /* real64 */
      for(i = 0; i < LEN; i++) {
        u = index_p[i] - 1;
        if(u >= mmap_len || u < 0)
          error("'i=%i' out of bounds", u + 1);
        
        assert(bitw_eq64(NA_REAL, NA_DOUBLE));
        realbuf = (double)real_value[i];
        if(swap)
          realbuf = swapb64(realbuf);
        memcpy(&data[u * record_size + offset], &realbuf, sizeof(double));
      }
    }
    break;
  }
}

void complex_replace(unsigned char *data, SEXP value, int LEN, int *index_p, int mmap_len, int record_size, int offset, int swap) {
  int i, u;
  Rcomplex *complex_value;
  double doublepairbuf[2];
  Rcomplex Rcomplexbuf;
  
  complex_value = COMPLEX(value);
  for(i = 0; i < LEN; i++) {
    u = index_p[i] - 1;
    if(u >= mmap_len || u < 0)
      error("'i=%i' out of bounds", u + 1);
    
    Rcomplexbuf = complex_value[i];
    if(swap) {
      Rcomplexbuf.r = swapb64(Rcomplexbuf.r);
      Rcomplexbuf.i = swapb64(Rcomplexbuf.i);
    }
    doublepairbuf[0] = Rcomplexbuf.r;
    doublepairbuf[1] = Rcomplexbuf.i;
    memcpy(&data[u * record_size + offset], &doublepairbuf, 2 * sizeof(double));
  }
}

void character_replace(unsigned char *data, SEXP value, int LEN, int *index_p, int mmap_len, int record_size, int offset, SEXP smode) {
  int i, u, fieldCbytes, hasnul, charsxp_len;
  
  fieldCbytes = SMODE_CBYTES(smode);
  hasnul = !!SMODE_NUL_TERM(smode);
  for(i = 0; i < LEN; i++) {
    u = index_p[i] - 1;
    if(u >= mmap_len || u < 0)
      error("'i=%i' out of bounds", u + 1);
    
    memset(&data[u * record_size + offset], '\0', fieldCbytes);
    // strnlen(CHAR(STRING_ELT(value, i)), fieldCbytes) is definitely O(n).
    // I'm hoping that R internally stores the length of strings
    //  so that length(CHARSXP) is O(1).
    charsxp_len = length(STRING_ELT(value,i));
    if(STRING_ELT(value, i) == NA_STRING) {
      // Strings that start with { 0x00, 0xff } represent NA.
      data[u * record_size + offset + 1] = 0xff;
    } else if(charsxp_len > fieldCbytes - hasnul) {
      warning("Long strings were truncated");
      memcpy(&data[u * record_size + offset], CHAR(STRING_ELT(value,i)), fieldCbytes - hasnul);
    } else {
      memcpy(&data[u * record_size + offset], CHAR(STRING_ELT(value,i)), charsxp_len);
    }
  }
}

void raw_replace(unsigned char *data, SEXP value, int LEN, int *index_p, int mmap_len, int record_size, int offset) {
  int i, u;
  Rbyte *raw_value;
  
  raw_value = RAW(value);
  for(i = 0; i < LEN; i++) {
    u = index_p[i] - 1;
    if(u >= mmap_len || u < 0)
      error("'i=%i' out of bounds", u + 1);
    
    data[u * record_size + offset] = (Rbyte)raw_value[i];
  }
}

/* mmap_replace {{{ */
SEXP mmap_replace (SEXP index, SEXP field, SEXP value, SEXP mmap_obj, SEXP swap_byte_order) {
  int v, fi, offset;
  unsigned char *data;
  int LEN = length(index);
  SEXP vec_smode, smode = MMAP_SMODE(mmap_obj);
  int mode = TYPEOF(smode);
  int Cbytes = SMODE_CBYTES(smode);
  int mmap_len = MMAP_SIZE(mmap_obj) / Cbytes; /* length.mmap() */
  int P = 0;
  SEXP vec_value;
  
  data = MMAP_DATA(mmap_obj);
  if(data == NULL)
    error("invalid mmap pointer");

  if(mode != VECSXP) {
    PROTECT(value = coerceVector(value, mode)); P++;
    if (length(value) != LEN)
      // Code on R side failed to properly handle the recycling.
      error("size of struct and size of replacement value do not match");
  }
  PROTECT(index = coerceVector(index, INTSXP)); P++;
  PROTECT(field = coerceVector(field, INTSXP)); P++;
  int *index_p = INTEGER(index);
  switch(mode) {
  case LGLSXP:
    logical_replace(data, value, LEN, index_p, mmap_len, Cbytes, 0, smode, asLogical(swap_byte_order));
    break;
  case INTSXP: /* {{{ */
    integer_replace(data, value, LEN, index_p, mmap_len, Cbytes, 0, smode, asLogical(swap_byte_order));
    break; /* }}} */
  case REALSXP: /* {{{ */
    double_replace(data, value, LEN, index_p, mmap_len, Cbytes, 0, smode, asLogical(swap_byte_order));
    break; /* }}} */
  case CPLXSXP:
    complex_replace(data, value, LEN, index_p, mmap_len, Cbytes, 0, asLogical(swap_byte_order));
    break;
  case STRSXP:
    character_replace(data, value, LEN, index_p, mmap_len, Cbytes, 0, smode);
    break;
  case RAWSXP:
    raw_replace(data, value, LEN, index_p, mmap_len, Cbytes, 0);
    break;
  case VECSXP: /* aka "struct"{{{ */
    if(length(value) != length(field))
      // Code on R side failed to properly handle the recycling.
      error("size of struct and size of replacement value do not match");
    for(fi = 0; fi < length(field); fi++) {
      v = INTEGER(field)[fi] - 1;
      offset = SMODE_OFFSET(smode, v);
      vec_smode = VECTOR_ELT(smode, v);
      vec_value = VECTOR_ELT(value, fi);
      if (length(vec_value) != LEN)
        // Code on R side failed to properly handle the recycling.
        error("size of struct and size of replacement value do not match");
      switch(TYPEOF(vec_smode)) {
      case LGLSXP:
        PROTECT(vec_value = coerceVector(vec_value, LGLSXP));
        logical_replace(data, vec_value, LEN, index_p, mmap_len, Cbytes, offset, vec_smode, LOGICAL(swap_byte_order)[fi]);
        UNPROTECT(1);
        break;
      case INTSXP:
        PROTECT(vec_value = coerceVector(vec_value, INTSXP));
        integer_replace(data, vec_value, LEN, index_p, mmap_len, Cbytes, offset, vec_smode, LOGICAL(swap_byte_order)[fi]);
        UNPROTECT(1);
        break;
      case REALSXP:
        PROTECT(vec_value = coerceVector(vec_value, REALSXP));
        double_replace(data, vec_value, LEN, index_p, mmap_len, Cbytes, offset, vec_smode, LOGICAL(swap_byte_order)[fi]);
        UNPROTECT(1);
        break;
      case CPLXSXP:
        PROTECT(vec_value = coerceVector(vec_value, CPLXSXP));
        complex_replace(data, vec_value, LEN, index_p, mmap_len, Cbytes, offset, LOGICAL(swap_byte_order)[fi]);
        UNPROTECT(1);
        break;
      case STRSXP:
        PROTECT(vec_value = coerceVector(vec_value, STRSXP));
        character_replace(data, vec_value, LEN, index_p, mmap_len, Cbytes, offset, vec_smode);
        UNPROTECT(1);
        break;
      case RAWSXP:
        PROTECT(vec_value = coerceVector(vec_value, RAWSXP));
        raw_replace(data, vec_value, LEN, index_p, mmap_len, Cbytes, offset);
        UNPROTECT(1);
        break;
      default:
        error("unimplemented replacement type");
        break;
      }
    } /* VECSXP }}} */
    break;
  default:
    error("unsupported type");
    break;
  }
  UNPROTECT(P);
  return R_NilValue;
} /*}}}*/

/* {{{ mmap_compare */
SEXP mmap_compare (SEXP compare_to, SEXP compare_how, SEXP mmap_obj) {
  int i;
  char *data;

  unsigned char charbuf;
  int intbuf;
  short shortbuf;
  float floatbuf;
  double realbuf;

  long LEN;
  int mode = TYPEOF(MMAP_SMODE(mmap_obj)); 
  int Cbytes = SMODE_CBYTES(MMAP_SMODE(mmap_obj));
  int isSigned = SMODE_SIGNED(MMAP_SMODE(mmap_obj));

  SEXP result;
  LEN = (long)(MMAP_SIZE(mmap_obj)/Cbytes);  /* change to REAL */
  PROTECT(result = allocVector(INTSXP, LEN));
  int *int_result = INTEGER(result);

  /* comp_how
     1  ==
     2  !=
     3  >=
     4  <=
     5  >
     6  < */
  int cmp_how = INTEGER(compare_how)[0];

  //int *int_dat;
  //double *real_dat;
  //char *cmp_str;
  int cmp_len;
  int hits=0;
  int cmp_to_int;
  double cmp_to_real;
  unsigned char * cmp_to_raw;

  data = MMAP_DATA(mmap_obj);
  if(data == NULL)
    error("invalid mmap pointer");

  switch(mode) {
  case LGLSXP:
    cmp_to_int = INTEGER(coerceVector(compare_to,INTSXP))[0];
    /* bitset, bool8, bool32 */
    switch(Cbytes) {
      case sizeof(char):
        if(cmp_how==1) {
          for(i=0;  i < LEN; i++) {
            memcpy(&charbuf, &(data[i * sizeof(char)]),sizeof(char));
            if(cmp_to_int == (int)charbuf)
              int_result[hits++] = i+1;
          }
        } else
        if(cmp_how==2) {
          for(i=0;  i < LEN; i++) {
            memcpy(&charbuf, &(data[i * sizeof(char)]), sizeof(char));
            if(cmp_to_int != (int)charbuf)
              int_result[hits++] = i+1;
          }
        } else
        if(cmp_how==3) {
          for(i=0;  i < LEN; i++) {
            memcpy(&charbuf, &(data[i * sizeof(char)]), sizeof(char));
            if(cmp_to_int <= (int)charbuf)
              int_result[hits++] = i+1;
          }
        } else
        if(cmp_how==4) {
          for(i=0;  i < LEN; i++) {
            memcpy(&charbuf, &(data[i * sizeof(char)]), sizeof(char));
            if(cmp_to_int >= (int)charbuf)
              int_result[hits++] = i+1;
          }
        } else
        if(cmp_how==5) {
          for(i=0;  i < LEN; i++) {
            memcpy(&charbuf, &(data[i * sizeof(char)]), sizeof(char));
            if(cmp_to_int <  (int)charbuf)
              int_result[hits++] = i+1;
          }
        } else
        if(cmp_how==6) {
          for(i=0;  i < LEN; i++) {
            memcpy(&charbuf, &(data[i * sizeof(char)]), sizeof(char));
            if(cmp_to_int >  (int)charbuf)
              int_result[hits++] = i+1;
          }
        }
        break;
      case sizeof(int):
        if(cmp_how==1) {
          for(i=0;  i < LEN; i++) {
            memcpy(&intbuf, &(data[i * sizeof(int)]), sizeof(char) * sizeof(int));
            if(cmp_to_int == intbuf) 
              int_result[hits++] = i+1;
          }
        } else
        if(cmp_how==2) {
          for(i=0;  i < LEN; i++) {
            memcpy(&intbuf, &(data[i * sizeof(int)]), sizeof(char) * sizeof(int));
            if(cmp_to_int != intbuf) 
              int_result[hits++] = i+1;
          }
        } else
        if(cmp_how==3) {
          for(i=0;  i < LEN; i++) {
            memcpy(&intbuf, &(data[i * sizeof(int)]), sizeof(char) * sizeof(int));
            if(cmp_to_int <= intbuf)
              int_result[hits++] = i+1;
          }
        } else
        if(cmp_how==4) {
          for(i=0;  i < LEN; i++) {
            memcpy(&intbuf, &(data[i * sizeof(int)]), sizeof(char) * sizeof(int));
            if(cmp_to_int >= intbuf)
              int_result[hits++] = i+1;
          }
        } else
        if(cmp_how==5) {
          for(i=0;  i < LEN; i++) {
            memcpy(&intbuf, &(data[i * sizeof(int)]), sizeof(char) * sizeof(int));
            if(cmp_to_int <  intbuf)
              int_result[hits++] = i+1;
          }
        } else
        if(cmp_how==6) {
          for(i=0;  i < LEN; i++) {
            memcpy(&intbuf, &(data[i * sizeof(int)]), sizeof(char) * sizeof(int));
            if(cmp_to_int >  intbuf)
              int_result[hits++] = i+1;
          }
        }
        break;
      default:
        error("unimplemented type in comparison");
        break;
    }
    break;
  case INTSXP:
    cmp_to_int = INTEGER(coerceVector(compare_to,INTSXP))[0];
    /* needs to branch for
        uint8, int8, uint16, int16, uint24, int24, int32 
        FIXME int64 (in REALSXP branch)
    */
    switch(Cbytes) {
    case 1: /* char int {{{ */
      if(isSigned) {
      if(cmp_how==1) {
        for(i=0;  i < LEN; i++) {
          memcpy(&charbuf, &(data[i * sizeof(char)]),sizeof(char));
          if(cmp_to_int == (int)charbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==2) {
        for(i=0;  i < LEN; i++) {
          memcpy(&charbuf, &(data[i * sizeof(char)]), sizeof(char));
          if(cmp_to_int != (int)charbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==3) {
        for(i=0;  i < LEN; i++) {
          memcpy(&charbuf, &(data[i * sizeof(char)]), sizeof(char));
          if(cmp_to_int <= (int)charbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==4) {
        for(i=0;  i < LEN; i++) {
          memcpy(&charbuf, &(data[i * sizeof(char)]), sizeof(char));
          if(cmp_to_int >= (int)charbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==5) {
        for(i=0;  i < LEN; i++) {
          memcpy(&charbuf, &(data[i * sizeof(char)]), sizeof(char));
          if(cmp_to_int <  (int)charbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==6) {
        for(i=0;  i < LEN; i++) {
          memcpy(&charbuf, &(data[i * sizeof(char)]), sizeof(char));
          if(cmp_to_int >  (int)charbuf)
            int_result[hits++] = i+1;
        }
      }
      } else { /* unsigned char */
      if(cmp_how==1) {
        for(i=0;  i < LEN; i++) {
          memcpy(&charbuf, &(data[i * sizeof(char)]),sizeof(char));
          if(cmp_to_int == (int)charbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==2) {
        for(i=0;  i < LEN; i++) {
          memcpy(&charbuf, &(data[i * sizeof(char)]), sizeof(char));
          if(cmp_to_int != (int)charbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==3) {
        for(i=0;  i < LEN; i++) {
          memcpy(&charbuf, &(data[i * sizeof(char)]), sizeof(char));
          if(cmp_to_int <= (int)charbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==4) {
        for(i=0;  i < LEN; i++) {
          memcpy(&charbuf, &(data[i * sizeof(char)]), sizeof(char));
          if(cmp_to_int >= (int)charbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==5) {
        for(i=0;  i < LEN; i++) {
          memcpy(&charbuf, &(data[i * sizeof(char)]), sizeof(char));
          if(cmp_to_int <  (int)charbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==6) {
        for(i=0;  i < LEN; i++) {
          memcpy(&charbuf, &(data[i * sizeof(char)]), sizeof(char));
          if(cmp_to_int >  (int)charbuf)
            int_result[hits++] = i+1;
        }
      } /* end of uchar */
      }
      break; /* }}} */
    case 2: /* short int {{{ */
      if(isSigned) {
      if(cmp_how==1) {
        cmp_to_int = (short)cmp_to_int;
        for(i=0;  i < LEN; i++) {
          memcpy(&shortbuf, &(data[i * sizeof(short)]),sizeof(short));
          //if(cmp_to_int == (int)(short)*(short *)(short_buf)) 
          if(cmp_to_int == shortbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==2) {
        for(i=0;  i < LEN; i++) {
          memcpy(&shortbuf, &(data[i * sizeof(short)]), sizeof(short));
          if(cmp_to_int != shortbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==3) {
        for(i=0;  i < LEN; i++) {
          memcpy(&shortbuf, &(data[i * sizeof(short)]), sizeof(short));
          if(cmp_to_int <= shortbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==4) {
        for(i=0;  i < LEN; i++) {
          memcpy(&shortbuf, &(data[i * sizeof(short)]), sizeof(short));
          if(cmp_to_int >= shortbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==5) {
        for(i=0;  i < LEN; i++) {
          memcpy(&shortbuf, &(data[i * sizeof(short)]), sizeof(short));
          if(cmp_to_int <  shortbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==6) {
        for(i=0;  i < LEN; i++) {
          memcpy(&shortbuf, &(data[i * sizeof(short)]), sizeof(short));
          if(cmp_to_int >  shortbuf)
            int_result[hits++] = i+1;
        }
      }
      } else { /* unsigned short */
      if(cmp_how==1) {
        for(i=0;  i < LEN; i++) {
          memcpy(&shortbuf, &(data[i * sizeof(short)]),sizeof(short));
          if(cmp_to_int == (unsigned short)shortbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==2) {
        for(i=0;  i < LEN; i++) {
          memcpy(&shortbuf, &(data[i * sizeof(short)]), sizeof(short));
          if(cmp_to_int != (unsigned short)shortbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==3) {
        for(i=0;  i < LEN; i++) {
          memcpy(&shortbuf, &(data[i * sizeof(short)]), sizeof(short));
          if(cmp_to_int <= (unsigned short)shortbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==4) {
        for(i=0;  i < LEN; i++) {
          memcpy(&shortbuf, &(data[i * sizeof(short)]), sizeof(short));
          if(cmp_to_int >= (unsigned short)shortbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==5) {
        for(i=0;  i < LEN; i++) {
          memcpy(&shortbuf, &(data[i * sizeof(short)]), sizeof(short));
          if(cmp_to_int <  (unsigned short)shortbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==6) {
        for(i=0;  i < LEN; i++) {
          memcpy(&shortbuf, &(data[i * sizeof(short)]), sizeof(short));
          if(cmp_to_int >  (unsigned short)shortbuf)
            int_result[hits++] = i+1;
        }
      } /* end of ushort */
      }
      break; /* }}} */
    case 4: /* int {{{ */
      if(cmp_how==1) {
        for(i=0;  i < LEN; i++) {
          memcpy(&intbuf, &(data[i * sizeof(int)]), sizeof(char) * sizeof(int));
          //if(cmp_to_int == *((int *)(void *)&int_buf)) 
          if(cmp_to_int == intbuf) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==2) {
        for(i=0;  i < LEN; i++) {
          memcpy(&intbuf, &(data[i * sizeof(int)]), sizeof(char) * sizeof(int));
          if(cmp_to_int != intbuf) //*((int *)(void *)&int_buf)) 
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==3) {
        for(i=0;  i < LEN; i++) {
          memcpy(&intbuf, &(data[i * sizeof(int)]), sizeof(char) * sizeof(int));
          if(cmp_to_int <= intbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==4) {
        for(i=0;  i < LEN; i++) {
          memcpy(&intbuf, &(data[i * sizeof(int)]), sizeof(char) * sizeof(int));
          if(cmp_to_int >= intbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==5) {
        for(i=0;  i < LEN; i++) {
          memcpy(&intbuf, &(data[i * sizeof(int)]), sizeof(char) * sizeof(int));
          if(cmp_to_int <  intbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==6) {
        for(i=0;  i < LEN; i++) {
          memcpy(&intbuf, &(data[i * sizeof(int)]), sizeof(char) * sizeof(int));
          if(cmp_to_int >  intbuf)
            int_result[hits++] = i+1;
        }
      }
      break;
    default: /* }}} */
      error("unsupported integer type");
    }
    break;
  case REALSXP:
    /* NA handling is missing .. how is this to behave? 
       Likely should test for compare_to as well as on-disk
       values.
    */
    cmp_to_real = REAL(coerceVector(compare_to,REALSXP))[0];
    switch(Cbytes) {
    case sizeof(float):
      if(cmp_how==1) {
        for(i=0;  i < LEN; i++) {
          memcpy(&floatbuf, &(data[i * sizeof(float)]), sizeof(float));
          if(cmp_to_real == (double)floatbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==2) {
        for(i=0;  i < LEN; i++) {
          memcpy(&floatbuf, &(data[i * sizeof(float)]), sizeof(float));
          if(cmp_to_real != (double)floatbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==3) {
        for(i=0;  i < LEN; i++) {
          memcpy(&floatbuf, &(data[i * sizeof(float)]), sizeof(float));
          if(cmp_to_real <= (double)floatbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==4) {
        for(i=0;  i < LEN; i++) {
          memcpy(&floatbuf, &(data[i * sizeof(float)]), sizeof(float));
          if(cmp_to_real >= (double)floatbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==5) {
        for(i=0;  i < LEN; i++) {
          memcpy(&floatbuf, &(data[i * sizeof(float)]), sizeof(float));
          if(cmp_to_real <  (double)floatbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==6) {
        for(i=0;  i < LEN; i++) {
          memcpy(&floatbuf, &(data[i * sizeof(float)]), sizeof(float));
          if(cmp_to_real >  (double)floatbuf)
            int_result[hits++] = i+1;
        }
      }
      break;
    case sizeof(double):
      if(cmp_how==1) {
        for(i=0;  i < LEN; i++) {
          memcpy(&realbuf, &(data[i * sizeof(double)]), sizeof(double));
          if(cmp_to_real == realbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==2) {
        for(i=0;  i < LEN; i++) {
          memcpy(&realbuf, &(data[i * sizeof(double)]), sizeof(double));
          if(cmp_to_real != realbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==3) {
        for(i=0;  i < LEN; i++) {
          memcpy(&realbuf, &(data[i * sizeof(double)]), sizeof(double));
          if(cmp_to_real <= realbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==4) {
        for(i=0;  i < LEN; i++) {
          memcpy(&realbuf, &(data[i * sizeof(double)]), sizeof(double));
          if(cmp_to_real >= realbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==5) {
        for(i=0;  i < LEN; i++) {
          memcpy(&realbuf, &(data[i * sizeof(double)]), sizeof(double));
          if(cmp_to_real <  realbuf)
            int_result[hits++] = i+1;
        }
      } else
      if(cmp_how==6) {
        for(i=0;  i < LEN; i++) {
          memcpy(&realbuf, &(data[i * sizeof(double)]), sizeof(double));
          if(cmp_to_real >  realbuf)
            int_result[hits++] = i+1;
        }
      }
      break;
    default:
      error("unknown floating point size");
      break;
    }
/*
    for(i=0;  i < LEN; i++) {
      memcpy(real_buf, &(data[i * sizeof(double)]), sizeof(char) * sizeof(double));
    }
*/
    break;
  case RAWSXP:
    for(i=0;  i < LEN; i++) {
      warning("unimplemented raw comparisons"); 
    }
    break;
  case STRSXP: /* {{{ */
    /* see https://svn.r-project.org/R/trunk/src/main/raw.c */
    /* fixed width character support */
    /*
    if(length(compare_to) > isNull(getAttrib(MMAP_SMODE(mmap_obj),install("nul"))) ? Cbytes-1 : Cbytes) {
      if(isNull(getAttrib(compare_how, install("partial")))) { 
        UNPROTECT(1); return allocVector(INTSXP,0);
      }  
      warning("only first %i characters of string compared", Cbytes-1);
    }
    */
    cmp_len = length(compare_to);
    cmp_to_raw = RAW(compare_to);
    char *str;
    int str_len;
    char *str_buf = R_alloc(sizeof(char), Cbytes);
    int hasnul = !!SMODE_NUL_TERM(MMAP_SMODE(mmap_obj));
    if(hasnul) {
      for(i=0; i < LEN; i++) {
        str = &(data[i*Cbytes]);
        //strncpy(str_buf, str, Cbytes);
        //str_len = strlen(str_buf);
        //str_len = (str_len > Cbytes) ? Cbytes : str_len;
        //Rprintf("strnlen(str,6):%i\n",strnlen(str,6));
        //if(str_len != cmp_len)
          //continue;
        if(memcmp(str,cmp_to_raw,cmp_len)==0)
          int_result[hits++] = i+1;
      }
//      for(i=0; i < LEN; i++) {
//          //for(b=0; b < Cbytes-1; b++) {
//          for(b=0; b < cmp_len; b++) {
//            Rprintf("%c == %c,", cmp_to_raw[b], data[i*Cbytes+b]);
//            if(cmp_to_raw[b] != data[i*Cbytes + b])
//              break;
//          }
//          Rprintf("\n");
//          if(b == Cbytes-1)
//            int_result[hits++] = i+1;
//      }
    } else {
      for(i=0; i < LEN; i++) {
        str = &(data[i*Cbytes]);
        strncpy(str_buf, str, Cbytes);
        str_len = strlen(str_buf);
        str_len = (str_len > Cbytes) ? Cbytes : str_len;
        //Rprintf("strnlen(str,6):%i\n",strnlen(str,6));
        if(str_len != cmp_len)
          continue;
        if(memcmp(str,cmp_to_raw,cmp_len)==0)
          int_result[hits++] = i+1;
      }
    }
    break; /* }}} */
  case CPLXSXP:
  default:
    error("unsupported type");
    break;
  }
  result = lengthgets(result, hits);
  UNPROTECT(1);
  return result;
  return ScalarInteger(hits);
}/*}}}*/

SEXP convert_ij_to_i (SEXP rows, SEXP i, SEXP j) {
  /* utility to take i,j subsets for matrix objects and
     convert to subset column-major array in memory */
  long n=0, jj, ii, lenj=length(j), leni=length(i);
  //int _rows = INTEGER(rows)[0];
  long _rows = ((long)REAL(rows)[0]);

  SEXP newi;
  int *_j, *_i, *_newi;

  _j = INTEGER(j);
  _i = INTEGER(i);

  PROTECT( newi = allocVector(INTSXP, leni * lenj));
  _newi = INTEGER(newi); 

  for(jj=0; jj<lenj; jj++) {
    for(ii=0; ii<leni; ii++) {
      _newi[n++] = (_j[jj] - 1) * _rows + _i[ii];
    }
  }

  UNPROTECT(1);
  return newi;
}
