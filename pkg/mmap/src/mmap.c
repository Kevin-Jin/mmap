#include "mmap.h"

#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <assert.h>

#ifndef WIN32
#ifdef HAVE_MMAP
#  include <sys/mman.h>
#endif
#ifndef HAVE_STRNLEN
size_t strnlen(const char *str, size_t max) {
  const char *end = memchr (str, 0, max);
  return end ? (size_t)(end - str) : max;
}
#endif
#else
#include <ctype.h>
#include <stdio.h>
#include <windows.h>
#include <aclapi.h>
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
#error "`char` is not 8 bits"
#endif
// If `cond` is a compile time constant that evaluates to FALSE, then GCC throws
//  a division by zero error.
#define STATIC_ASSERT(cond, message) enum { message = 1 / (cond) }
// We also want to make sure `_FILE_OFFSET_BITS` is working and that `float` and
//  `double` are the expected sizes.
STATIC_ASSERT(sizeof(float) == 4, SIZEOF_FLOAT_IS_4);
STATIC_ASSERT(sizeof(double) == 8, SIZEOF_DOUBLE_IS_4);
STATIC_ASSERT(sizeof(off_t) == 8, SIZEOF_OFF_T_IS_8);
// On Win32 and most POSIX implementations, the "OTH" family of permissions are
//  less significant than the "GRP" and "USR" families. Therefore, we want to
//  ensure that S_IXOTH is positive and a power of two.
STATIC_ASSERT(S_IXOTH > 0 && (S_IXOTH & (S_IXOTH - 1)) == 0, HAVE_PMODE);

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
  madvise
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
int32_t bitmask[32];
int32_t nbitmask[32];

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
  R_len_t i, len_flags = length(_flags);
  int flags_bit = 0x0;

  for(i = 0; i < len_flags; i++) {
    cur_string = (char *)CHAR(STRING_ELT(_flags,i));
    // mprotect() and mmap() prot bits
    if(strcmp(cur_string,"PROT_READ") == 0) {
      flags_bit = flags_bit | PROT_READ; continue;
    } else if(strcmp(cur_string,"PROT_WRITE") == 0) {
      flags_bit = flags_bit | PROT_WRITE; continue;
    } else if(strcmp(cur_string,"PROT_EXEC") == 0) {
      flags_bit = flags_bit | PROT_EXEC; continue;
    } else if(strcmp(cur_string,"PROT_NONE") == 0) {
      flags_bit = flags_bit | PROT_NONE; continue;

    // msync() flags bits
    } else if(strcmp(cur_string,"MS_ASYNC") == 0) {
      flags_bit = flags_bit | MS_ASYNC; continue;
    } else if(strcmp(cur_string,"MS_SYNC") == 0) {
      flags_bit = flags_bit | MS_SYNC; continue;
#ifndef WIN32
    } else if(strcmp(cur_string,"MS_INVALIDATE") == 0) {
      flags_bit = flags_bit | MS_INVALIDATE; continue;
#endif

    // mmap() flags bits
    } else if(strcmp(cur_string,"MAP_SHARED") == 0) {
      flags_bit = flags_bit | MAP_SHARED; continue;
    } else if(strcmp(cur_string,"MAP_PRIVATE") == 0) {
      flags_bit = flags_bit | MAP_PRIVATE; continue;
    } else if(strcmp(cur_string,"MAP_ANONYMOUS") == 0) {
      flags_bit = flags_bit | MAP_ANONYMOUS; continue;
#ifndef WIN32
    } else if(strcmp(cur_string,"MAP_FIXED") == 0) {
      flags_bit = flags_bit | MAP_FIXED; continue;
    } else if(strcmp(cur_string,"MAP_NORESERVE") == 0) {
      flags_bit = flags_bit | MAP_NORESERVE; continue;
#endif

    // mmap()'s open()/shm_open() oflag bits; natively supported by MSVCRT too.
    } else if(strcmp(cur_string,"O_RDONLY") == 0) {
      flags_bit = flags_bit | O_RDONLY; continue;
    } else if(strcmp(cur_string,"O_RDWR") == 0) {
      flags_bit = flags_bit | O_RDWR; continue;
    } else if(strcmp(cur_string,"O_CREAT") == 0) {
      flags_bit = flags_bit | O_CREAT; continue;
    } else if(strcmp(cur_string,"O_EXCL") == 0) {
      flags_bit = flags_bit | O_EXCL; continue;
    } else if(strcmp(cur_string,"O_TRUNC") == 0) {
      flags_bit = flags_bit | O_TRUNC; continue;

    // mmap()'s open()/shm_open() mode bits; partly supported by MSVCRT too.
    } else if(strcmp(cur_string,"S_IRUSR") == 0) {
      flags_bit = flags_bit | S_IRUSR; continue;
    } else if(strcmp(cur_string,"S_IWUSR") == 0) {
      flags_bit = flags_bit | S_IWUSR; continue;
    } else if(strcmp(cur_string,"S_IXUSR") == 0) {
      flags_bit = flags_bit | S_IXUSR; continue;
    } else if(strcmp(cur_string,"S_IRWXU") == 0) {
      flags_bit = flags_bit | S_IRWXU; continue;
    } else if(strcmp(cur_string,"S_IRGRP") == 0) {
      flags_bit = flags_bit | S_IRGRP; continue;
    } else if(strcmp(cur_string,"S_IWGRP") == 0) {
      flags_bit = flags_bit | S_IWGRP; continue;
    } else if(strcmp(cur_string,"S_IXGRP") == 0) {
      flags_bit = flags_bit | S_IXGRP; continue;
    } else if(strcmp(cur_string,"S_IRWXG") == 0) {
      flags_bit = flags_bit | S_IRWXG; continue;
    } else if(strcmp(cur_string,"S_IROTH") == 0) {
      flags_bit = flags_bit | S_IROTH; continue;
    } else if(strcmp(cur_string,"S_IWOTH") == 0) {
      flags_bit = flags_bit | S_IWOTH; continue;
    } else if(strcmp(cur_string,"S_IXOTH") == 0) {
      flags_bit = flags_bit | S_IXOTH; continue;
    } else if(strcmp(cur_string,"S_IRWXO") == 0) {
      flags_bit = flags_bit | S_IRWXO; continue;

    // madvise() advice enum
    } else if(strcmp(cur_string,"MADV_NORMAL") == 0) {
      flags_bit = flags_bit | MADV_NORMAL; continue;
#if defined(HAVE_MADVISE) || defined(WIN32)
    } else if(strcmp(cur_string,"MADV_RANDOM") == 0) {
      flags_bit = flags_bit | MADV_RANDOM; continue;
    } else if(strcmp(cur_string,"MADV_SEQUENTIAL") == 0) {
      flags_bit = flags_bit | MADV_SEQUENTIAL; continue;
    } else if(strcmp(cur_string,"MADV_WILLNEED") == 0) {
      flags_bit = flags_bit | MADV_WILLNEED; continue;
    } else if(strcmp(cur_string,"MADV_DONTNEED") == 0) {
      flags_bit = flags_bit | MADV_DONTNEED; continue;
#endif

    } else {
      warning("unknown constant: skipped");
    }
  }
  return ScalarInteger(flags_bit);
} /*}}}*/

/* mmap_munmap {{{ */
#ifdef WIN32
#define BUFSIZE 8192

void rperror(const char *format) {
  // `Rf_error()` calls `longjmp()` to a location set further up the call stack
  //  and never returns here to where it was invoked. Since `errorText` must be
  //  valid when passed to `Rf_error()`, yet we cannot explicitly free it from
  //  the heap after the call to `Rf_error()`, we must use either a stack
  //  variable or a static variable.
  static __thread TCHAR errorText[BUFSIZE];
  
  HRESULT errnum = HRESULT_FROM_WIN32(GetLastError());
  if(!FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM|FORMAT_MESSAGE_IGNORE_INSERTS, NULL, errnum, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPTSTR)&errorText, BUFSIZE, NULL))
    snprintf(errorText, BUFSIZE, "0x%lx", errnum);
  
  error(format, errorText);
}

void rpwarning(const char *format) {
  LPTSTR errorText = NULL;
  
  HRESULT errnum = HRESULT_FROM_WIN32(GetLastError());
  if(!FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM|FORMAT_MESSAGE_ALLOCATE_BUFFER|FORMAT_MESSAGE_IGNORE_INSERTS, NULL, errnum, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPTSTR)&errorText, 0, NULL) || errorText == NULL) {
    errorText = LocalAlloc(LMEM_FIXED, snprintf(NULL, 0, "0x%lx", errnum) + 1);
    sprintf(errorText, "0x%lx", errnum);
  }
  
  warning(format, errorText);
  LocalFree(errorText);
}

SEXP mmap_munmap (SEXP mmap_obj) {
  int success;
  unsigned char *data = MMAP_DATA(mmap_obj);
  HANDLE fd = (HANDLE)MMAP_FD(mmap_obj);
  HANDLE mh = (HANDLE)MMAP_HANDLE(mmap_obj);
  int pageoff = MMAP_PAGEOFF(mmap_obj);

  if(data == NULL) {
    success = 0;
    warning("unable to munmap file: Invalid mmap pointer.");
  } else {
    success = !!UnmapViewOfFile((unsigned char *)(data - pageoff));
    if(!success)
      rpwarning("unable to munmap file: %s");
  }
  
  // We don't actually need to keep the HANDLE returned by `CreateFileMapping()`
  //  around but instead can just `CloseHandle()` it after `MapViewOfFile()`:
  //  "to fully close a file mapping object, an application must unmap all
  //  mapped views of the file mapping object by calling UnmapViewOfFile and
  //  close the file mapping object handle by calling CloseHandle. These
  //  functions can be called in any order."
  // The fildes, on the other hand, is needed in `mmap_msync()`, although it too
  //  is safe to close after `CreateFileMapping()`: "although an application may
  //  close the file handle used to create a file mapping object, the system
  //  holds the corresponding file open until the last view of the file is
  //  unmapped. Files for which the last view has not yet been unmapped are held
  //  open with no sharing restrictions."
  CloseHandle(mh);
  if(fd != INVALID_HANDLE_VALUE)
    CloseHandle(fd);
  R_ClearExternalPtr(findVar(install("data"),mmap_obj));
  return(ScalarLogical(success));
}
#else
void rperror(const char *format) {
  error(format, strerror(errno));
}

void rpwarning(const char *format) {
  warning(format, strerror(errno));
}

SEXP mmap_munmap (SEXP mmap_obj) {
  int success;
  unsigned char *data = MMAP_DATA(mmap_obj);
  int fd = MMAP_FD(mmap_obj);
  int pageoff = MMAP_PAGEOFF(mmap_obj);

  if(data == NULL) {
    success = 0;
    warning("unable to munmap file: Invalid mmap pointer.");
  } else {
    success = !munmap((unsigned char *)(data - pageoff), (size_t)(MMAP_SIZE(mmap_obj) + pageoff));
    if(!success)
      rpwarning("unable to munmap file: %s");
  }

  SEXP fd_obj = findVar(install("filedesc"), mmap_obj);
  if(strcmp(CHAR(asChar(getAttrib(fd_obj, install("backedby")))), "shared") == 0)
    shm_unlink(CHAR(asChar(getAttrib(fd_obj, R_NamesSymbol))));
  // We don't actually need to keep the fildes around but instead can just
  //  `close()` it after `mmap()` in `mmap_mmap()`: "the mmap() function adds an
  //  extra reference to the file associated with the file descriptor fildes
  //  which is not removed by a subsequent close() on that file descriptor. This
  //  reference is removed when there are no more mappings to the file."
  close(fd);
  R_ClearExternalPtr(findVar(install("data"),mmap_obj));
  return(ScalarLogical(success)); 
} /*}}}*/
#endif

/* mmap_mmap {{{ */
#ifdef WIN32
int get_acl_max_prot_and_acc (SECURITY_DESCRIPTOR *sec_des, GENERIC_MAPPING mapping, int oflag, DWORD *max_prot, DWORD *max_acc) {
  HANDLE token = INVALID_HANDLE_VALUE;
  HANDLE imp_token = INVALID_HANDLE_VALUE;
  DWORD token_acc = TOKEN_IMPERSONATE | TOKEN_READ | TOKEN_DUPLICATE;
  DWORD granted_acc, des_acc;
  PRIVILEGE_SET priv_set = { 0 };
  DWORD priv_set_len = sizeof(priv_set);
  BOOL has_r_acc, has_w_acc, has_x_acc;
  int success = 0;
  
  // Allocate the resource and get the current process's effective access token.
  if(!OpenThreadToken(GetCurrentThread(), token_acc, TRUE, &token)
       && (GetLastError() != ERROR_NO_TOKEN || !OpenProcessToken(GetCurrentProcess(), token_acc, &token)))
    goto done;
  
  // Create an impersonation token.
  if(!DuplicateToken(token, SecurityImpersonation, &imp_token))
    goto done;
  
  // FILE_GENERIC_READ (FILE_READ_DATA, FILE_READ_EA, FILE_READ_ATTRIBUTES, STANDARD_RIGHTS_READ (READ_CONTROL), SYNCHRONIZE)
  des_acc = GENERIC_READ;
  MapGenericMask(&des_acc, &mapping);
  if(!AccessCheck(sec_des, imp_token, des_acc, &mapping, &priv_set, &priv_set_len, &granted_acc, &has_r_acc))
    goto done;
  assert(!!has_r_acc == !!(granted_acc & FILE_READ_DATA));
  
  // FILE_GENERIC_WRITE (FILE_WRITE_DATA, FILE_APPEND_DATA, FILE_WRITE_EA, FILE_WRITE_ATTRIBUTES, STANDARD_RIGHTS_WRITE (READ_CONTROL), SYNCHRONIZE)
  des_acc = GENERIC_WRITE;
  MapGenericMask(&des_acc, &mapping);
  if(!AccessCheck(sec_des, imp_token, des_acc, &mapping, &priv_set, &priv_set_len, &granted_acc, &has_w_acc))
    goto done;
  assert(!!has_w_acc == !!(granted_acc & FILE_WRITE_DATA));
  
  // FILE_GENERIC_EXECUTE (FILE_EXECUTE, FILE_READ_ATTRIBUTES, STANDARD_RIGHTS_EXECUTE (READ_CONTROL), SYNCHRONIZE)
  des_acc = GENERIC_EXECUTE;
  MapGenericMask(&des_acc, &mapping);
  if(!AccessCheck(sec_des, imp_token, des_acc, &mapping, &priv_set, &priv_set_len, &granted_acc, &has_x_acc))
    goto done;
  assert(!!has_x_acc == !!(granted_acc & FILE_EXECUTE));
  
  has_r_acc = !!(has_r_acc && ((oflag & O_RDWR) == O_RDWR || (oflag & O_RDONLY) == O_RDONLY));
  has_w_acc = !!(has_w_acc && ((oflag & O_RDWR) == O_RDWR || (oflag & O_WRONLY) == O_WRONLY));
  switch(has_r_acc << 2 | has_w_acc << 1 | has_x_acc << 0) {
  case 1 << 2 | 1 << 1 | 1 << 0: // rwx
    *max_prot = PAGE_EXECUTE_READWRITE;
    *max_acc = FILE_MAP_READ | FILE_MAP_WRITE | FILE_MAP_EXECUTE;
    break;
  case 1 << 2 | 1 << 1 | 0 << 0: // rw-
    *max_prot = PAGE_READWRITE;
    *max_acc = FILE_MAP_READ | FILE_MAP_WRITE;
    break;
  case 1 << 2 | 0 << 1 | 1 << 0: // r-x
    *max_prot = PAGE_EXECUTE_READ;
    *max_acc = FILE_MAP_READ | FILE_MAP_EXECUTE;
    break;
  case 1 << 2 | 0 << 1 | 0 << 0: // r--
    *max_prot = PAGE_READONLY;
    *max_acc = FILE_MAP_READ;
    break;
  case 0 << 2 | 0 << 1 | 1 << 0: // --x
    *max_prot = PAGE_EXECUTE;
    *max_acc = FILE_MAP_EXECUTE;
    break;
  default: // -wx, -w-, ---
    *max_prot = PAGE_NOACCESS;
    *max_acc = 0;
    break;
  }
  
  success = 1;
done:
  if(imp_token != INVALID_HANDLE_VALUE) CloseHandle(imp_token);
  if(token != INVALID_HANDLE_VALUE) CloseHandle(token);
  return success;
}

int get_pmode_max_prot_and_acc (int pmode, int oflag, DWORD *max_prot, DWORD *max_acc) {
  switch(pmode & (S_IRUSR | S_IWUSR | S_IXUSR)) {
  case S_IRUSR | S_IWUSR | S_IXUSR: // rwx
    *max_prot = PAGE_EXECUTE_READWRITE;
    *max_acc = FILE_MAP_READ | FILE_MAP_WRITE | FILE_MAP_EXECUTE;
    break;
  case S_IRUSR | S_IWUSR | 0      : // rw-
    *max_prot = PAGE_READWRITE;
    *max_acc = FILE_MAP_READ | FILE_MAP_WRITE;
    break;
  case S_IRUSR | 0       | S_IXUSR: // r-x
    *max_prot = PAGE_EXECUTE_READ;
    *max_acc = FILE_MAP_READ | FILE_MAP_EXECUTE;
    break;
  case S_IRUSR | 0       | 0      : // r--
    *max_prot = PAGE_READONLY;
    *max_acc = FILE_MAP_READ;
    break;
  case 0       | 0       | S_IXUSR: // --x
    *max_prot = PAGE_EXECUTE;
    *max_acc = FILE_MAP_EXECUTE;
    break;
  default: // -wx, -w-, ---
    *max_prot = PAGE_NOACCESS;
    *max_acc = 0;
    break;
  }
  return 1;
}

int get_file_max_prot_and_acc (LPCTSTR file, int pmode, int oflag, DWORD *max_prot, DWORD *max_acc) {
  GENERIC_MAPPING mapping = { FILE_GENERIC_READ, FILE_GENERIC_WRITE, FILE_GENERIC_EXECUTE, FILE_ALL_ACCESS };
  SECURITY_DESCRIPTOR *secDes = NULL;
  DWORD secDesSz;
  int success = 0;
  // Fetch the buffer size needed for TokenOwner by passing NULL and 0.
  if(!GetFileSecurity(file, OWNER_SECURITY_INFORMATION | GROUP_SECURITY_INFORMATION | DACL_SECURITY_INFORMATION, NULL, 0, &secDesSz) && GetLastError() != ERROR_INSUFFICIENT_BUFFER) {
    if(GetLastError() == ERROR_FILE_NOT_FOUND)
      success = get_pmode_max_prot_and_acc(pmode, oflag, max_prot, max_acc);
    goto done;
  }
  // Allocate the buffer on the heap.
  if((secDes = (SECURITY_DESCRIPTOR *)LocalAlloc(LPTR, secDesSz)) == NULL)
    goto done;
  // Fill the buffer with the usrSID.
  if(!GetFileSecurity(file, OWNER_SECURITY_INFORMATION | GROUP_SECURITY_INFORMATION | DACL_SECURITY_INFORMATION, secDes, secDesSz, &secDesSz))
    goto done;
  success = get_acl_max_prot_and_acc(secDes, mapping, oflag, max_prot, max_acc);
  
done:
  if(secDes != NULL) LocalFree(secDes);
  return success;
}

int get_share_max_prot_and_acc (LPCTSTR sharename, int pmode, int oflag, DWORD *max_prot, DWORD *max_acc) {
  GENERIC_MAPPING mapping = { FILE_MAP_READ, FILE_MAP_WRITE, FILE_MAP_EXECUTE, FILE_MAP_ALL_ACCESS };
  SECURITY_DESCRIPTOR *secDes = NULL;
  int success = 0;
  if(GetNamedSecurityInfo((LPTSTR)sharename, SE_KERNEL_OBJECT, OWNER_SECURITY_INFORMATION | GROUP_SECURITY_INFORMATION | DACL_SECURITY_INFORMATION, NULL, NULL, NULL, NULL, (void **)&secDes) != ERROR_SUCCESS) {
    if(GetLastError() == ERROR_FILE_NOT_FOUND)
      success = get_pmode_max_prot_and_acc(pmode, oflag, max_prot, max_acc);
    goto done;
  }
  success = get_acl_max_prot_and_acc(secDes, mapping, oflag, max_prot, max_acc);
  
done:
  if(secDes != NULL) LocalFree(secDes);
  return success;
}

// open() mode bits -> ACL access permissions bits.
DWORD translate_pmode_des_acc (int pmode, int r, int w, int x) {
  DWORD des_acc = 0;
  
  if(pmode & r)
    des_acc |= GENERIC_READ;
  if(pmode & w)
    des_acc |= GENERIC_WRITE;
  if(pmode & x)
    des_acc |= GENERIC_EXECUTE;
  if((pmode & (r | w | x)) == (r | w | x))
    des_acc = GENERIC_ALL;
  
  return des_acc;
}

struct sec_desc_refs {
  EXPLICIT_ACCESS ea[3];
  HANDLE token;
  TOKEN_OWNER *tokUsr;
  TOKEN_PRIMARY_GROUP *tokGrp;
  SID *othSID;
  ACL *acl;
};

int translate_pmode_sec_attr (SECURITY_ATTRIBUTES *secAttr, SECURITY_DESCRIPTOR *secDesc, struct sec_desc_refs *refs, int pmode) {
  refs->token = INVALID_HANDLE_VALUE;
  refs->tokUsr = NULL;
  refs->tokGrp = NULL;
  refs->othSID = NULL;
  refs->acl = NULL;
  
  int success = 0;
  DWORD tokUsrSz, tokGrpSz;
  DWORD errnum;
  
  // Initialize secDesc to an empty SECURITY_DESCRIPTOR.
  if(!InitializeSecurityDescriptor(secDesc, SECURITY_DESCRIPTOR_REVISION))
    goto done; // 0
  
  // Allocate the resource and get the current process's effective (impersonated) access token.
  if(!OpenThreadToken(GetCurrentThread(), TOKEN_QUERY, TRUE, &refs->token)
       && (GetLastError() != ERROR_NO_TOKEN || !OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, &refs->token)))
    goto done; // 0
  
  // Fetch the buffer size needed for TokenOwner by passing NULL and 0.
  if(!GetTokenInformation(refs->token, TokenOwner, NULL, 0, &tokUsrSz) && GetLastError() != ERROR_INSUFFICIENT_BUFFER)
    goto done; // 1
  // Allocate the buffer on the heap.
  if((refs->tokUsr = (TOKEN_OWNER *)LocalAlloc(LPTR, tokUsrSz)) == NULL)
    goto done; // 1
  // Fill the buffer with the usrSID.
  if(!GetTokenInformation(refs->token, TokenOwner, refs->tokUsr, tokUsrSz, &tokUsrSz))
    goto done; // 2
  
  // Fetch the buffer size needed for TokenPrimaryGroup by passing NULL and 0.
  if(!GetTokenInformation(refs->token, TokenPrimaryGroup, NULL, 0, &tokGrpSz) && GetLastError() != ERROR_INSUFFICIENT_BUFFER)
    goto done; // 2
  // Allocate the buffer on the heap.
  if((refs->tokGrp = (TOKEN_PRIMARY_GROUP *)LocalAlloc(LPTR, tokGrpSz)) == NULL)
    goto done; // 2
  // Fill the buffer with the grpSID.
  if(!GetTokenInformation(refs->token, TokenPrimaryGroup, refs->tokGrp, tokGrpSz, &tokGrpSz))
    goto done; // 3
  
  // Allocate and fill the buffer with the othSID on the heap.
  if(!AllocateAndInitializeSid(&(SID_IDENTIFIER_AUTHORITY){SECURITY_WORLD_SID_AUTHORITY}, 1,
                               SECURITY_WORLD_RID, 0, 0, 0, 0, 0, 0, 0, (void**)&refs->othSID))
    goto done; // 3
  
  // Initialize all members in ea to an empty EXPLICIT_ACCESS.
  ZeroMemory(&refs->ea, 3 * sizeof(EXPLICIT_ACCESS));
  
  refs->ea[0].grfAccessPermissions = translate_pmode_des_acc(pmode, S_IRUSR, S_IWUSR, S_IXUSR);
  refs->ea[0].grfAccessMode = SET_ACCESS;
  refs->ea[0].grfInheritance= CONTAINER_INHERIT_ACE | OBJECT_INHERIT_ACE;
  refs->ea[0].Trustee.TrusteeForm = TRUSTEE_IS_SID;
  refs->ea[0].Trustee.TrusteeType = TRUSTEE_IS_USER;
  refs->ea[0].Trustee.ptstrName  = (LPTSTR) refs->tokUsr->Owner;
  
  refs->ea[1].grfAccessPermissions = translate_pmode_des_acc(pmode, S_IRGRP, S_IWGRP, S_IXGRP);
  refs->ea[1].grfAccessMode = SET_ACCESS;
  refs->ea[1].grfInheritance= CONTAINER_INHERIT_ACE | OBJECT_INHERIT_ACE;
  refs->ea[1].Trustee.TrusteeForm = TRUSTEE_IS_SID;
  refs->ea[1].Trustee.TrusteeType = TRUSTEE_IS_GROUP;
  refs->ea[1].Trustee.ptstrName  = (LPTSTR) refs->tokGrp->PrimaryGroup;
  
  refs->ea[2].grfAccessPermissions = translate_pmode_des_acc(pmode, S_IROTH, S_IWOTH, S_IXOTH);
  refs->ea[2].grfAccessMode = SET_ACCESS;
  refs->ea[2].grfInheritance= CONTAINER_INHERIT_ACE | OBJECT_INHERIT_ACE;
  refs->ea[2].Trustee.TrusteeForm = TRUSTEE_IS_SID;
  refs->ea[2].Trustee.TrusteeType = TRUSTEE_IS_WELL_KNOWN_GROUP;
  refs->ea[2].Trustee.ptstrName  = (LPTSTR) refs->othSID;
  
  // Allocate a new ACL for the defined `ea` on the heap and assign it to `secDesc`.
  if((errnum = SetEntriesInAcl(3, refs->ea, NULL, &refs->acl)) != ERROR_SUCCESS) {
    SetLastError(errnum);
    goto done; // 4
  }
  if(!SetSecurityDescriptorDacl(secDesc, 1, refs->acl, 0))
    goto done; // 5
  
  secAttr->nLength = sizeof(SECURITY_ATTRIBUTES);
  secAttr->lpSecurityDescriptor = secDesc;
  secAttr->bInheritHandle = 1;
  success = 1;
  
done:
  return success;
}

void free_sec_desc (struct sec_desc_refs *refs) {
  // NOTE: the resources must be deallocated in the same order as they are
  //  allocated in `translate_pmode_sec_attr()` so that we can early exit.
  // If CloseHandle(), LocalFree(), or FreeSid() fail, we don't care. Restore
  //  the current errno at the end in case it is overwritten.
  DWORD errnum = GetLastError();
  
  // 0
  if(refs->token == INVALID_HANDLE_VALUE) goto done;
  // 1
  CloseHandle(refs->token);
  refs->token = INVALID_HANDLE_VALUE;
  if(refs->tokUsr == NULL) goto done;
  // 2
  LocalFree(refs->tokUsr);
  refs->tokUsr = NULL;
  if(refs->tokGrp == NULL) goto done;
  // 3
  LocalFree(refs->tokGrp);
  refs->tokGrp = NULL;
  if(refs->othSID == NULL) goto done;
  // 4
  FreeSid(refs->othSID);
  refs->othSID = NULL;
  if(refs->acl == NULL) goto done;
  // 5
  LocalFree(refs->acl);
  refs->acl = NULL;
  
done:
  SetLastError(errnum);
}

// open() oflag bits -> CreateFile() desired access bits.
DWORD translate_oflag_des_acc (int oflag, DWORD max_acc) {
  // Not specifying exactly one of O_RDONLY or O_RDWR is unspecified behavior so
  //  don't bother checking that particular case.
  if((oflag & O_RDWR) == O_RDWR) {
    if(max_acc & FILE_MAP_EXECUTE)
      return GENERIC_READ | GENERIC_WRITE | GENERIC_EXECUTE;
    else
      return GENERIC_READ | GENERIC_WRITE;
  } else if((oflag & O_RDONLY) == O_RDONLY) {
    if(max_acc & FILE_MAP_EXECUTE)
      return GENERIC_READ | GENERIC_EXECUTE;
    else
      return GENERIC_READ;
  }
  return 0;
}

// open() oflag bits -> CreateFile() creation disposition enum.
DWORD translate_oflag_creat_disp (int oflag) {
  // O_EXCL without O_CREAT behaves as if neither was specified.
  switch(oflag & (O_CREAT | O_EXCL | O_TRUNC)) {
  default:
  case 0       | 0      | 0:
  case 0       | O_EXCL | 0:
    // Open a file; fail if it doesn't already exist.
    return OPEN_EXISTING;
  case 0       | 0      | O_TRUNC:
  case 0       | O_EXCL | O_TRUNC:
    // Clear a file; fail if it doesn't already exist.
    return TRUNCATE_EXISTING;
  case O_CREAT | 0      | 0:
    // Open a file; create one first if it doesn't already exist.
    return OPEN_ALWAYS;
  case O_CREAT | 0      | O_TRUNC:
    // Clear a file; create one first if it doesn't already exist.
    return CREATE_ALWAYS;
  case O_CREAT | O_EXCL | 0:
  case O_CREAT | O_EXCL | O_TRUNC:
    // Clear a file; fail if it does already exist.
    return CREATE_NEW;
  }
}

// mmap() prot bits and flags bits -> MapViewOfFile() prot enum.
// mprotect() prot bits -> VirtualProtect() prot enum.
DWORD translate_prot_and_flags (int prot, int flags) {
  if(prot & PROT_WRITE) {
    if(flags & MAP_SHARED) {
      if(prot & PROT_EXEC)
        return PAGE_EXECUTE_READWRITE;
      else
        return PAGE_READWRITE;
    } else if(flags & MAP_PRIVATE) {
      if(prot & PROT_EXEC)
        return PAGE_EXECUTE_WRITECOPY;
      else
        return PAGE_WRITECOPY;
    }
  } else if(prot & PROT_READ) {
    if(prot & PROT_EXEC)
      return PAGE_EXECUTE_READ;
    else
      return PAGE_READONLY;
  } else if(prot == PROT_NONE) {
    return PAGE_NOACCESS;
  }
  
  return 0;
}

// madvise() advice enum -> CreateFile() flag bits.
DWORD translate_advice_cf (SEXP _advice) {
  int *advice_p;
  R_len_t i, len = length(_advice);
  DWORD cf_advice = 0;
  
  PROTECT(_advice = coerceVector(_advice, INTSXP));
  advice_p = INTEGER(_advice);
  for(i = 0; i < len; i++) {
    switch(advice_p[i]) {
    case MADV_NORMAL:
      cf_advice |= FILE_ATTRIBUTE_NORMAL;
      break;
    case MADV_RANDOM:
      cf_advice |= FILE_FLAG_RANDOM_ACCESS;
      break;
    case MADV_SEQUENTIAL:
      cf_advice |= FILE_FLAG_SEQUENTIAL_SCAN;
      break;
    }
  }
  
  UNPROTECT(1);
  return cf_advice;
}

SEXP mmap_mmap (SEXP _type, SEXP _fildesc, SEXP _sharename, SEXP _prot,
                SEXP _flags, SEXP _oflag, SEXP _pmode, SEXP _advice,
                SEXP _len, SEXP _off, SEXP _pageoff) {
  unsigned char *data;
  SYSTEM_INFO sSysInfo;
  GetSystemInfo(&sSysInfo);
  int oflag = (xlength(_flags) == 0) ? 0 : asInteger(_oflag);
  int pmode = (xlength(_pmode) == 0) ? 0 : asInteger(_pmode);
  int flags = asInteger(_flags);
  SECURITY_ATTRIBUTES secAttr;
  SECURITY_DESCRIPTOR secDesc;
  struct sec_desc_refs tmp_resrc;
  int zero_view = 0;
  
  assert(asReal(_pageoff) <= INT_MAX_VALUE);
  if(asReal(_len) + asInteger(_pageoff) > (SIZE_T)-1)
    error("unable to mmap file: Requested length is too high.");
  
  if((!(flags & MAP_PRIVATE) && !(flags & MAP_SHARED)) || ((flags & MAP_PRIVATE) && (flags & MAP_SHARED)))
    error("unable to mmap file: Exactly one of MAP_PRIVATE or MAP_SHARED must be set.");
  
  HANDLE hFile, hMap;
  DWORD max_prot, max_acc;
  
  ULARGE_INTEGER len = {.QuadPart  = (uint64_t)(asReal(_len) + asInteger(_pageoff) + asReal(_off))};
  if(xlength(_fildesc) != 0 && xlength(_sharename) != 0) {
    error("unable to mmap file: File name and share name were both given.");
  } else if(xlength(_fildesc) != 0) {
    if(flags == MAP_ANONYMOUS)
      error("unable to mmap file: File name was given but MAP_ANONYMOUS was set.");
    
    // Give the system's most permissive access to `CreateFile()` first so that
    //  we can give the most permissive protections to `CreateFileMapping()` and
    //  the most permissive access to `MapViewOfFile()`.
    // Then we can grant more permissions of the pointer at any time without a
    //  problem through `VirtualProtect()` in `mmap_mprotect()`. Otherwise, we
    //  could only deny existing permissions later but not grant new ones.
    if(!get_file_max_prot_and_acc(CHAR(asChar(_fildesc)), pmode, oflag, &max_prot, &max_acc))
      rperror("unable to open file: %s");
    
    if(!translate_pmode_sec_attr(&secAttr, &secDesc, &tmp_resrc, pmode)) {
      free_sec_desc(&tmp_resrc);
      rperror("unable to open file: %s");
    }
    hFile = CreateFile(CHAR(asChar(_fildesc)),
                    translate_oflag_des_acc(oflag, max_acc),
                    FILE_SHARE_READ | FILE_SHARE_WRITE, &secAttr,
                    translate_oflag_creat_disp(oflag),
                    translate_advice_cf(_advice), NULL);
    if(hFile == INVALID_HANDLE_VALUE) {
      free_sec_desc(&tmp_resrc);
      rperror("unable to open file: %s");
    }
    if((intptr_t)hFile > INT_MAX || (intptr_t)hFile < INT_MIN) {
      free_sec_desc(&tmp_resrc);
      CloseHandle(hFile);
      error("unable to open file: File descriptor overflow.");
    }
    
    hMap = CreateFileMapping(hFile, &secAttr, max_prot, len.HighPart, len.LowPart, NULL);
    free_sec_desc(&tmp_resrc);
  } else if(xlength(_sharename) != 0) {
    if(flags == MAP_ANONYMOUS)
      error("unable to mmap file: Share name was given but MAP_ANONYMOUS was set.");
    
    if(!get_share_max_prot_and_acc(CHAR(asChar(_sharename)), pmode, oflag, &max_prot, &max_acc))
      rperror("unable to open file: %s");
    
    hFile = INVALID_HANDLE_VALUE;
    if(oflag & O_CREAT) {
      if(!translate_pmode_sec_attr(&secAttr, &secDesc, &tmp_resrc, pmode)) {
        free_sec_desc(&tmp_resrc);
        rperror("unable to open file: %s");
      }
      hMap = CreateFileMapping(hFile, &secAttr, max_prot, len.HighPart, len.LowPart, CHAR(asChar(_sharename)));
      free_sec_desc(&tmp_resrc);
      if(hMap != NULL && GetLastError() == ERROR_ALREADY_EXISTS) {
        if(oflag & O_EXCL) {
          CloseHandle(hMap);
          hMap = NULL;
        } else if(oflag & O_TRUNC) {
          zero_view = 1;
        }
      }
    } else {
      // MapViewOfFile() doesn't complain if `len` is greater than the initial
      //  `len` of the shared memory mapping. If it ever does, we must use
      //  `CreateFileMapping()` instead and throw an error if that call does not
      //  throw `ERROR_ALREADY_EXISTS`.
      hMap = OpenFileMapping(max_acc, 1, CHAR(asChar(_sharename)));
      if(oflag & O_TRUNC)
        zero_view = 1;
    }
  } else {
    if(!(flags & MAP_ANONYMOUS))
      error("unable to mmap file: Neither file name and share name were given but MAP_ANONYMOUS was not set.");
    if(xlength(_oflag) != 0)
      error("unable to mmap file: oflag was given but neither file name nor share name were given.");
    if(xlength(_pmode) != 0)
      error("unable to mmap file: pmode was given but neither file name nor share name were given.");
    // `MAP_ANONYMOUS | MAP_SHARED` is meaningful on Linux because processes can
    //  be forked after the call to `mmap()` and the child will retain the same
    //  memory mapping. There is no such functionality on Windows, so it is
    //  impossible to share the nameless memory mapping with anyone else.
    // Additionally, accepting a non-zero `off` is not meaningful for
    //  `MAP_ANONYMOUS` because all of the bytes skipped over will always be
    //  initialized to 0 and will only ever be accessible to us.
    // Still, we will just let Win32 decide whether to accept these useless
    //  combinations of parameters. Our own parameter checking should merely
    //  ensure that _fildesc and _sharename are ignored when `MAP_ANONYMOUS` is
    //  set, to imitate the interface for most Unix-like operating systems.
    // Similarly, we leave much of the parameter checking to the kernel in our
    //  POSIX implementation in order to provide as low-level an interface as
    //  possible. For example, when `flags & MAP_ANONYMOUS`, we'll let FreeBSD
    //  complain if `off != 0 || fildes != -1`; but we'll also let Solaris,
    //  OpenBSD, NetBSD, HP-UX, and AIX complain only if `fildes != -1`; and let
    //  Linux just ignore `fildes`; and let macOS just ignore `off`.
    
    max_prot = PAGE_EXECUTE_READWRITE;
    max_acc = FILE_MAP_READ | FILE_MAP_WRITE | FILE_MAP_EXECUTE;
    
    hFile = INVALID_HANDLE_VALUE;
    hMap = CreateFileMapping(hFile, NULL, max_prot, len.HighPart, len.LowPart, NULL);
  }

  if(hMap == NULL) {
    CloseHandle(hFile);
    rperror("unable to mmap file: %s");
  }
  if((intptr_t)hMap > INT_MAX || (intptr_t)hMap < INT_MIN) {
    CloseHandle(hMap);
    CloseHandle(hFile);
    error("unable to mmap file: Map handle overflow.");
  }
  ULARGE_INTEGER off = {.QuadPart  = (uint64_t)asReal(_off)};
  data = (unsigned char *)MapViewOfFile(hMap, max_acc, off.HighPart, off.LowPart, (SIZE_T)(asReal(_len) + asInteger(_pageoff)));
  if(data == NULL) {
    CloseHandle(hMap);
    CloseHandle(hFile);
    rperror("unable to mmap file: %s");
  }
  if(!VirtualProtect(data, (SIZE_T)(asReal(_len) + asInteger(_pageoff)), translate_prot_and_flags(asInteger(_prot), flags), &max_prot)) {
    CloseHandle(hMap);
    CloseHandle(hFile);
    rperror("unable to mmap file: %s");
  }
  if(zero_view)
    // This doesn't quite do the trick when `MAP_PRIVATE` is set, but it's the
    //  best we can do when `O_TRUNC` is specified for shared memory.
    ZeroMemory(data, (SIZE_T)(asReal(_len) + asInteger(_pageoff)));
  
  data = data + asInteger(_pageoff); /* advance ptr to byte offset from page boundary */

  SEXP mmap_obj;
  PROTECT(mmap_obj = allocSExp(ENVSXP));
  SET_FRAME(mmap_obj, R_NilValue);
  SET_ENCLOS(mmap_obj, R_NilValue);
  SET_HASHTAB(mmap_obj, R_NilValue);
  SET_ATTRIB(mmap_obj, R_NilValue);
  defineVar(install("data"), R_MakeExternalPtr(data, R_NilValue, R_NilValue),mmap_obj);
  defineVar(install("pageoff"), _pageoff,mmap_obj);
  defineVar(install("bytes"), _len,mmap_obj);
  defineVar(install("filedesc"), ScalarInteger((int)(intptr_t)hFile),mmap_obj);
  defineVar(install("storage.mode"), _type,mmap_obj);
  defineVar(install("handle"), ScalarInteger((int)(intptr_t)hMap),mmap_obj);
  defineVar(install("flags"), _flags,mmap_obj);
  defineVar(install("dim"), R_NilValue ,mmap_obj);
  UNPROTECT(1);
  return(mmap_obj);
}
#else
SEXP mmap_madvise (SEXP, SEXP, SEXP);

SEXP mmap_mmap (SEXP _type, SEXP _fildesc, SEXP _sharename, SEXP _prot,
                SEXP _flags, SEXP _oflag, SEXP _pmode, SEXP _advice,
                SEXP _len, SEXP _off, SEXP _pageoff) {
  int fd;
  unsigned char *data;
  struct stat st;
  
  assert(asReal(_pageoff) <= INT_MAX_VALUE);
  if(asReal(_len) + asInteger(_pageoff) > SIZE_MAX)
    error("unable to mmap file: Requested length is too high.");
  
  if(xlength(_fildesc) != 0 && xlength(_sharename) != 0) {
    error("unable to mmap file: File name and share name were both given.");
  } else if(xlength(_fildesc) != 0) {
    fd = open(CHAR(asChar(_fildesc)), asInteger(_oflag), (mode_t)asInteger(_pmode));
    if(fd < 0)
      rperror("unable to open file: %s");
  } else if(xlength(_sharename) != 0) {
    fd = shm_open(CHAR(asChar(_sharename)), asInteger(_oflag), (mode_t)asInteger(_pmode));
    if(fd < 0)
      rperror("unable to open file: %s");
  } else {
    if(xlength(_oflag) != 0)
      error("unable to mmap file: oflag was given but neither file name nor share name were given.");
    if(xlength(_pmode) != 0)
      error("unable to mmap file: pmode was given but neither file name nor share name were given.");
    fd = -1;
  }
  
  // Lengthen file if necessary.
  if(fd >= 0) {
    fstat(fd, &st);
    if((off_t)asReal(_len) > st.st_size)
      if(!!ftruncate(fd, (off_t)asReal(_len)))
        rperror("unable to mmap file: %s");
  }
  
  data = mmap((caddr_t)0, 
              (size_t)(asReal(_len) + asInteger(_pageoff)), 
              asInteger(_prot), 
              asInteger(_flags), 
              fd, 
              (off_t)asReal(_off));
  if(data == MAP_FAILED) {
    close(fd);
    rperror("unable to mmap file: %s");
  }
  
  data = data + asInteger(_pageoff); /* advance ptr to byte offset from page boundary */
  
  SEXP mmap_obj;
  PROTECT(mmap_obj = allocSExp(ENVSXP));
  SET_FRAME(mmap_obj, R_NilValue);
  SET_ENCLOS(mmap_obj, R_NilValue);
  SET_HASHTAB(mmap_obj, R_NilValue);
  SET_ATTRIB(mmap_obj, R_NilValue);
  defineVar(install("data"), R_MakeExternalPtr(data, R_NilValue, R_NilValue),mmap_obj);
  defineVar(install("pageoff"), _pageoff,mmap_obj);
  defineVar(install("bytes"), _len,mmap_obj);
  defineVar(install("filedesc"), ScalarInteger(fd),mmap_obj);
  defineVar(install("storage.mode"), _type,mmap_obj);
  defineVar(install("dim"), R_NilValue ,mmap_obj);
  
  mmap_madvise(mmap_obj, R_NilValue, _advice);
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

SEXP mmap_allocation_granularity () {
  SYSTEM_INFO sSysInfo;
  GetSystemInfo(&sSysInfo);
  return ScalarInteger((int)sSysInfo.dwAllocationGranularity);
}
#else
SEXP mmap_pagesize () {
  return ScalarInteger((int)sysconf(_SC_PAGE_SIZE));
}

SEXP mmap_allocation_granularity () {
  return mmap_pagesize();
}
#endif
/*}}}*/

/* mmap_is_mmapped {{{ */
SEXP mmap_is_mmapped (SEXP mmap_obj) {
  return(ScalarLogical(MMAP_DATA(mmap_obj) != NULL));
} /*}}}*/

#ifdef WIN32
/* {{{ mmap_msync */
SEXP mmap_msync (SEXP mmap_obj, SEXP _flags) {
  unsigned char *data;
  data = MMAP_DATA(mmap_obj);
  HANDLE fd = (HANDLE)MMAP_FD(mmap_obj);
  int flags = asInteger(_flags);
  int success;
  
  success = ((flags & MS_SYNC) || (flags & MS_ASYNC)) && (!(flags & MS_SYNC) || !(flags & MS_ASYNC));
  if(!success)
    warning("unable to msync file: Exactly one of MS_SYNC or MS_ASYNC must be set.");
  
  if(success && fd != INVALID_HANDLE_VALUE) {
    success = !!FlushViewOfFile((void *)data, (SIZE_T)MMAP_SIZE(mmap_obj));
    if(success && (flags & MS_SYNC))
      success = !!FlushFileBuffers(fd);
    if(!success)
      rpwarning("unable to msync file: %s");
  }
  
  if(success && fd != INVALID_HANDLE_VALUE && (MMAP_FLAGS(mmap_obj) & MAP_SHARED)) {
    // "When modifying a file through a mapped view, the last modification
    //  timestamp may not be updated automatically."
    // Ideally, we would only update the modification time if the file were
    //  dirty, but there is no easy way to check except to maintain additional
    //  state ourselves, which is overkill for just updating the file metadata.
    FILETIME ft;
    SYSTEMTIME st;
    GetSystemTime(&st);
    if(!SystemTimeToFileTime(&st, &ft) || !SetFileTime(fd, (LPFILETIME)NULL, (LPFILETIME)NULL, &ft))
      rpwarning("unable to msync file: %s");
  }
  
  return ScalarLogical(success);
}
#else
SEXP mmap_msync (SEXP mmap_obj, SEXP _flags) {
  unsigned char *data;
  R_xlen_t pageoff = MMAP_PAGEOFF(mmap_obj);
  data = MMAP_DATA(mmap_obj);
  // "The implementation  will require that addr be a multiple of the page size"
  int success = !msync((unsigned char *)(data - pageoff), (size_t)(MMAP_SIZE(mmap_obj) + pageoff), asInteger(_flags));
  if(!success)
    rpwarning("unable to msync file: %s");
  return ScalarLogical(success);
}/*}}}*/
#endif

ptrdiff_t inline align_down(ptrdiff_t idx, ptrdiff_t pageoff, ptrdiff_t pagesize) {
  // If pagesize is a power of 2, the bit twiddling hack rounds down
  //  `idx + pageoff` to the nearest multiple of pagesize without any
  //  division operations, i.e. equivalently:
  //    return (R_xlen_t)floor((idx + pageoff) / pagesize) * pagesize - pageoff;
  // Note that result can be negative if `pageoff != 0` and `idx` is small.
  // This is not a buffer overflow; we are still referencing a valid address
  //  since we assigned `data = &mmap(...)[pageoff]` in `mmap_mmap()` and the
  //  the left hand side of the subtraction is always non-negative since
  //  `idx` and `pageoff` are both non-negative and `pagesize` is positive.
  assert(idx >= 0 && pageoff >= 0);
  assert(pagesize > 0 && (pagesize & (pagesize - 1)) == 0);
  return ((idx + pageoff) & ~(pagesize - 1)) - pageoff;
}

ptrdiff_t inline align_up(ptrdiff_t idx, ptrdiff_t pageoff, ptrdiff_t pagesize) {
  return align_down(idx + (pagesize - 1), pageoff, pagesize);
}

/* {{{ mmap_madvise */
#ifdef WIN32
SEXP mmap_madvise (SEXP mmap_obj, SEXP index, SEXP _advice) {
  R_xlen_t i, LEN;
  R_xlen_t u, mmap_len, Cbytes;
  unsigned char *data;
  
  typedef struct { PVOID addr; SIZE_T size; } mem_range;
  typedef BOOL (WINAPI *madv_willneed_fn)(HANDLE, ULONG_PTR, mem_range*, ULONG);
  
  HMODULE kern32 = NULL;
  madv_willneed_fn madv_willneed = NULL;

  data = MMAP_DATA(mmap_obj);
  LEN = xlength(index);
  
  SEXP success;
  
  PROTECT(success = allocVector(LGLSXP, LEN == 0 ? 1 : LEN));
  PROTECT(index = coerceVector(index, REALSXP));
  double *index_p = REAL(index);
  
  Cbytes = SMODE_CBYTES(MMAP_SMODE(mmap_obj));
  mmap_len = MMAP_SIZE(mmap_obj) / Cbytes; /* length.mmap() */

  if(asInteger(_advice) == MADV_WILLNEED) {
    // Cannot use load-time dynamic linking because mingw may link against an
    //  old kernel32.lib and memoryapi.h file so compilation fails.
    // Conveniently, kernel32.dll appears to always be loaded at runtime, so we
    //  don't have to deal with the hassle of LoadLibrary() and FreeLibrary()
    //  and can just simply use GetModuleHandle() for run-time dynamic linking.
    kern32 = GetModuleHandle("kernel32.dll");
    if(kern32 == NULL)
      rpwarning("unable to madvise file: %s");
    
    if(kern32 != NULL) {
      madv_willneed = (madv_willneed_fn)GetProcAddress(kern32, "PrefetchVirtualMemory");
      if(madv_willneed == NULL)
        rpwarning("unable to madvise file: %s");
    }
  } else if(asInteger(_advice) != MADV_DONTNEED) {
    warning("unable to madvise file: This advice cannot be changed once set.");
  }
  
  if(madv_willneed != NULL) {
    if(LEN == 0) {
      LOGICAL(success)[0] = !!madv_willneed(GetCurrentProcess(), 1, &(mem_range){data, (SIZE_T)MMAP_SIZE(mmap_obj)}, 0);
      if(!LOGICAL(success)[0])
        rpwarning("unable to madvise file: %s");
    }
    
    for(i = 0; i < LEN; i++) {
      u = (R_xlen_t)index_p[i] - 1;
      if(u >= mmap_len || u < 0)
        error("'i=%i' out of bounds", u + 1);
      
      LOGICAL(success)[i] = !!madv_willneed(GetCurrentProcess(), 1, &(mem_range){&data[u * Cbytes], (SIZE_T)Cbytes}, 0);
      if(!LOGICAL(success)[i])
        rpwarning("unable to madvise file: %s");
    }
  } else if(asInteger(_advice) == MADV_DONTNEED) {
    // "Calling VirtualUnlock on a range of memory that is not locked releases
    //  the pages from the process's working set."
    // "If any of the pages in the specified range are not locked, VirtualUnlock
    //  removes such pages from the working set, sets last error to
    //  ERROR_NOT_LOCKED, and returns FALSE."
    // According to Task Manager, this indeed reduces the "working set size"
    //  immediately just like how "the resident set size (RSS) of the calling
    //  process will be immediately reduced" on Linux with "MADV_DONTNEED". In
    //  other words, physical memory is freed up for other processes to use.
    if(LEN == 0) {
      LOGICAL(success)[0] = !!VirtualUnlock(data, (SIZE_T)MMAP_SIZE(mmap_obj)) || GetLastError() == ERROR_NOT_LOCKED;
      if(!LOGICAL(success)[0])
        rpwarning("unable to madvise file: %s");
    }
    
    for(i = 0; i < LEN; i++) {
      u = (R_xlen_t)index_p[i] - 1;
      if(u >= mmap_len || u < 0)
        error("'i=%i' out of bounds", u + 1);
      
      LOGICAL(success)[i] = !!VirtualUnlock(&data[u * Cbytes], (SIZE_T)Cbytes) || GetLastError() == ERROR_NOT_LOCKED;
      if(!LOGICAL(success)[i])
        rpwarning("unable to madvise file: %s");
    }
  } else {
    if(LEN == 0)
      LOGICAL(success)[0] = 0;
    
    for(i = 0; i < LEN; i++) {
      u = (R_xlen_t)index_p[i] - 1;
      if(u >= mmap_len || u < 0)
        error("'i=%i' out of bounds", u + 1);
      
      LOGICAL(success)[i] = 0;
    }
  }
  
  UNPROTECT(2);
  return success;
}
#else
SEXP mmap_madvise (SEXP mmap_obj, SEXP index, SEXP _advice) {
  R_xlen_t i, LEN;
  R_xlen_t u, mmap_len, Cbytes;
#ifdef HAVE_MADVISE
  unsigned char *data;
  R_xlen_t aligned;
  R_xlen_t pagesize = asInteger(mmap_pagesize());
  R_xlen_t pageoff = MMAP_PAGEOFF(mmap_obj);
  data = MMAP_DATA(mmap_obj);
#endif
  LEN = xlength(index);

  SEXP success;
  
  if(LEN == 0) {
#ifdef HAVE_MADVISE
    // Note: MADV_DONTNEED is kind of dangerous since it's destructive on some
    //  POSIX implementations. On Linux: "If the pages are dirty, it's OK to
    //  just throw them away. [...] An interface that causes the system to free
    //  clean pages and flush dirty pages is already available as
    //  msync(MS_INVALIDATE)."
    // This warning should definitely show up in the R documentation and should
    //  advise the user to call `msync(MS_SYNC)` before using `MADV_DONTNEED`.
    if(!madvise((unsigned char *)(data - pageoff), (size_t)(MMAP_SIZE(mmap_obj) + pageoff), asInteger(_advice)))
      return ScalarLogical(1);
    rpwarning("unable to madvise file: %s");
#else
    warning("unable to madvise file: This advice cannot be changed once set.");
#endif
    return ScalarLogical(0);
  }
  
  PROTECT(success = allocVector(LGLSXP, LEN));
  PROTECT(index = coerceVector(index, REALSXP));
  double *index_p = REAL(index);
  
  Cbytes = SMODE_CBYTES(MMAP_SMODE(mmap_obj));
  mmap_len = MMAP_SIZE(mmap_obj) / Cbytes; /* length.mmap() */
  for(i = 0; i < LEN; i++) {
    u = (R_xlen_t)index_p[i] - 1;
    if(u >= mmap_len || u < 0)
      error("'i=%i' out of bounds", u + 1);
    
#ifdef HAVE_MADVISE
    // "The Linux implementation requires that the address addr be page-aligned"
    aligned = align_down(u * Cbytes, pageoff, pagesize);
    LOGICAL(success)[i] = !madvise(&data[aligned], (size_t)((u + 1) * Cbytes - aligned), asInteger(_advice));
    if(!LOGICAL(success)[i])
      rpwarning("unable to madvise file: %s");
#else
    LOGICAL(success)[i] = 0;
    warning("unable to madvise file: This advice cannot be changed once set.");
#endif
  }
  
  UNPROTECT(2);
  return success;
}
#endif

/* {{{ mmap_mprotect */
SEXP mmap_mprotect (SEXP mmap_obj, SEXP index, SEXP _prot) {
  R_xlen_t i, LEN;
  R_xlen_t u, mmap_len, Cbytes;
  unsigned char *data;
  
#ifndef WIN32
  R_xlen_t aligned;
  R_xlen_t pagesize = asInteger(mmap_pagesize());
  int prot = asInteger(_prot);
  R_xlen_t pageoff = MMAP_PAGEOFF(mmap_obj);
#else
  DWORD old_prot;
  DWORD prot = translate_prot_and_flags(asInteger(_prot), MMAP_FLAGS(mmap_obj));
#endif
  
  data = MMAP_DATA(mmap_obj);
  LEN = xlength(index);

  SEXP success;
  
  if(LEN == 0) {
#ifndef WIN32
    if(!mprotect((unsigned char *)(data - pageoff), (size_t)(MMAP_SIZE(mmap_obj) + pageoff), prot))
      return ScalarLogical(1);
#else
    if(VirtualProtect(data, (SIZE_T)MMAP_SIZE(mmap_obj), prot, &old_prot))
      return ScalarLogical(1);
#endif
    rpwarning("unable to mprotect file: %s");
    return ScalarLogical(0);
  }
  
  PROTECT(success = allocVector(LGLSXP, LEN));
  PROTECT(index = coerceVector(index, REALSXP));
  double *index_p = REAL(index);

  Cbytes = SMODE_CBYTES(MMAP_SMODE(mmap_obj));
  mmap_len = MMAP_SIZE(mmap_obj) / Cbytes; /* length.mmap() */
  for(i = 0; i < LEN; i++) {
    u = (R_xlen_t)index_p[i] - 1;
    if(u >= mmap_len || u < 0)
      error("'i=%i' out of bounds", u + 1);
    
#ifndef WIN32
    // "addr must be aligned to a page boundary."
    aligned = align_down(u * Cbytes, pageoff, pagesize);
    LOGICAL(success)[i] = !mprotect(&data[aligned], (size_t)((u + 1) * Cbytes - aligned), prot);
#else
    LOGICAL(success)[i] = !!VirtualProtect(&data[u * Cbytes], (SIZE_T)Cbytes, prot, &old_prot);
#endif
    if(!LOGICAL(success)[i])
      rpwarning("unable to mprotect file: %s");
  }
  
  UNPROTECT(2);
  return success;
}/*}}}*/

void logical_extract(unsigned char *data, SEXP dat, R_xlen_t LEN, double *index_p, R_xlen_t mmap_len, R_xlen_t record_size, R_xlen_t offset, SEXP smode, int swap) {
  R_xlen_t i, u;
  int *lgl_dat;
  int8_t bytebuf;
  int32_t intbuf;
  div_t word;
  
  lgl_dat = LOGICAL(dat);
  if(strcmp(SMODE_CTYPE(smode), "bitset") == 0) { /* bitset */
    for(i = 0; i < LEN; i++) {
      u = (R_xlen_t)index_p[i] - 1;
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
        u = (R_xlen_t)index_p[i] - 1;
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
        u = (R_xlen_t)index_p[i] - 1;
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

void integer_extract(unsigned char *data, SEXP dat, R_xlen_t LEN, double *index_p, R_xlen_t mmap_len, R_xlen_t record_size, R_xlen_t offset, SEXP smode, int swap) {
  R_xlen_t i, u;
  int *int_dat;
  int8_t bytebuf;
  int16_t sbuf;
  int32_t intbuf;
  
  int_dat = INTEGER(dat);
  switch(SMODE_CBYTES(smode)) {
  case 1:
    if(SMODE_SIGNED(smode)) { /* int8 */
      for(i = 0; i < LEN; i++) {
        u = (R_xlen_t)index_p[i] - 1;
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
        u = (R_xlen_t)index_p[i] - 1;
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
        u = (R_xlen_t)index_p[i] - 1;
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
        u = (R_xlen_t)index_p[i] - 1;
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
      u = (R_xlen_t)index_p[i] - 1;
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

void double_extract(unsigned char *data, SEXP dat, R_xlen_t LEN, double *index_p, R_xlen_t mmap_len, R_xlen_t record_size, R_xlen_t offset, SEXP smode, int swap) {
  R_xlen_t i, u;
  double *real_dat;
  int64_t longbuf;
  float floatbuf;
  double realbuf;
  
  real_dat = REAL(dat);
  switch(SMODE_CBYTES(smode)) {
  case 4: /* real32 */
    for(i = 0; i < LEN; i++) {
      u = (R_xlen_t)index_p[i] - 1;
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
        u = (R_xlen_t)index_p[i] - 1;
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
        u = (R_xlen_t)index_p[i] - 1;
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

void complex_extract(unsigned char *data, SEXP dat, R_xlen_t LEN, double *index_p, R_xlen_t mmap_len, R_xlen_t record_size, R_xlen_t offset, int swap) {
  R_xlen_t i, u;
  Rcomplex *complex_dat;
  double doublepairbuf[2];
  Rcomplex Rcomplexbuf;
  
  complex_dat = COMPLEX(dat);
  for(i = 0; i < LEN; i++) {
    u = (R_xlen_t)index_p[i] - 1;
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

void character_extract(unsigned char *data, SEXP dat, R_xlen_t LEN, double *index_p, R_xlen_t mmap_len, R_xlen_t record_size, R_xlen_t offset, SEXP smode) {
  R_xlen_t i, u, fieldCbytes;
  char *str;
  
  fieldCbytes = SMODE_CBYTES(smode);
  if(SMODE_NUL_TERM(smode)) {
    for(i = 0; i < LEN; i++) {
      u = (R_xlen_t)index_p[i] - 1;
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
      u = (R_xlen_t)index_p[i] - 1;
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

void raw_extract(unsigned char *data, SEXP dat, R_xlen_t LEN, double *index_p, R_xlen_t mmap_len, R_xlen_t record_size, R_xlen_t offset) {
  R_xlen_t i, u;
  Rbyte *raw_dat;

  raw_dat = RAW(dat);
  for(i = 0; i < LEN; i++) {
    u = (R_xlen_t)index_p[i] - 1;
    if(u >= mmap_len || u < 0)
      error("'i=%i' out of bounds", u + 1);
    
    raw_dat[i] = (unsigned char)data[u * record_size + offset];
  }
}

/* {{{ mmap_extract */
SEXP mmap_extract (SEXP index, SEXP field, SEXP dim, SEXP mmap_obj, SEXP swap_byte_order) {
/*SEXP mmap_extract (SEXP index, SEXP field, SEXP mmap_obj) {*/
  R_xlen_t v, fi;
  int P = 0;
  unsigned char *data;

  PROTECT(index = coerceVector(index, REALSXP)); P++;
  PROTECT(field = coerceVector(field, REALSXP)); P++;
  SEXP vec_smode, smode = MMAP_SMODE(mmap_obj);
  R_xlen_t LEN = xlength(index);
  SEXPTYPE mode = TYPEOF(smode);
  R_xlen_t Cbytes = SMODE_CBYTES(smode);
  R_xlen_t mmap_len = MMAP_SIZE(mmap_obj) / Cbytes; /* length.mmap() */

  data = MMAP_DATA(mmap_obj);
  if(data == NULL)
    error("invalid mmap pointer");

  SEXP dat; /* dat is either a column, or list of columns */
  if(mode == VECSXP)
    PROTECT(dat = allocVector(VECSXP, xlength(field)));
  else
    PROTECT(dat = allocVector(mode, LEN));
  P++;

  double *index_p = REAL(index);

  R_xlen_t offset;
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
    for(fi = 0; fi < xlength(field); fi++) {
      v = (R_xlen_t)REAL(field)[fi] - 1;
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

void logical_replace(unsigned char *data, SEXP value, R_xlen_t LEN, double *index_p, R_xlen_t mmap_len, R_xlen_t record_size, R_xlen_t offset, SEXP smode, int swap) {
  R_xlen_t i, u;
  int *lgl_value;
  int32_t intbuf;
  div_t word;
  
  lgl_value = LOGICAL(value);
  if(strcmp(SMODE_CTYPE(smode), "bitset") == 0) {  /* bitset */
    for(i = 0; i < LEN; i++) {
      u = (R_xlen_t)index_p[i] - 1;
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
        u = (R_xlen_t)index_p[i] - 1;
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
        u = (R_xlen_t)index_p[i] - 1;
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

void integer_replace(unsigned char *data, SEXP value, R_xlen_t LEN, double *index_p, R_xlen_t mmap_len, R_xlen_t record_size, R_xlen_t offset, SEXP smode, int swap) {
  R_xlen_t i, u;
  int *int_value;
  int16_t sbuf;
  int32_t intbuf;
  
  int_value = INTEGER(value);
  switch(SMODE_CBYTES(smode)) {
  case 1:
    if(SMODE_SIGNED(smode)) { /* int8 */
      for(i = 0; i < LEN; i++) {
        u = (R_xlen_t)index_p[i] - 1;
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
        u = (R_xlen_t)index_p[i] - 1;
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
        u = (R_xlen_t)index_p[i] - 1;
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
        u = (R_xlen_t)index_p[i] - 1;
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
      u = (R_xlen_t)index_p[i] - 1;
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

void double_replace(unsigned char *data, SEXP value, R_xlen_t LEN, double *index_p, R_xlen_t mmap_len, R_xlen_t record_size, R_xlen_t offset, SEXP smode, int swap) {
  R_xlen_t i, u;
  double *real_value;
  int64_t longbuf;
  float floatbuf;
  double realbuf;
  
  real_value = REAL(value);
  switch(SMODE_CBYTES(smode)) {
  case 4: /* real32 */
    for(i = 0; i < LEN; i++) {
      u = (R_xlen_t)index_p[i] - 1;
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
        u = (R_xlen_t)index_p[i] - 1;
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
        u = (R_xlen_t)index_p[i] - 1;
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

void complex_replace(unsigned char *data, SEXP value, R_xlen_t LEN, double *index_p, R_xlen_t mmap_len, R_xlen_t record_size, R_xlen_t offset, int swap) {
  R_xlen_t i, u;
  Rcomplex *complex_value;
  double doublepairbuf[2];
  Rcomplex Rcomplexbuf;
  
  complex_value = COMPLEX(value);
  for(i = 0; i < LEN; i++) {
    u = (R_xlen_t)index_p[i] - 1;
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

void character_replace(unsigned char *data, SEXP value, R_xlen_t LEN, double *index_p, R_xlen_t mmap_len, R_xlen_t record_size, R_xlen_t offset, SEXP smode) {
  R_xlen_t i, u, fieldCbytes;
  R_len_t charsxp_len;
  int hasnul;
  
  fieldCbytes = SMODE_CBYTES(smode);
  hasnul = !!SMODE_NUL_TERM(smode);
  for(i = 0; i < LEN; i++) {
    u = (R_xlen_t)index_p[i] - 1;
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

void raw_replace(unsigned char *data, SEXP value, R_xlen_t LEN, double *index_p, R_xlen_t mmap_len, R_xlen_t record_size, R_xlen_t offset) {
  R_xlen_t i, u;
  Rbyte *raw_value;
  
  raw_value = RAW(value);
  for(i = 0; i < LEN; i++) {
    u = (R_xlen_t)index_p[i] - 1;
    if(u >= mmap_len || u < 0)
      error("'i=%i' out of bounds", u + 1);
    
    data[u * record_size + offset] = (Rbyte)raw_value[i];
  }
}

/* mmap_replace {{{ */
SEXP mmap_replace (SEXP index, SEXP field, SEXP value, SEXP mmap_obj, SEXP swap_byte_order) {
  R_xlen_t v, fi, offset;
  unsigned char *data;
  R_xlen_t LEN = xlength(index);
  SEXP vec_smode, smode = MMAP_SMODE(mmap_obj);
  SEXPTYPE mode = TYPEOF(smode);
  R_xlen_t Cbytes = SMODE_CBYTES(smode);
  R_xlen_t mmap_len = MMAP_SIZE(mmap_obj) / Cbytes; /* length.mmap() */
  int P = 0;
  SEXP vec_value;
  
  data = MMAP_DATA(mmap_obj);
  if(data == NULL)
    error("invalid mmap pointer");

  if(mode != VECSXP) {
    PROTECT(value = coerceVector(value, mode)); P++;
    if (xlength(value) != LEN)
      // Code on R side failed to properly handle the recycling.
      error("size of struct and size of replacement value do not match");
  }
  PROTECT(index = coerceVector(index, REALSXP)); P++;
  PROTECT(field = coerceVector(field, REALSXP)); P++;
  double *index_p = REAL(index);
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
    if(xlength(value) != xlength(field))
      // Code on R side failed to properly handle the recycling.
      error("size of struct and size of replacement value do not match");
    for(fi = 0; fi < xlength(field); fi++) {
      v = (R_xlen_t)REAL(field)[fi] - 1;
      offset = SMODE_OFFSET(smode, v);
      vec_smode = VECTOR_ELT(smode, v);
      vec_value = VECTOR_ELT(value, fi);
      if (xlength(vec_value) != LEN)
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
