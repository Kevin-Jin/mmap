#include <R.h>
#include <Rinternals.h>

#define MMAP_DATA(mmap_object)        R_ExternalPtrAddr(VECTOR_ELT(mmap_object,0))
#define MMAP_SIZE(mmap_object)        INTEGER(VECTOR_ELT(mmap_object,1))[0]
#define MMAP_FD(mmap_object)          INTEGER(VECTOR_ELT(mmap_object,2))[0]
#define MMAP_MODE(mmap_object)        TYPEOF(VECTOR_ELT(mmap_object,3))
#define MMAP_CBYTES(mmap_object)       INTEGER(getAttrib(VECTOR_ELT( \
                                      mmap_object,3),install("bytes")))[0]
#define MMAP_PAGESIZE(mmap_object)    INTEGER(VECTOR_ELT(mmap_object,3))[0]
#define MMAP_SYNC(mmap_object)        INTEGER(VECTOR_ELT(mmap_object,4))[0]

#define INT8_C          1 
#define INT16_C         2 
#define INT32_C         4 
#define REAL32_C        4 
#define REAL64_C        8 