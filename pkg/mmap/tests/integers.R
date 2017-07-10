library(mmap)
tmp <- tempfile()

##### int8() #####
# write binary from min to max signed 2^8
test.int8 <- function() {
  writeBin(-127:127L, tmp, size=1)
  
  m <- mmap(int8(), tmp)  # signed 1 byte integers
  if(!all(m[] == (-127:127L)) )
    stop("m[] == (-127:127L)")
  
  # test replacement
  m[] <- -127L
  if(!all(m[] == -127))
    stop("m[] == -127")
  munmap(m)
}




#### uint8() ####
test.uint8 <- function(on=TRUE) {
  if(!isTRUE(on)) {
    cat("test.uint8 disabled\n")
    return(NULL)
  }
  writeBin(0:254L, tmp, size=1)
  m <- mmap(uint8(), tmp)  # unsigned 1 byte integers
  if(!all(m[] == 0:254L) )
    stop("m[] == 0:254L")
  
  # test replacement
  m[] <- 254L;
  if(!all(m[] == 254L))
    stop("m[] == 254L")
  munmap(m)
}




#### int16() ####
test.int16 <- function(on=TRUE) {
  if(!isTRUE(on)) {
    cat("test.int16 disabled\n")
    return(NULL)
  }
  writeBin(-32767:32767L, tmp, size=2)
  m <- mmap(int16(endian="platform"), tmp)  # signed 2 byte integers
  if(!all(m[] == -32767:32767L) )
    stop("m[] == -32767:32767L")
  
  # test replacement
  m[] <- -32767L
  if(!all(m[] == -32767L))
    stop("m[] == -32767L")
  munmap(m)
}




#### uint16() ####
test.uint16 <- function(on=TRUE) {
  cat("checking test.uint16...")
  if(!isTRUE(on)) {
    cat("test.uint16 disabled\n")
    return(NULL)
  }
  writeBin(0:65534L, tmp, size=2)
  m <- mmap(uint16(endian="platform"), tmp)  # unsigned 2 byte integers
  if(!all(m[] == 0:65534L) )
    stop("m[] == 0:65534L")
  
  # test replacement
  m[] <- 65534L
  if(!all(m[] == 65534L))
    stop("m[] == 65534L")
  munmap(m)
  cat("OK\n")
}




#### int32() ####
test.int32 <- function(on=TRUE) {
  cat("checking test.int32...")
  if(!isTRUE(on)) {
    cat("test.int32 disabled\n")
    return(NULL)
  }
  writeBin(-1e6:1e6L, tmp, size=4)
  m <- mmap(int32(endian="platform"), tmp)  # signed 4 byte integers
  if(!all(m[] == -1e6:1e6L) )
    stop("m[] == -1e6:1e6L")
  
  # test replacement
  m[] <- .Machine$integer.max
  if(!all(m[] == .Machine$integer.max))
    stop("m[] == .Machine$integer.max")
  munmap(m)
  cat("OK\n")
}




#### int64() ####
test.int64 <- function(on=TRUE) {
  cat("checking test.int64...")
  if(!isTRUE(on)) {
    cat("test.int32 disabled\n")
    return(NULL)
  }
  writeBin(0.0, tmp)
  m <- mmap(int64(endian="platform"), tmp)  # signed 8 byte integers as doubles
  m[] <- 2^40
  if(!all(m[] == 2^40) )
    stop("m[] == 2^40")
  munmap(m)
  cat("OK\n")
}

test.int8()
test.uint8()
test.int16()
test.uint16()
test.int32()
test.int64(FALSE)
