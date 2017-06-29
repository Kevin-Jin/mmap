# S3 accessor methods
extractFUN <- function(x) {
  UseMethod("extractFUN")
}
`extractFUN<-` <- function(x, value) {
  UseMethod("extractFUN<-")
}
extractFUN.mmap <- function(x) {
  x$extractFUN
}
`extractFUN<-.mmap` <- function(x, value) {
  x$extractFUN <- value
  x
}
replaceFUN <- function(x) {
  UseMethod("replaceFUN")
}
`replaceFUN<-` <- function(x, value) {
  UseMethod("replaceFUN<-")
}
replaceFUN.mmap <- function(x) {
  x$replaceFUN
}
`replaceFUN<-.mmap` <- function(x, value) {
  x$replaceFUN <- value
  x
}

# Basic S3 methods
head.mmap <- function(x, n=6L, ...) {
  x[1:(min(NROW(x),n))]
}
tail.mmap <- function(x, n=6L, ...) {
  x[(NROW(x)-n):NROW(x)]
}

str.mmap <- function (object, ...) 
{
    print(object)
    cat("  data         :") 
    str(object$data)
    cat("  pageoff      :") 
    str(object$pageoff)
    cat("  bytes        :")
    str(object$bytes)
    cat("  filedesc     :")
    str(object$filedesc)
    cat("  storage.mode :") 
    str(object$storage.mode)
    cat("  dim          :")
    print(object$dim)
}

summary.mmap <- function(object) { str(object) }

print.mmap <- function(x, ...) {
  stopifnot(is.mmap(x))
  file_name <- names(x$filedesc)
  if(nchar(file_name) > 10)
    file_name <- paste(substring(file_name,0,10),"...",sep="")
  type_name <- switch(typeof(x$storage.mode),
                      "list"="struct",
                      "integer"="int",
                      "logical"="logi",
                      "double"="num",
                      "complex"="cplx",
                      "character"="chr",
                      "raw"="raw")
  if(type_name == "struct") {
    firstN <- x[1][[1]]
  } else {
  firstN <- head(x)
  firstN <- if(cumsum(nchar(firstN))[length(firstN)] > 20) {
                firstN[1:min(3,length(x))]
              } else {
                firstN
              }
  }
  if( !is.null(x$dim)) { # has dim
  cat(paste("<mmap:",file_name,">  (",get.Ctype(x$storage.mode),") ",
            type_name," [1:", nrow(x),", 1:", ncol(x),"]",sep=""),firstN,"...\n")
  } else {
  cat(paste("<mmap:",file_name,">  (",get.Ctype(x$storage.mode),") ",
            type_name," [1:", length(x),"]",sep=""),firstN,"...\n")
  }
}
print.summary_mmap <- function() {}

close.mmap <- function(con, ...) {
  munmap(con)
}

# creat flags using upper case symbols/strings
# mmapFlags(PROT_READ,PROT_WRITE) OR mmapFlags(PROT_READ | PROT_WRITE)
mmapFlags <- function(...) {
  flags <- as.character(match.call(call=sys.call())[-1])
  if(nargs()==1) {
    flags <- gsub(" ","",unlist(strsplit(flags,"\\|")))
    flags <- gsub('\"',"",flags) # in case "" | ""
  }
  .Call("mmap_mkFlags", flags, PKG="mmap")
}

# S3 constructor
mmap <- function(file, mode=int32(), 
                 extractFUN=NULL, replaceFUN=NULL,
                 prot=mmapFlags("PROT_READ","PROT_WRITE"),
                 flags=mmapFlags("MAP_SHARED"),advice=mmapFlags("MADV_NORMAL"),len,off=0L,
                 ...) {
    if(missing(file))
      stop("'file' must be specified")
    # pageoff is the offset from page boundary
    # off is the page-aligned offset
    #   e.g. off=22 would set off=0 and pageoff=22 on a system with 4096 page sizing
    pageoff <- off %% allocation.granularity()
    off <- off - pageoff
    if(missing(len))
      len <- file.info(file)$size - off - pageoff
    else
      len <- min(file.info(file)$size - off - pageoff, len)
    
    mmap_obj <- .Call("mmap_mmap", 
                      as.Ctype(mode),
                      file,
                      as.integer(prot), 
                      as.integer(flags), 
                      as.integer(advice), 
                      as.double(len),
                      as.double(off),
                      as.integer(pageoff),
                      PKG="mmap")
    reg.finalizer(mmap_obj, mmap_finalizer, TRUE)
    mmap_obj$filedesc <- structure(mmap_obj$filedesc, .Names=file)
    mmap_obj$extractFUN <- extractFUN
    mmap_obj$replaceFUN <- replaceFUN
    class(mmap_obj) <- "mmap"
    return(mmap_obj)
}

# S3 destructor
munmap <- function(x) {
  if(!is.mmap(x))
    stop("mmap object required to munmap")
  invisible(.Call("mmap_munmap", x, PKG="mmap"))
}

mmap_finalizer <- function(x) {
  if(is.mmap(x))
    invisible(.Call("mmap_munmap", x, PKG="mmap"))
}

msync <- function(x, flags=mmapFlags("MS_ASYNC")) {
  if(!is.mmap(x))
    stop("mmap object required to munmap")
  .Call("mmap_msync", x, as.integer(flags), PKG="mmap")
}

madvise <- function(x, i = NULL, advice) {
  if(!is.mmap(x))
    stop("mmap object required to munmap")
  .Call("mmap_madvise", x, i, as.integer(advice), PKG="mmap")
}

mprotect <- function(x, i = NULL, prot) {
  if(!is.mmap(x))
    stop("mmap object required to munmap")
  # # Assuming i is the sequence of targeted record numbers (1-indexed):
  # # Ranges of targeted memory offsets (relative to the address of the first page) can be found by
  # r <- cbind((i - 1) * sizeof(x$storage.mode) + x$pageoff, i * sizeof(x$storage.mode) + x$pageoff - 1)
  # # Ranges of affected page numbers (0-indexed) can be found by
  # s <- floor(r / pagesize()) # round to -Inf, not round to 0!
  # # Sequence of affected page numbers (0-indexed) can be found by
  # u <- unique(do.call(c, as.list(apply(s, 1, function(range) range[1]:range[2]))))
  # # Ranges of affected memory offsets (relative to the address of the first record) can be found by
  # v <- cbind(u * pagesize() - x$pageoff, (u + 1) * pagesize() - x$pageoff - 1)
  # # Ranges of affected record numbers (1-indexed) can be found by
  # w <- floor(v / sizeof(x$storage.mode)) + 1 # round to -Inf, not round to 0!
  # # Sequence of affected record numbers can be found by
  # z <- unique(do.call(c, as.list(apply(w, 1, function(range) range[1]:range[2]))))
  .Call("mmap_mprotect", x, i, as.integer(prot), PKG="mmap")
}

is.mmap <- function(x) {
  inherits(x, "mmap") && .Call("mmap_is_mmapped",x,PKG="mmap")
}

`[.mmap` <- function(x, i, j, ...) {
  if(sizeof(x) == 0) stop('no data to extract')
  if( is.struct(x$storage.mode) || is.null(x$dim)) {
    if(missing(i))
      i <- 1:length(x)
    if(missing(j))
      j <- 1:length(x$storage.mode)
    if(is.character(j))
      j <- match(j, names(x$storage.mode))
    DIM <- NULL
  } else {
    if( missing(i))
      i <- 1:dim(x)[1]
    if( missing(j))
      j <- 1:dim(x)[2]
    DIM <- c(length(i),length(j))
    i <- .Call("convert_ij_to_i", as.double(dim(x)[1]), as.integer(i), as.integer(j))
    j <- 1L
  }
  j <- j[j>0] # only positive values
  swap.byte.order <- FALSE
  if (is.struct(x$storage.mode)) {
    swap.byte.order <- logical(length(j))
    for (fi in 1:length(j))
      if (!inherits(x$storage.mode[[j[fi]]], "string") && sizeof(x$storage.mode[[j[fi]]]) > 1)
        swap.byte.order[fi] <- attr(x$storage.mode[[j[fi]]], "endian") != .Platform$endian
  } else if (!inherits(x$storage.mode, "string") && sizeof(x$storage.mode) > 1) {
    swap.byte.order <- attr(x$storage.mode, "endian") != .Platform$endian
  }
  xx <- .Call("mmap_extract", i, j, DIM, x, swap.byte.order, PKG="mmap")
  names(xx) <- names(x$storage.mode)[j]
  if (is.struct(x$storage.mode)) {
    for (fi in 1:length(j))
      if (inherits(x$storage.mode[[j[fi]]], "string"))
        Encoding(xx[[fi]]) <- attr(x$storage.mode[[j[fi]]], "enc")
  } else if (inherits(x$storage.mode, "string")) {
    Encoding(xx) <- attr(x$storage.mode, "enc")
  }
  if(is.null(extractFUN(x)))
    xx
  else
    as.function(extractFUN(x))(xx)
}

normalize.encoding <- function(x, to) {
  if (length(x) == 0)
    return(x)
  
  original.encoding <- Encoding(x)
  splitted <- split(x, original.encoding)
  splitted[names(splitted) != "unknown"] <- lapply(names(splitted)[names(splitted) != "unknown"], function(from)
    iconv(splitted[[from]], from, to))
  unsplit(splitted, original.encoding)
}

`[<-.mmap` <- function(x, i, j, ..., sync=TRUE, value) {
  # add type checking/coercing at the C-level
  if(sizeof(x) == 0) stop('no data to extract')
  if(is.struct(x$storage.mode) && !is.list(value))
    value <- list(value)
  if( is.struct(x$storage.mode) || is.null(x$dim)) {
    if(missing(i))
      i <- 1:length(x)
    if(missing(j))
      j <- 1:length(x$storage.mode)
    if(is.character(j))
      j <- match(j, names(x$storage.mode))
    if(is.list(value) && any(length(i) != lengths(value)))
      value <- lapply(value, rep, length.out=length(i))
    else if(!is.list(value) && length(i) != length(value))
      value <- rep(value, length.out=length(i))
  } else { # has dimension
    if(missing(i))
      i <- 1:dim(x)[1]
    if(missing(j))
      j <- 1:dim(x)[2]
    if(is.character(j))
      j <- match(j, names(x$dimnames))
    i <- .Call("convert_ij_to_i", as.double(dim(x)[1]), as.integer(i), as.integer(j))
    j <- 1L
    if(length(i) != length(value))
      value <- rep(value, length.out=length(i))
  }
# likely we need to check for list()/struct to correctly handle in C
  if(max(i) > length(x) || min(i) < 0)
    stop("improper 'i' range")
  swap.byte.order <- FALSE
  if (is.struct(x$storage.mode)) {
    if(length(j) != length(value))
      value <- rep(value, length.out=length(j))
    swap.byte.order <- logical(length(j))
    for (fi in 1:length(j))
      if (inherits(x$storage.mode[[j[fi]]], "string"))
        value[[fi]] <- normalize.encoding(as.character(value[[fi]]), attr(x$storage.mode[[j[fi]]], "enc"))
      else if (sizeof(x$storage.mode[[j[fi]]]) > 1)
        swap.byte.order[fi] <- attr(x$storage.mode[[j[fi]]], "endian") != .Platform$endian
  } else if (inherits(x$storage.mode, "string")) {
    value <- normalize.encoding(as.character(value), attr(x$storage.mode, "enc"))
  } else if (sizeof(x$storage.mode) > 1) {
    swap.byte.order <- attr(x$storage.mode, "endian") != .Platform$endian
  }
  .Call("mmap_replace", i, j, value, x, swap.byte.order, PKG="mmap") 
  if(sync)
    msync(x)
  x
}

length.mmap <- function(x) {
  size_in_bytes <- sizeof(x)
  size <- sizeof(x$storage.mode)
  if( get.Ctype(x$storage.mode) == "bitset")
    trunc(size_in_bytes/size) * 32L
  else
    trunc(size_in_bytes/size)
}

`length<-.mmap` <- function(x, value) {
  # should have some mechanism to sanity check length
  # as to not pass end of file
  size_in_bytes <- value * sizeof(x$storage.mode)
  if(file.info(names(x$filedesc))$size < size_in_bytes) {
    stop("cannot increase an mmap file's size") # implement something automatic here
  }
  x$bytes <- as.double(size_in_bytes)
  x 
}


# coerce to disk object and mmap back in.  Need
# to register a proper finalizer in the C code, else
# we are likely to end up with memory leaks.  For now
# this is not too probable, and not too dangerous.
# Famous last words ...

as.mmap <- function(x, mode, file,...) {
  UseMethod("as.mmap")
}

as.mmap.raw <- function(x, mode=as.Ctype(x, ...), file=tempmmap(), ...) {
  mode <- as.Ctype(mode)
  writeBin(x, file)
  mmap(file, mode)
}

as.mmap.logical <- function(x, mode=as.Ctype(x, ...), file=tempmmap(), ...) {
  mode <- as.Ctype(mode)
  nbytes <- sizeof(mode)
  
  if(get.Ctype(mode) == "bitset") {
    if(anyNA(x))
      warning("NA values treated as FALSE in coercion to bitset")
    
    # Bitstring of all 1s except for the most significant bit, which is 0.
    int.max.val <- .Machine$integer.max
    
    num.full.words <- length(x) %/% 32L
    if(num.full.words > 0) {
      # Subset the logical vector by full 32-bit words.
      words <- unlist(lapply(1:num.full.words - 1, function(word.num) {
        word <- sum(bitwShiftL(as.integer(!!x[1:31 + word.num * 32L]), 1:31 - 1), na.rm = TRUE)
        
        # `2^31` is interesting for two reasons: (1) it is R's representation
        #  for NA_integer_, and (2) it is a negative number in 32-bit two's 
        #  complement and R returns NA_integer_ upon overflow.
        # Thus, if we need to set `2^31` (the "sign bit"), we have to be clever.
        if(!x[32L + word.num * 32L]) {
          word
        } else if(word != int.max.val) {
          # Negate the entire word so that the sign bit is set to 1, and then
          #  negate again all bits besides the sign bit by exploiting the fact
          #  that `xor(FALSE, TRUE) == TRUE && xor(TRUE, TRUE) == FALSE`.
          bitwXor(bitwNot(word), int.max.val)
        } else {
          # Since `bitwNot(int.max.value) == NA_integer_`, we cannot use the
          #  above hack. But since toggling the sign bit of `int.max.value`
          #  results in the bitstring of all 1s, the result is trivial.
          bitwNot(0)
        }
      }))
    } else {
      words <- integer(0)
    }
    
    # Any remaining bits cannot make up a full word (or else they would've been
    #  handled above) so we don't have to worry about avoiding NAs here.
    rem <- length(x) %% 32L
    if(rem != 0)
      words <- c(words, sum(bitwShiftL(as.integer(!!tail(x, rem)), 1:rem - 1), na.rm = TRUE))
    
    x <- words
  }
  
  # writeBin() simply overflows, so we need to do some preprocessing.
  if(nbytes < 4L) {
    INT_MAX <- bitwShiftL(1L, nbytes * 8L) - 1L
    INT_MIN <- 0L
    NA_INT <- INT_MAX
    
    # Replace out-of-range values (they must be non-zero) with 1 and replace NAs
    #  with the representation of NA for the specific storage mode. Note that
    #  this is necessary because R internally stores logical values as integers.
    x <- as.integer(x)
    out.of.range <- (x == NA_INT | x < INT_MIN | x > INT_MAX)
    x[out.of.range] <- 1L
    x[is.na(x)] <- NA_INT
    x
  }
  
  if(nbytes > 1)
    writeBin(x, file, size=nbytes, endian=attr(mode, "endian"))
  else
    writeBin(x, file, size=nbytes)
  mmap(file, mode)
}

as.mmap.integer <- function(x, mode=as.Ctype(x, ...), file=tempmmap(), ...) {
  mode <- as.Ctype(mode)
  nbytes <- sizeof(mode)
  
  # writeBin() simply overflows, so we need to do some preprocessing.
  if(nbytes < 4L) {
    if(attr(mode, "signed")) {
      INT_MAX <- bitwShiftL(1L, nbytes * 8L - 1L) - 1L
      INT_MIN <- -(INT_MAX + 1L)
      NA_INT <- INT_MIN
    } else {
      INT_MAX <- bitwShiftL(1L, nbytes * 8L) - 1L
      INT_MIN <- 0L
      NA_INT <- INT_MAX
    }
    
    # Replace out-of-range values and NAs with the representation of NA for the
    #  specific storage mode. Display a warning if any values were out of range.
    out.of.range <- (x == NA_INT | x < INT_MIN | x > INT_MAX)
    if(any(out.of.range))
      warning(paste("NAs introduced by coercion to", get.Ctype(mode), "range"))
    x[out.of.range | is.na(x)] <- NA_INT
    x
  }
  
  if(nbytes > 1)
    writeBin(x, file, size=nbytes, endian=attr(mode, "endian"))
  else
    writeBin(x, file, size=nbytes)
  mmap(file, mode)
}

as.mmap.double <- function(x, mode=as.Ctype(x, ...), file=tempmmap(), ...) {
  mode <- as.Ctype(mode)
  nbytes <- sizeof(mode)
  writeBin(x, file, size=nbytes, endian=attr(mode, "endian"))
  result <- mmap(file, mode)
  
  # writeBin() simply strips NaN payloads, so we need to do some postprocessing.
  nas <- is.na(x) & !is.nan(x)
  if(nbytes < 8L && any(nas)) {
    # Since R doesn't have a 32-bit float type, and since any custom NaN payload
    #  injected into a double will be lost when the double is eventually casted
    #  to a float, we have to perform the logic in C code, i.e.`mmap_replace()`.
    result[which(nas)] <- NA
  }
  
  result
}

as.mmap.complex <- function(x, mode=as.Ctype(x, ...), file=tempmmap(), ...) {
  mode <- as.Ctype(mode)
  nbytes <- sizeof(mode)
  writeBin(x, file, size=nbytes, endian=attr(mode, "endian"))
  mmap(file, mode)
}

as.mmap.character <- function(x, mode=as.Ctype(x, ...), file=tempmmap(), ...) {
  mode <- as.Ctype(mode)
  payload.cap <- sizeof(mode) - 1
  
  # writeBin() doesn't support null-padded fixed-width strings and doesn't write
  #  NAs in the way we want it to, so we need to do some preprocessing.
  nas <- is.na(x)
  x[!nas] <- normalize.encoding(x[!nas], attr(mode, "enc"))
  under.over.lengths.span <- if (all(nas)) c(0, 0) else range(nchar(x[!nas], type = "bytes") - payload.cap)
  if (under.over.lengths.span[2] > 0)
    warning("Long strings were truncated")
  if (any(under.over.lengths.span != 0) || any(nas)) {
    x <- lapply(x, charToRaw)
    x[!nas] <- lapply(x[!nas], function(u) {
      under.over.length <- length(u) - payload.cap
      if (under.over.length <= 0)
        c(u, raw(1 - under.over.length))
      else if (under.over.length > 0)
        c(u[1:payload.cap], raw(1))
    })
    
    # Strings that start with as.raw(c(0x00, 0xff)) represent NA.
    na.representation <- raw(payload.cap + 1)
    na.representation[2] <- as.raw(0xff)
    x[nas] <- list(na.representation)
    
    x <- do.call(c, x)
  }
  
  writeBin(x, file, useBytes=TRUE)
  mmap(file, mode)
}

as.mmap.matrix <- function(x, mode=as.Ctype(x, ...), file=tempmmap(), ...) {
  DIM <- dim(x)
  dim(x) <- NULL
  x <- as.mmap(x, mode, file, ...)
  dim(x) <- DIM
  x
}

as.mmap.data.frame <- function(x, mode, file=tempmmap(), ...) {
  if( !missing(mode))
    warning("'mode' argument currently unsupported")
  mode <- as.Ctype(x, ...)
  writeBin(raw(sizeof(mode) * NROW(x)), file)
  m <- mmap(file, mode, extractFUN=as.data.frame)
  for(i in 1:NCOL(x)) {
    m[,i] <- x[,i]
  }
  m
}


tempmmap <- function(tmpdir=tempdir()) {
  tempfile("mmap",tmpdir)
}

pagesize <- function() {
  .Call("mmap_pagesize")
}

allocation.granularity <- function() {
  .Call("mmap_allocation_granularity")
}
