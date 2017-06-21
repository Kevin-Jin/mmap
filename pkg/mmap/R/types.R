# C_types
sizeofCtypes <- function() {
  structure(.Call("sizeof_Ctypes"), 
            .Names=c("char","short","int","long","float","double"))
}

as.Ctype <- function(x) {
  UseMethod("as.Ctype")
}

is.Ctype <- function(x) inherits(x, "Ctype")

as.Ctype.Ctype <- identity

as.Ctype.integer <- function(x) {
  if(length(x) == 1 && x==0)
    int32()
  else int32(length(x))
}
as.Ctype.double <- function(x) {
  csingle <- attr(x, "Csingle")
  if( !is.null(csingle) && isTRUE(csingle)) {
    if(length(x) == 1 && x==0)
      real32()
    else
      real32(length(x))
  } else {
    if(length(x) == 1 && x==0)
      real64()
    else real64(length(x))
  }
}
as.Ctype.raw <- function(x) {
  if(length(x) == 1 && x==0)
    uchar()
  else uchar(length(x))
}
as.Ctype.character <- function(x) {
  char(sample=x)
}
as.Ctype.complex <- function(x) {
  if(length(x) == 1 && x==0)
    cplx()
  else cplx(length(x))
}
as.Ctype.logical <- function(x) {
  if(length(x) == 1 && x==0)
    logi32()
  else logi32(length(x))
}

char <- C_char <- function(length=NULL, enc=NULL, nul=TRUE, sample=NULL) {
  if (length(sample) > 0) {
    if (is.null(enc)) {
      enc <- unique(Encoding(sample))
      enc <- enc[enc != "unknown"]
      if (length(enc) == 0)
        # If none of the strings in the sample have codepoints above 0x7F,
        #  then assign a single-byte encoding.
        enc <- "latin1"
      else if (length(enc) > 1)
        # If the sample mixes multiple encodings (probably UTF-8 and latin1),
        #  then assign "UTF-8" so that all characters can be represented.
        enc <- "UTF-8"
    }
    if (is.null(length)) {
      # Without this step, the number of bytes each string takes up
      #  can be understated e.g. we reencode latin1 strings containing
      #  codepoints above 0x7F as UTF-8.
      sample <- normalize.encoding(sample, enc)
      length <- max(nchar(sample[!is.na(sample)], type = "bytes"))
    }
  } else if (!is.null(sample)) {
    enc <- "latin1"
    length <- 0
  }
  if(is.null(length)) { # a char byte
    structure(raw(0), bytes=1L, signed=1L, class=c("Ctype","char"))
  } else {
    structure(character(max(length,1)+!!nul), bytes=as.integer(max(length,1)+!!nul), signed=0L,
              enc=enc, nul=nul, class=c("Ctype","char","string"))
  }
}

as.char <- function(x, ...) UseMethod("as.char")
as.char.mmap <- function(x, length, ...) {
  x$storage.mode <- char(length)
  x 
}

uchar <- C_uchar <- function(length=0) {
  # unsigned 1 byte char
  structure(raw(length), bytes=1L, signed=0L, class=c("Ctype","uchar"))
}

as.uchar <- function(x, ...) UseMethod("as.uchar")
as.uchar.mmap <- function(x, length, ...) {
  x$storage.mode <- uchar(length)
  x 
}

int8 <- function(length=0) {
  # signed 1 byte int
  structure(integer(length), bytes=1L, signed=1L, class=c("Ctype","char"))
}
as.int8 <- function(x, length, ...) UseMethod("as.int8")
as.int8.mmap <- function(x, length=0, ...) {
  x$storage.mode <- int8(length)
  x
}

uint8 <- function(length=0) {
  # unsigned 1 byte int
  structure(integer(length), bytes=1L, signed=0L, class=c("Ctype","uchar"))
}
as.uint8 <- function(x, length, ...) UseMethod("as.uint8")
as.uint8.mmap <- function(x, length=0, ...) {
  x$storage.mode <- uint8(length)
  x
}

int16 <- C_short <- function(length=0) {
  structure(integer(length), bytes=2L, signed=1L, class=c("Ctype","short"))
}
as.int16 <- function(x, length, ...) UseMethod("as.int16")
as.int16.mmap <- function(x, length=0, ...) {
  x$storage.mode <- int16(length)
  x
}

uint16 <- C_ushort <- function(length=0) {
  structure(integer(length), bytes=2L, signed=0L, class=c("Ctype","ushort"))
}
as.uint16 <- function(x, length, ...) UseMethod("as.uint16")
as.uint16.mmap <- function(x, length=0, ...) {
  x$storage.mode <- uint16(length)
  x
}

int24 <- C_int24 <- function(length=0) {
  structure(integer(length), bytes=3L, signed=1L, class=c("Ctype","int24"))
}
as.int24 <- function(x, length, ...) UseMethod("as.int24")
as.int24.mmap <- function(x, length=0, ...) {
  x$storage.mode <- int24(length)
  x
}

uint24 <- C_uint24 <- function(length=0) {
  structure(integer(length), bytes=3L, signed=0L, class=c("Ctype","uint24"))
}

int32 <- C_int <- function(length=0) {
  structure(integer(length), bytes=4L, signed=1L, class=c("Ctype","int"))
}

int64 <- C_int64 <- function(length=0) {
  # currently untested and experimental. Will lose precision in R though we cast
  # to a double precision float to minimize the damage
  if(.Machine$sizeof.long != 8)
    warning("unsupported int64, use int32 or real64")
  structure(double(length), bytes=as.integer(.Machine$sizeof.long), signed=1L, class=c("Ctype","int64"))
}

uint32 <- C_uint <- function(length=0) {
  structure(integer(length), bytes=4L, signed=0L, class=c("Ctype","uint"))
}

real32 <- C_float <- function(length=0) { 
  structure(double(length),  bytes=4L, signed=1L, class=c("Ctype","float"))
}

real64 <- C_double <- function(length=0) { 
  structure(double(length),  bytes=8L, signed=1L, class=c("Ctype","double"))
}

cplx <- C_complex <- function(length=0) {
  structure(complex(length),  bytes=16L, signed=1L, class=c("Ctype","complex"))
}

bits <- C_bits <- function(length=0) {
  structure(logical(length), bytes=4L, signed=0L, class=c("Ctype", "bits"))
}

logi32 <- C_logi <- function(length=0) {
  structure(logical(length), bytes=4L, signed=0L, class=c("Ctype", "logi32"))
}

logi8 <- C_logi <- function(length=0) {
  structure(logical(length), bytes=1L, signed=0L, class=c("Ctype", "logi8"))
}

pad <- C_pad <- function(...) {
  UseMethod("pad")
}

pad.default <- function(length=0, ...) {
  structure(NA_integer_, bytes=length, class=c("Ctype", "pad"))
}

pad.Ctype <- function(ctype, ...) {
  pad(sizeof(ctype))
}

.struct <- function (..., bytes, offset) {
    dots <- lapply(list(...), as.Ctype)
    if( missing(bytes))
      bytes <- sapply(dots, sizeof)
    if( missing(offset))
      offset <- cumsum(bytes) - bytes
    structure(dots, bytes = as.integer(sum(bytes)), 
                    offset = as.integer(offset), 
                    signed = NA, 
              class = c("Ctype", "struct"))
}

struct <- function (..., bytes, offset) {
    dots <- lapply(list(...), as.Ctype)
    bytes_ <- sapply(dots, sizeof)
    if (missing(offset)) 
      offset <- cumsum(bytes_) - bytes_
    if (!missing(bytes)) 
      bytes_ <- bytes
    padding <- which(sapply(dots, function(C) class(C)[2])=="pad")
    if( length(padding) > 0) {
      dots <- dots[-padding]
      offset <- offset[-padding]
    }
    structure(dots, bytes = as.integer(sum(bytes_)), 
                    offset = as.integer(offset), 
                    signed = NA, 
              class = c("Ctype", "struct"))
}

as.list.Ctype <- struct


`[[<-.struct` <- function(x,i,value) {
  x <- unclass(x)
  x[[i]] <- as.Ctype(value)
  do.call(struct,x)
}

print.Ctype <- function(x, ...) {
  if(class(x)[2] == "struct") {
    cat("struct:\n")
    for(i in 1:length(x)) {
    cat(paste("  (",class(x[[i]])[2],") ",sep=""))
    attributes(x[[i]]) <- NULL
    if(length(x[[i]])==0)
      cat(paste(typeof(x[[i]]),"(0)\n",sep=""))
    else
    cat(x[[i]],"\n")
    }
  } else {
    cat(paste("(",class(x)[2],") ",sep=""))
    .class <- class(x)[2]
    attributes(x) <- NULL
    if(length(x)==0) {
      cat(paste(typeof(x),"(0)\n",sep=""))
    } else
    if(.class=="char") {
      cat(paste("character(",length(x),")\n",sep=""))
    } else
    cat(x,"\n")
  }
}

is.struct <- function(x) {
  inherits(x, "struct")
}

as.struct <- function(x, ...) {
  UseMethod("as.struct")
}

as.struct.struct <- function(x, ...) x

as.struct.default <- function(x, ...) {
  # According to src/main/util.c#TypeTable, src/main/coerce.c#do_is, and src/include/Rinlinedfuns.h#isFunction,
  #  some functions of interest are implemented as:
  #   is.atomic(x)    <- typeof(x) %in% c("NULL", "char", "logical", "integer", "double", "complex", "character", "raw")
  #   is.language(x)  <- typeof(x) %in% c("symbol" (name), "language" (call), "expression")
  #   is.function(x)  <- typeof(x) %in% c("closure", "builtin", "special")
  #   is.recursive(x) <- typeof(x) %in% c("list", "pairlist", "environment") || is.language(x) && typeof(x) != "symbol" || is.function(x) || typeof(x) %in% c("promise", "...", "any")
  # The remaining types not yet mentioned are "externalptr", "bytecode", "weakref", and "S4".
  # Technically, "language", "expression", "closure", "builtin", "special", "promise", "...", "any" are basically lists, mostly
  #  of symbols, so `lapply(x, as.Ctype)` would not fail with a cryptic error message. The error would arise once as.Ctype()
  #  is called on a type that is neither atomic nor recursive, i.e. "symbol", "externalptr", "bytecode", "weakref", or "S4".
  # Since the error message will still be legible in the end, it is acceptable to just use the condition `is.recursive(x)` for simplicity.
  if (is.atomic(x))
    # Single field.
    struct(as.Ctype(x))
  else if (is.recursive(x))
    # Multiple fields.
    do.call(struct, lapply(x, as.Ctype))
  else
    # stopifnot(typeof(x) %in% c("symbol", "externalptr", "bytecode", "weakref", "S4"))
    stop(paste("Low-level language type '", typeof(x), "' is unsupported", sep = ""))
}

##  sizeof analogous to sizeof() in C.  Allows for
##  passing either Ctype objects, or functions that
##  construct Ctype objects.
##    e.g. sizeof(int8) or sizeof(int8())

sizeof <- function(type) UseMethod("sizeof")

sizeof.function <- function(type) {
  type_name <- deparse(substitute(type))
  type <- try(as.Ctype(type()), silent=TRUE)
  if( is.Ctype(type))
    sizeof(type)
  else
    stop(paste("can't find 'sizeof'",type_name))
}

sizeof.Ctype <- function(x) attr(x, "bytes")

sizeof.mmap <- function(x) x$bytes

sizeof.default <- function(type) {
  ty <- try( as.Ctype(type), silent=TRUE)
  if( is.Ctype(ty))
    sizeof(ty)
  else
    stop("unsupported type")
}
