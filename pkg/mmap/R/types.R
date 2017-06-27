# C_types
sizeofCtypes <- function() {
  structure(.Call("sizeof_Ctypes"), 
            .Names=c("char","short","int","long","float","double"))
}

as.Ctype <- function(x, ...) {
  UseMethod("as.Ctype")
}

is.Ctype <- function(x) inherits(x, "Ctype")

as.Ctype.Ctype <- function(x, ...) x

as.Ctype.integer <- function(x, ...) {
  int32(...)
}
as.Ctype.double <- function(x, ...) {
  if( isTRUE(attr(x, "Csingle")))
    real32(...)
  else
    real64(...)
}
as.Ctype.raw <- function(x, ...) {
  uchar(...)
}
as.Ctype.character <- function(x, ...) {
  string(sample=x, ...)
}
as.Ctype.complex <- function(x, ...) {
  cplx(...)
}
as.Ctype.logical <- function(x, ...) {
  bool32(...)
}
as.Ctype.NULL <- function(x, ...) {
  pad(...)
}

get.Ctype <- function(x, ...) UseMethod("get.Ctype")

get.Ctype.Ctype <- function(x, ...) class(x)[1]

normalize.endianness <- function(endian)
  switch(endian, big="big", little="little", platform=.Platform$endian,
         swap=if (.Platform$endian == "little") "big" else "little")

string <- function(length=NULL, enc=NULL, nul=TRUE, sample=NULL) {
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
  } else {
    if (is.null(enc))
      enc <- "latin1"
    if (is.null(length))
      length <- 0
  }
  structure(character(0), bytes=as.integer(max(length,1)+!!nul),
            enc=enc, nul=nul, class=c("string","Ctype"))
}
as.string <- function(x, ...) UseMethod("as.string")
as.string.mmap <- function(x, ...) {
  x$storage.mode <- string(...)
  x
}

char <- function() {
  structure(raw(0), bytes=1L, signed=1L, class=c("char","Ctype"))
}
as.char <- function(x, ...) UseMethod("as.char")
as.char.mmap <- function(x, ...) {
  x$storage.mode <- char(...)
  x 
}

uchar <- function() {
  structure(raw(0), bytes=1L, signed=0L, class=c("uchar","Ctype"))
}
as.uchar <- function(x, ...) UseMethod("as.uchar")
as.uchar.mmap <- function(x, ...) {
  x$storage.mode <- uchar(...)
  x 
}

int8 <- function() {
  structure(integer(0), bytes=1L, signed=1L, class=c("int8","Ctype"))
}
as.int8 <- function(x, ...) UseMethod("as.int8")
as.int8.mmap <- function(x, ...) {
  x$storage.mode <- int8(...)
  x
}

uint8 <- function() {
  structure(integer(0), bytes=1L, signed=0L, class=c("uint8","Ctype"))
}
as.uint8 <- function(x, ...) UseMethod("as.uint8")
as.uint8.mmap <- function(x, ...) {
  x$storage.mode <- uint8(...)
  x
}

int16 <- function(endian=c("big", "little", "swap", "platform")) {
  endian <- normalize.endianness(match.arg(endian))
  structure(integer(0), bytes=2L, signed=1L, endian=endian, class=c("int16","Ctype"))
}
as.int16 <- function(x, ...) UseMethod("as.int16")
as.int16.mmap <- function(x, ...) {
  x$storage.mode <- int16(...)
  x
}

uint16 <- function(endian=c("big", "little", "swap", "platform")) {
  endian <- normalize.endianness(match.arg(endian))
  structure(integer(0), bytes=2L, signed=0L, endian=endian, class=c("uint16","Ctype"))
}
as.uint16 <- function(x, ...) UseMethod("as.uint16")
as.uint16.mmap <- function(x, ...) {
  x$storage.mode <- uint16(...)
  x
}

int32 <- function(endian=c("big", "little", "swap", "platform")) {
  endian <- normalize.endianness(match.arg(endian))
  structure(integer(0), bytes=4L, signed=1L, endian=endian, class=c("int32","Ctype"))
}

uint32 <- function(endian=c("big", "little", "swap", "platform")) {
  endian <- normalize.endianness(match.arg(endian))
  structure(integer(0), bytes=4L, signed=0L, endian=endian, class=c("uint32","Ctype"))
}

int64 <- function(endian=c("big", "little", "swap", "platform")) {
  endian <- normalize.endianness(match.arg(endian))
  # currently untested and experimental. Will lose precision in R though we cast
  # to a double precision float to minimize the damage
  if(.Machine$sizeof.long != 8)
    warning("unsupported int64, use int32 or real64")
  structure(double(0), bytes=as.integer(.Machine$sizeof.long), signed=1L, endian=endian, class=c("int64","Ctype"))
}

real32 <- function(endian=c("big", "little", "swap", "platform")) { 
  endian <- normalize.endianness(match.arg(endian))
  structure(double(0), bytes=4L, endian=endian, class=c("real32","Ctype"))
}

real64 <- function(endian=c("big", "little", "swap", "platform")) { 
  endian <- normalize.endianness(match.arg(endian))
  structure(double(0), bytes=8L, endian=endian, class=c("real64","Ctype"))
}

cplx <- function(endian=c("big", "little", "swap", "platform")) {
  endian <- normalize.endianness(match.arg(endian))
  structure(complex(0), bytes=16L, endian=endian, class=c("cplx","Ctype"))
}

bitset <- function(endian=c("big", "little", "swap", "platform")) {
  endian <- normalize.endianness(match.arg(endian))
  structure(logical(0), bytes=4L, endian=endian, class=c("bitset","Ctype"))
}

bool32 <- function(endian=c("big", "little", "swap", "platform")) {
  endian <- normalize.endianness(match.arg(endian))
  structure(logical(0), bytes=4L, endian=endian, class=c("bool32","Ctype"))
}

bool8 <- function() {
  structure(logical(0), bytes=1L, class=c("bool8","Ctype"))
}

pad <- function(...) {
  UseMethod("pad")
}

pad.default <- function(length=0L, ...) {
  structure(NA_integer_, bytes=as.integer(length), class=c("pad","Ctype"))
}

pad.Ctype <- function(ctype, ...) {
  pad(sizeof(ctype))
}

struct <- function (..., bytes, offset) {
    dots <- lapply(list(...), as.Ctype)
    bytes_ <- sapply(dots, sizeof)
    if (missing(offset)) 
      offset <- cumsum(bytes_) - bytes_
    if (!missing(bytes)) 
      bytes_ <- bytes
    padding <- which(sapply(dots, get.Ctype)=="pad")
    if( length(padding) > 0) {
      dots <- dots[-padding]
      offset <- offset[-padding]
    }
    structure(dots, bytes=as.integer(sum(bytes_)), offset=as.integer(offset), 
              class=c("struct","Ctype"))
}

as.list.Ctype <- struct


`[[<-.struct` <- function(x,i,value) {
  x <- unclass(x)
  x[[i]] <- as.Ctype(value)
  do.call(struct,x)
}

print.Ctype <- function(x, indent = "", ...) {
  .class <- get.Ctype(x)
  if(.class == "struct") {
    cat(paste(indent,"struct:\n",sep=""))
    for(i in 1:length(x))
      print.Ctype(x[[i]], paste(indent, "  ", sep = ""))
  } else {
    cat(paste(indent,"(",.class,") ",sep=""))
    attributes(x) <- NULL
    if(length(x)==0) {
      cat(paste(typeof(x),"(0)\n",sep=""))
    } else if(.class=="string") {
      cat(paste("character(",length(x),")\n",sep=""))
    } else {
      cat(x,"\n")
    }
  }
}

is.struct <- function(x) {
  inherits(x, "struct")
}

as.Ctype.default <- function(x, ...) {
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
    stop(paste("Vector of class '", class(x)[1], "' is unsupported", sep = ""))
  else if (is.recursive(x))
    # Multiple fields.
    do.call(struct, c(lapply(x, as.Ctype), list(...)))
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
