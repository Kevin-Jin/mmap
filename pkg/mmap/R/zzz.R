.onLoad <- function(lib, pkg) {
  .Call("mmap_init_globals")
}
