#' Lock a cache; whenever the function tries to evaluate rather than read from
#' the cache, it will fail with an error. Useful, if you want to ensure
#' reproduction of former execution
#' @param f The function to lock the cache from
#' @export
lockCache <- function(f) {
  if (!isCacheLocked(f))
    file.create(paste0(getCacheDir(f), '/locked'))
  invisible(NULL)
}
