#' Unlock a cache
#' @seealso \code{\link{lockCache}}
#' param f The function to unlock the cache from
#' @export
unlockCache <- function(f) {
  if (isCacheLocked(f))
    file.remove(paste0(getCacheDir(f), '/locked'))
  invisible(NULL)
}
