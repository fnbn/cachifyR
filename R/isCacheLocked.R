#' Is the cache of a function locked?
#' @seealso \code{\link{lockCache}}
#' @param f The function to inform about its cache status
#' @export
isCacheLocked <- function(f) {
  file.exists(paste0(getCacheDir(f), '/locked'))
}
