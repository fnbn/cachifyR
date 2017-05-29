#' Get cache directory of a cachified function
#' @param f The function to return the cache directory from
#' @return The cache directory of the function
#' @export
getCacheDir <- function(f) {
  if (!('cachified' %in% names(attributes(f))))
    stop('Function is not cachified.')
  get('cacheDir', environment(f))
}
