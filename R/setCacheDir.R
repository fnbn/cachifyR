#' Set cache directory of a cachified function
#' @param f The function to set the cache directory from
#' @param cacheDir A directory where the results of the function are stored.
#' @export
setCacheDir <- function(f, cacheDir) {
  # convert to absolute path
  if (!grepl(x = cacheDir, pattern='^([A-Z]:)?/'))
    cacheDir <- gsub(pattern = '/+',
                     replacement = '/',
                     x = paste(getwd(), cacheDir, sep='/'))
  assign('cacheDir', cacheDir, environment(f))
  
  # initiate cache
  if (!dir.exists(cacheDir)) dir.create(cacheDir, recursive = TRUE)
  
  invisible(NULL)
}
