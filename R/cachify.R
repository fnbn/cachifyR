#' cachify turns a function into a caching function
#' @param f The function to cachify (either name or function)
#' @param debug Set to TRUE to receive info about cache retrieval vs. evaluation
#' @param cacheDir A directory where the results of the function are stored.
#' @return The cachified function. Cachifistic!
#' @export
cachify <- function(f, cacheDir, debug=FALSE) {
  
  UseMethod("cachify", object = f)

}
