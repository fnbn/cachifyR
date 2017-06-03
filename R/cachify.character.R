#' cachify turns a function into a caching function
#' @param f name of the function to cachify
#' @param debug Set to TRUE to receive info about cache retrieval vs. evaluation
#' @param cacheDir A directory where the results of the function are stored.
#' @return No return. The original function with name \code{f} is replaced by the cachified version
#' @S3method cachify character
cachify.character <- function(f, cacheDir, debug=FALSE) {
  
  assign(x     = f, 
         value = cachify(f = get(f), cacheDir = cacheDir),
         envir = environment(get(f)))
  
  return(invisible(NULL))
}
