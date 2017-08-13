#' cachify turns a function into a caching function
#' @param f name of the function to cachify (character)
#' @param debug Set to TRUE to receive info about cache retrieval vs. evaluation
#' @param cacheDir A directory where the results of the function are stored.
#' @return No return. The original function with name \code{f} is replaced by the cachified version
#' @details Will always assign the new function to \code{.GlobalEnv}, 
#'          no matter where the original binding of the function was.
#' @export
cachify.character <- function(f, cacheDir, debug=FALSE) {
  
  if(!exists(f)) stop(sprintf("'%s' is not a valid function name.", f))

  replaceFunctionByReference(name     = f,
                             newFunc  = cachify(f        = get(f, pos = parent.frame(n = 1)), 
                                                cacheDir = cacheDir, 
                                                debug    = debug), 
                             startEnv = parent.frame(n = 1))
  
  return(invisible(NULL))
  
}
