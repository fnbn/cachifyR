#' cachify turns a function into a caching function
#' @param f name of the function to cachify (character)
#' @param debug Set to TRUE to receive info about cache retrieval vs. evaluation
#' @param cacheDir A directory where the results of the function are stored.
#' @return No return. The original function with name \code{f} is replaced by the cachified version
#' @details ATTENTION: will always assign the new function to \code{.GlobalEnv}, 
#'          no matter where the original binding of the function was.
#'          This can be changed by setting \code{pos = pryr::where(get(f))} in the assign statement.
#'          See http://adv-r.had.co.nz/Environments.html#function-envs.
#' @S3method cachify character
cachify.character <- function(f, cacheDir, debug=FALSE) {
  
  assign(x     = f, 
         value = cachify(f = get(f), cacheDir = cacheDir),
         pos   = .GlobalEnv)
  
  return(invisible(NULL))
}
