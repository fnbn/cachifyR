#' @title cachifyPackage
#' @description cachifies all functions that are exported by a package
#' @param packageName name of the package
#' @param cacheDir A directory where the results of the function are stored.
#' @return Nothing; all functions in the package are cachified\cr
#' @importFrom utils lsf.str
#' @export
cachifyPackage <- function(packageName, cacheDir) {
  
  functions <- lsf.str(paste0("package:", packageName))
  startEnv  <- as.environment(paste0("package:", packageName))
  
  for(funName in functions){
    cachify(f        = funName, 
            cacheDir = cacheDir)
  }
  
  return(invisible(NULL))
  
}