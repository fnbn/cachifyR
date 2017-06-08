#' Replaces a function with a new value, taking care of 
#' consistent placement in the search path
#' @param name character; name of the function to replace
#' @param newFunc function; new value
#' @param startEnv environment; where to start looking for 'name'
#' @return no return; the first instance of 'name' on the search path 
#'        will be replaced by newFunc.
#' @details Tries to consistently take care of locked environments
#'          by creating a "cachified" unlocked environment one position upstream in the search path
#'          and assigning the new function there. \cr
#'          Cachified functions need special care because they don't work without their
#'          enclosing environment. This is dealt with by leaving a cachified new function 
#'          in its enclosing environment and only setting the parent to 
#'          the enclosing environment of the original function
#' @importFrom pryr where
replaceFunctionByReference <- function(name, newFunc, startEnv = environment()){
  
  ## determine the location of the original function
  bindingEnv   <- where(name, startEnv)
  enclosingEnv <- environment(get(name, startEnv))
  
  ## take care of locked bindingEnv if necessary
  if(environmentIsLocked(bindingEnv) | 
     bindingIsLocked(name, bindingEnv)){
    
    ## get search path rank of bindingEnv
    bindingEnvName <- attr(bindingEnv, "name")
    
    if(is.null(bindingEnvName) | bindingEnvName == "") {
      stop("Original function is bound to an unnamed locked environment. 
           cachify by reference does not work here")
    }
    
    searchPos <- match(bindingEnvName, search())[1]
    
    if(is.na(searchPos)) {
      stop("binding environment of original function can't be identified in the search path.
           cachify by reference does not work here")
    }
    
    ## check if a cachify version of the binding environment already exists:
    ## it should be one position upstream in the search path and have the name 
    ## [nameOfBindingEnv]_cachified
    ## if it doesn't exist, create it
    
    if(search()[max(searchPos - 1, 0)] != paste0(bindingEnvName, "_cachified")){
      attach(new.env(), pos = searchPos, name = paste0(bindingEnvName, "_cachified"))
    }
    bindingEnv <- as.environment(paste0(bindingEnvName, "_cachified"))
  }
  
  ## set the correct enclosing environment
  if('cachified' %in% names(attributes(newFunc))){
    
    ## if the function is cachified, 
    ## we need to keep the enclosing environment.
    ## Other functions that depend on objects in their environment
    ## will fail!
    parent.env(environment(newFunc)) <- enclosingEnv
    
  } else {
    
    environment(newFunc) <- enclosingEnv
    
  }
  
  ## do the real assignment
  assign(x        = name, 
         value    = newFunc, 
         pos      = bindingEnv, 
         inherits = FALSE)
  
  return(invisible(NULL))
  
}