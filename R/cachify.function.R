#' cachify turns a function into a caching function
#' @param f The function to cachify
#' @param debug Set to TRUE to receive info about cache retrieval vs. evaluation
#' @param cacheDir A directory where the results of the function are stored.
#' @return The cachified function. Cachifistic!
#' @import digest
#' @S3method cachify function
cachify.function <- function(f, cacheDir, debug=FALSE) {
  
  # internal functions
  
  # is the hash cashed already?
  isCached <- function(hash) {
    paste0(hash, '.Rds') %in% dir(cacheDir, pattern='^[a-f0-9]{32}.Rds$')
  }
  
  # write to cache
  writeToCache <- function(hash, args, result) {
    saveRDS(object = list(args=args, res=result),
            file = paste0(cacheDir, '/', hash, '.Rds'))
  }
  
  # read from cache (returns only the result, not the arguments)
  readFromCache <- function(hash) {
    readRDS(paste0(cacheDir, '/', hash, '.Rds'))$res
  }
  
  # is cache locked?
  cacheIsLocked <- function() {
    file.exists(paste0(cacheDir, '/locked'))
  }
  
  if (!'cachified' %in% names(attributes(f))) {
    # get argument list
    args <- formals(f)
    
    # convert to absolute path
    if (!grepl(x = cacheDir, pattern='^([A-Z]:)?/'))
      cacheDir <- gsub(pattern = '/+',
                       replacement = '/',
                       x = paste(getwd(), cacheDir, sep='/'))
    
    # initiate cache
    if (!dir.exists(cacheDir)) dir.create(cacheDir, recursive = TRUE)
    
    # hash over function
    function_hash <- digest(deparse(f))
    
    # define new function
    if (length(args) == 0) { # function with no args
      newfun <- function() {
        # hash arguments and function
        hash <- digest(list(function_hash, list()))
        
        if(isCached(hash)) {
          res <- readFromCache(hash)
        } else {
          if(cacheIsLocked()) {
            stop('Cache is locked and function needs evaluation')
          } else {
            res <- f()
            writeToCache(hash, list(), res)
          }
        }
        
        return(res)
      }
    } else { # function with > 0 args
      newfun <- function() {
        # store args in list
        eval_args <- lapply(as.list(match.call()),
                            function(e) eval(e, envir=parent.frame(3)))
        
        # hash arguments and function
        hash <- digest(list(function_hash, eval_args[2:length(eval_args)]))
        
        if(isCached(hash)) {
          res <- readFromCache(hash)
        } else {
          if(cacheIsLocked()) {
            stop('Cache is locked and function needs to be evaluated')
          } else {
            res <- do.call(f, eval_args[2:length(eval_args)])
            writeToCache(hash, eval_args[2:length(eval_args)], res)
          }
        }
        return(res)
      }
    }
    
    # set formals of new function
    formals(newfun) <- args
    attributes(newfun)$cachified <- TRUE
    return(newfun)
  } else {
    warning('Function is already cachified! Returning input function.')
    return(f)
  }
}