#' Remove caching functionality from function
#' @param f The function to remove caching functionality from
#' @return The uncachified function
#' @export
uncachify <- function(f) {
  if ('cachified' %in% names(attributes(f))) {
    return(get('f', environment(f)))
  } else {
    warning('Function is not cachified! Returning input function.')
    return(f)
  }
}
