groupCenter <- function(var, group, centerFn=mean, ...) {

  #' Group Centering Function
  #'
  #' This function subtracts the value returned by \code{centerFn} for each
  #' unique \code{group} from \code{var}, and returns the modified vector.
  #'
  #' @param var A vector of values.
  #' @param group A group identifier variable containing a unique value for each
  #'   group.
  #' @param centerFn A function object used to compute the value to subtract
  #'   from each score. It should accept a vector (passed by \code{tapply}) and
  #'   return a single value.

  varC <- var - tapply(var, group, centerFn, ...)[group]
  return(varC)
}
