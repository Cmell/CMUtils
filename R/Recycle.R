recycle <- function (vec, length, na.fill = FALSE) 
{
  #'  Recycle a vector to the specified length.
  #'  
  #'  @param vec The vector to recycle.
  #'  @param length Either a vector whose length will be matched, or an integer
  #'  @param na.fill If TRUE, then the vector is simply filled with NAs to 
  #'  match `length`
  #'  
  #'  @export recycle
  #' 

  if (!is.vector(vec) & !is.vector(length)) 
    stop("vec and length must be vectors. length may also be an integer")
  if (!is.numeric(length) & is.vector(length)) 
    length <- length(length)
  if (is.vector(length) & length(length) > 1L) 
    length <- length(length)
  if (!na.fill) {
    newvec <- rep(vec, ceiling(length/length(vec)))
  }
  else {
    newvec <- c(vec, rep(NA, length * (ceiling(length/length(vec)) - 
                                         1L)))
  }
  return(newvec[1L:length])
}