loadStuff <- function(pkgList){
  #' Checks to see if each package in the list is installed. If yes, loads it.
  #' If no, installs it, then loads it.
  #' 
  #' @param pkgList A character vector of package names to install/load
  #' 
  #' @export loadStuff
  for(pkg in pkgList){
    if(!require(pkg, character.only=T)){
      install.packages(pkg)
      library(pkg, character.only=T)
    }
  }
}