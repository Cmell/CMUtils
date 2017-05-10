loadStuff <- function(pkgList){
  for(pkg in pkgList){
    if(!require(pkg, character.only=T)){
      install.packages(pkg)
      library(pkg, character.only=T)
    }
  }
}