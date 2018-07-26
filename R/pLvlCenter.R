#' Center Variables at the Participant Level
#'
#' @param df Data frame.
#' @param var Variable in \code{df} to center.
#' @param group Grouping variable. Must be in \code{df}!
#' @param cntrValFn Function that returns the value to center around within each
#' participant. Most often the default, \code{mean}.
#' @param returnType Can return either a dataframe ("df"; default) or a vector
#' ("vec").
#' @param ... Other arguments to pass to \code{ctrlValFn}.
#'
#' @return Either a dataframe with the new variable or a vector of the centered
#' values, depending on the \code{returnType} argument.
#' @export pLvlCenter
#'
pLvlCenter <- function(df, var, group='pnum',
                       cntrValFn=mean, returnType=c('df', 'vec'),
                       ...){



  returnType <- match.arg(returnType)
  groupClass <- typeof(df[,group])
  vCName <- paste(var, 'PCen', sep='')
  vMnName <- paste(var,
                   'cntrVal',
                   sep="_")

  pLvlMns <- tapply(df[,var], INDEX=list(df[,group]), FUN=cntrValFn, ...)
  pLvlMns <- data.frame(pLvlMns, names(pLvlMns), stringsAsFactors=F)
  colnames(pLvlMns) <- c(vMnName, group)

  # Convert types to make it possible to merge in every situation
  pLvlMns[,group] <- as(pLvlMns[,group], groupClass)

  dTemp <- merge(df, pLvlMns, by=group)
  dTemp[,vCName] <- dTemp[,var] - dTemp[,vMnName]

  # Drop the intermediate value
  dTemp <- dTemp[, !names(dTemp) %in% c('vMnName')]

  if(returnType=='df'){
    return(dTemp)
  }
  if(returnType=='vec'){
    return(dTemp[,vCName])
  }
}
