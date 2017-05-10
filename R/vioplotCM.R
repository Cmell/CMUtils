vioplotCM <- function (x, ..., range = 1.5, h = NULL, ylim = NULL, names = NULL, 
          horizontal = FALSE, col = "cornflowerblue", border = "black", lty = 1, 
          lwd = 1, rectCol = "black", colMed = "white", pchMed = 19, 
          at, add = FALSE, wex = 1, drawRect = TRUE, na.fun=na.omit,
          drawMean=T, mnLty=2, mnLwd=1.5, mnCol="black", mnLength=NULL,
          main="", mainLine=1, maincex=1.5, nameCex=1.0,
          xlab="", xlabLine=2, xlabcex=1.1,
          ylab=NULL, ylabLine=2.5, ylabcex=1.3,
          labelMeans=F, mnsDigits=2) 
#' violin plots with control
#' 
#' Plot violin plots the cool way.
#' 
#' @param x A vector of values to vioplot.
#' @param ... More vectors of values to vioplot.
#' @param range A factor to calculate the upper/lower adjacent values.
#' @param h The height for the density estimator, if omit as explained in 
#'   sm.density, h will be set to an optimum.
#' @param ylim y limits (vector of length 2).
#' @param names An optional character vector of labels for each set of values to
#'   be vioplotted. If omitted, they are generated from the variable names.
#' @param horizontal Logical, make it horizontal or vertical.
#' @param col A recycled vector of colors for the vioplots themselves.
#' @param border,rectCol Colors for the things.
#' @param lty,lwd Graphical paramters when drawing the lines.
#' @param colMed,pchMed Graphical paramters for the symbol representing the 
#'   median.
#' @param at The position of each violin. Defaults to 1:n, where n is the number
#'   of vectors being vioplotted.
#' @param add If FALSE (default) a new plot is created.
#' @param wex Relative expansion of the violin.
#' @param drawRect Put on a rectangle on it?
#' @param na.fun Defaults to \code{na.omit()}. This is the way that \code{NA}s 
#'   are dealt with.
#' @param drawMean Should a line be drawn for each mean?
#' @param mnLty,mnLwd,mnCol Graphical paramters for drawing the mean line.
#' @param mnLength The length of the mean line in plot units. Defaults to 
#'   something sensible.
#' @param main Plot main title.
#' @param mainLine,maincex Graphical parameters for the main title.
#' @param nameCex Relative size of the category labels.
#' @param xlab A title for the x-axis placed underneath the category labels.
#' @param xlabLine,xlabCex Graphical parameters for the x-axis label.
#' @param ylab An optional label for the y-axis.
#' @param ylabLine,ylabCex Graphical paramters for the y-axis label.
#' @param labelMeans Logical. If true, the means for each group will be
#'   calculated and added in parentheses to the \code{names} items.
#' @param mnsDigits The number of digits to round the means values to.

{
  pkgList <- c("sm")
  for(p in pkgList){
    if(!require(p, character.only=T)){
      install.packages(p)
      library(p, character.only=T)
    }
  }
  
  recycle <- function (vec, length, na.fill = FALSE) 
  {
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
  
  if (typeof(x)=='list'){
    datas <- c(x, list(...))
  } else {
    datas <- list(x, ...)
  }
  n <- length(datas)
  if (missing(at)) 
    at <- 1:n
  
  # Recycle color vector:
  col <- recycle(col, n)
  colMed <- recycle(colMed, n)
  mnCol <- recycle(mnCol, n)
  rectCol <- recycle(rectCol, n)
  
  # Remove missing cases
  datas <- lapply(datas, na.omit)
  
  upper <- vector(mode = "numeric", length = n)
  lower <- vector(mode = "numeric", length = n)
  q1 <- vector(mode = "numeric", length = n)
  q3 <- vector(mode = "numeric", length = n)
  med <- vector(mode = "numeric", length = n)
  mn <- vector(mode = "numeric", length = n)
  base <- vector(mode = "list", length = n)
  height <- vector(mode = "list", length = n)
  baserange <- c(Inf, -Inf)
  args <- list(display = "none")
  if (!(is.null(h))) 
    args <- c(args, h = h)
  for (i in 1:n) {
    data <- datas[[i]]
    data.min <- min(data)
    data.max <- max(data)
    q1[i] <- quantile(data, 0.25)
    q3[i] <- quantile(data, 0.75)
    med[i] <- median(data)
    mn[i] <- mean(data)
    iqd <- q3[i] - q1[i]
    upper[i] <- min(q3[i] + range * iqd, data.max)
    lower[i] <- max(q1[i] - range * iqd, data.min)
    est.xlim <- c(min(lower[i], data.min), max(upper[i], 
                                               data.max))
    smout <- do.call("sm.density", c(list(data, xlim = est.xlim), 
                                     args))
    hscale <- 0.4/max(smout$estimate) * wex
    base[[i]] <- smout$eval.points
    height[[i]] <- smout$estimate * hscale
    t <- range(base[[i]])
    baserange[1] <- min(baserange[1], t[1])
    baserange[2] <- max(baserange[2], t[2])
  }
  if (!add) {
    xlim <- if (n == 1) 
      at + c(-0.5, 0.5)
    else range(at) + min(diff(at))/2 * c(-1, 1)
    if (is.null(ylim)) {
      ylim <- baserange
    }
  }
  if (is.null(names)) {
    label <- 1:n
  }
  else {
    # add means to labels if requested
    if (labelMeans) {
      mns <- paste0('(M=', round(mn, mnsDigits), ')')
      label <- paste(names, mns)
    } else {
      label <- names
    }
  }
  boxwidth <- 0.05 * wex
  if(is.null(mnLength)) mnLength <- .5 * wex
  if (!add) 
    plot.new()
  if (!horizontal) {
    if (!add) {
      plot.window(xlim = xlim, ylim = ylim)
      axis(2)
      axis(1, at = at, labels = label, cex.axis=nameCex)
      mtext(main, side=3, line=mainLine, cex=maincex)
      mtext(xlab, side=1, line=xlabLine, cex=xlabcex)
      if(!is.null(ylab)){
        mtext(ylab, side=2, line=ylabLine, cex=ylabcex)
      }
    }
    box()
    for (i in 1:n) {
      polygon(c(at[i] - height[[i]], rev(at[i] + height[[i]])), 
              c(base[[i]], rev(base[[i]])), col = col[i], border = border, 
              lty = lty, lwd = lwd)
      if (drawRect) {
        lines(at[c(i, i)], c(lower[i], upper[i]), lwd = lwd, 
              lty = lty)
        rect(at[i] - boxwidth/2, q1[i], at[i] + boxwidth/2, 
             q3[i], col = rectCol[i])
        points(at[i], med[i], pch = pchMed, col = colMed[i])
      }
      if(drawMean) {
        mnX1 <- at[i] - .5*mnLength
        mnX2 <- at[i] + .5*mnLength
        lines(x=c(mnX1, mnX2), 
              y=c(mn[i], mn[i]),
              lty=mnLty, lwd=mnLwd, col=mnCol[i])
      }
    }
  }
  else {
    if (!add) {
      plot.window(xlim = ylim, ylim = xlim)
      axis(1)
      axis(2, at = at, labels = label, cex.axis=nameCex)
      mtext(main, side=3, line=mainLine, cex=maincex)
      mtext(xlab, side=1, line=xlabLine, cex=xlabcex)
      if(!is.null(ylab)){
        mtext(ylab, side=2, line=ylabLine, cex=ylabcex)
      }
    }
    box()
    for (i in 1:n) {
      polygon(c(base[[i]], rev(base[[i]])), c(at[i] - height[[i]], 
                                              rev(at[i] + height[[i]])), col = col[i], border = border, 
              lty = lty, lwd = lwd)
      if (drawRect) {
        lines(c(lower[i], upper[i]), at[c(i, i)], lwd = lwd, 
              lty = lty)
        rect(q1[i], at[i] - boxwidth/2, q3[i], at[i] + 
               boxwidth/2, col = rectCol[i])
        points(med[i], at[i], pch = pchMed, col = colMed[i])
      }
      if(drawMean) {
        mnY1 <- at[i] - .5*mnLength
        mnY2 <- at[i] + .5*mnLength
        lines(x=c(mn[i], mn[i]),
              y=c(mnY1, mnY2), 
              lty=mnLty, lwd=mnLwd, col=mnCol[i])
      }
    }
  }
  invisible(list(upper = upper, lower = lower, median = med, 
                 mean = mn, q1 = q1, q3 = q3))
}