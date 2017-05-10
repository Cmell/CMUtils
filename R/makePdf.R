makePdf <- function(file, plotFn, printToDefaultDev=T, ...) {
  
  #' Write to a pdf, but allow printing to the default device first.
  #'
  #'@param file The filename to be passed to pdf()
  #'@param plotFn A function that wraps the plot statement. Should not take any
  #'arguments.
  #'@param printToDefaultDev If TRUE (default), the plot function will first
  #'be called without opening the pdf graphics device. Thus, it will print to 
  #'the screen, or in your markdown output, etc.
  #'
  #' @export makePdf
  if (printToDefaultDev) {
    plotFn()
  }
  pdf(file=file, ...)
  plotFn()
  dev.off()
}