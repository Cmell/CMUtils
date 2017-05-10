logitToP <- function (l) {
  #' Convert a logit unit to a probability.
  #' 
  #' @param l The logit.
  #' @return The probability.
  1 / (1 + exp(-(l)))
}

pToLogit <- function (p) {
  #' Convert a probability unit to a logit.
  #' 
  #' @param p The probability.
  #' @return The logit.
  log(p/(1-p))
}

pToOdds <- function (p) {
  #' Convert a probility to an odds ratio.
  #' 
  #' @param p The probability.
  #' @return The odds ratio.
  p/(1-p)
}

oddsToP <- function (o) {
  #' Convert an odds ratio to a probability.
  #' 
  #' @param o The odds ratio.
  #' @return The probability.
  o/(o+1)
}

logitToOdds <- function (l) {
  #' Convert a logit unit to an odds ratio.
  #' 
  #' @param l The logit.
  #' @return The odds ratio.
  exp(l)
}

oddsToLogit <- function (o) {
  #' Convert an odds ratio to a logit.
  #' 
  #' @param o The odds ratio.
  #' @return The logit.
  log(o)
}
