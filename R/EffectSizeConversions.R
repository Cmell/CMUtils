eta2_f2 <- function (eta2) {
  #' Convert an \eqn{\eta^2_p} to an \eqn{\f^2}.
  #'
  #' @param eta2 The \eqn{\eta^2_p} value.
  #' @return The \eqn{f^2}.
  #'
  #' @export eta2_f2
  eta2 / (1 - eta2)
}

eta2_d <- function (eta2) {
  #' Convert an \eqn{\eta^2_p} to a Cohen's d.
  #'
  #' @param eta2 The \eqn{\eta^2_p} value.
  #' @return Cohen's d.
  #'
  #' @export eta2_d
  2 * sqrt(eta2) / sqrt(1 - eta2)
}

eta2_r <- function (eta2) {
  #' Convert an \eqn{\eta^2_p} to a correlation coefficient (\eqn{r}).
  #'
  #' @param eta2 The \eqn{\eta^2_p} value.
  #' @return \eqn{r}
  #'
  #' @export eta2_r
  sqrt(eta2)
}

eta2_f <- function (eta2) {
  #' Convert an \eqn{\eta^2_p} to an \eqn{f}.
  #'
  #' @param eta2 The \eqn{\eta^2_p} value.
  #' @return \eqn{f}
  #'
  #' @export eta2_f
  sqrt(eta2 / (1 - eta2))
}

