eta2_f2 <- function (eta2) {
  #' Convert an \eqn{\eta^2_p} to an \eqn{\f^2}.
  #'
  #' @param eta2 The \eqn{\eta^2_p} value.
  #' @return The \eqn{f^2}.
  #'
  #' @export eta2_f2
  eta2 / (1 - eta2)
}

f2_eta2 <- function (f2) {
  #' Convert an \eqn{\f^2} to an \eqn{\eta^2_p}.
  #'
  #' @param eta2 The \eqn{f^2} value.
  #' @return The \eqn{\eta^2_p}.
  #'
  #' @export eta2_f2
  f2 / (1 + f2)
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

d_eta2 <- function (d) {
  #' Convert a Cohen's d to an \eqn{\eta^2_p}.
  #'
  #' @param d2 The Cohen's d value.
  #' @return \eqn{\eta^2_p}
  #'
  #' @export d_eta2
  d^2 / (1 + d^2)
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

r_eta2 <- function (r) {
  #' Convert a correlation coefficient to a \eqn{\eta^2_p}.
  #'
  #' @param r The correlation coefficient.
  #' @return \eqn{\eta^2_p}
  #'
  #' @export r_eta2
  r^2
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

f_eta2 <- function (f) {
  #' Convert an \eqn{f} to an \eqn{\eta^2_p}.
  #'
  #' @param f The \eqn{f} value.
  #' @return \eqn{\eta^2_p}
  #'
  #' @export f_eta2
  f^2 / (1 + f^2)
}

