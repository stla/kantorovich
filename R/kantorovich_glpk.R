#' Computes Kantorovich distance with GLPK
#'
#' Kantorovich distance using the \code{Rglpk} package
#'
#' @param mu (row margins) probability measure in numeric mode
#' @param nu (column margins) probability measure in numeric mode
#' @param dist matrix defining the distance to be minimized on average
#' @param solution logical; if \code{TRUE} the solution is returned in the
#' \code{"solution"} attributes of the output
#' @param stop_if_fail logical; if \code{TRUE}, an error is returned in the
#' case when no solution is found; if \code{FALSE}, the output of
#' \code{\link[Rglpk]{Rglpk_solve_LP}} is returned with a warning
#' @param ... arguments passed to \code{\link[Rglpk]{Rglpk_solve_LP}}
#'
#' @examples
#' x <- c(1.5, 2, -3)
#' mu <- c(1/7, 2/7, 4/7)
#' y <- c(4, 3.5, 0, -2)
#' nu <- c(1/4, 1/4, 1/4, 1/4)
#' M <- outer(x, y, FUN = function(x, y) abs(x - y))
#' kantorovich_glpk(mu, nu, dist = M)
#'
#' @import Rglpk
#' @importFrom slam as.simple_triplet_matrix
#' @importFrom methods is
#' @export
#'
kantorovich_glpk <- function(mu, nu, dist, solution=FALSE, stop_if_fail=TRUE, ...){
  m <- length(mu)
  n <- length(nu)
  # checks
  if(!is(dist, "matrix") || mode(dist) != "numeric")
    stop("dist must be a numeric matrix")
  if(nrow(dist)!=m || ncol(dist)!=n)
    stop("invalid dimensions of the dist matrix")
  if(sum(mu)!=1 || sum(nu)!=1 || any(mu<0) || any(nu<0)){
    message("Warning: mu and/or nu are not probability measures")
  }
  #
  kanto <-
    Rglpk_solve_LP(
      obj = c(t(dist)),
      mat = as.simple_triplet_matrix(
        rbind(-diag(m*n),
              rbind(t(model.matrix(~0+gl(m,n)))[,],
                    t(model.matrix(~0+factor(rep(1:n,m))))[,]))),
      dir = c(rep("<=", m*n), rep("==", m+n)),
      rhs = c(rep(0,m*n), c(mu, nu)), ...)
  # status
  if(kanto$status != 0){
    if(stop_if_fail){
      stop(sprintf("No optimal solution found: status %s \n", kanto$status))
    }else{
      warning(sprintf("No optimal solution found: status %s \n", kanto$status))
      return(kanto)
    }
  }
  # output
  out <- kanto$optimum
  if(solution) attr(out, "solution") <- matrix(kanto$solution, nrow=m, byrow=TRUE)
  out
}
