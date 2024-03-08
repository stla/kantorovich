#' Computes Kantorovich distance with lp_solve
#'
#' Kantorovich distance using the \code{lpSolve} package
#'
#' @param mu (row margins) probability measure in numeric mode
#' @param nu (column margins) probability measure in numeric mode
#' @param dist matrix defining the distance to be minimized on average
#' @param lp.object logical, if \code{FALSE}, the output is the Kantorovich
#' distance; if \code{TRUE}, the output is a \code{\link[lpSolve]{lp.object}}
#' @param solution logical, to use only if \code{lp.object=FALSE};
#' if \code{TRUE} the solution is returned in the \code{"solution"} attributes
#' of the output
#' @param ... arguments passed to \code{\link[lpSolve]{lp}}
#'
#' @examples
#' x <- c(1.5, 2, -3)
#' mu <- c(1/7, 2/7, 4/7)
#' y <- c(4, 3.5, 0, -2)
#' nu <- c(1/4, 1/4, 1/4, 1/4)
#' M <- outer(x, y, FUN = function(x, y) abs(x - y))
#' kantorovich_lp(mu, nu, dist = M)
#'
#' @import lpSolve
#' @importFrom methods is
#' @export
#'
kantorovich_lp <- function(mu, nu, dist, solution=FALSE, lp.object=FALSE, ...){
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
    lpSolve::lp(
      direction = "min",
      objective.in = c(t(dist)),
      const.mat = rbind(-diag(m*n),
                        rbind(t(model.matrix(~0+gl(m,n)))[,],
                              t(model.matrix(~0+factor(rep(1:n,m))))[,])),
      const.dir = c(rep("<=", m*n), rep("==", m+n)),
      const.rhs = c(rep(0,m*n), c(mu, nu)), ...)
  # status
  if(lp.object==FALSE){
    if(kanto$status != 0){
      if(kanto$status == 2){
        message("Error: no feasible solution found")
      }else{
        message("Error: status ", kanto$status)
      }
    }
  }
  # output
  out <- if(lp.object) kanto else kanto$objval
  if(!lp.object && solution)
    attr(out, "solution") <- matrix(kanto$solution, nrow=m, byrow=TRUE)
  out
}
