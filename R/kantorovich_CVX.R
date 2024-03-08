#' Computes Kantorovich distance with CVX
#'
#' Kantorovich distance using the \code{CVXR} package
#'
#' @param mu (row margins) probability measure in numeric mode
#' @param nu (column margins) probability measure in numeric mode
#' @param dist matrix defining the distance to be minimized on average
#' @param solution logical; if \code{TRUE} the solution is returned in the
#' \code{"solution"} attributes of the output
#' @param stop_if_fail logical; if \code{TRUE}, an error is returned in the
#' case when no solution is found; if \code{FALSE}, the output of
#' \code{\link[CVXR]{psolve}} is returned with a warning
#' @param solver the \code{CVX} solver, passed to \code{\link[CVXR]{psolve}}
#' @param ... other arguments passed to \code{\link[CVXR]{psolve}}
#'
#' @examples
#' x <- c(1.5, 2, -3)
#' mu <- c(1/7, 2/7, 4/7)
#' y <- c(4, 3.5, 0, -2)
#' nu <- c(1/4, 1/4, 1/4, 1/4)
#' M <- outer(x, y, FUN = function(x, y) abs(x - y))
#' kantorovich_CVX(mu, nu, dist = M)
#'
#' @import CVXR
#' @importFrom methods is
#' @export
kantorovich_CVX <- function(
    mu, nu, dist, solution=FALSE, stop_if_fail=TRUE, solver = "ECOS", ...
){
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

  obj <- c(t(dist))
  A <- rbind(t(model.matrix(~0+gl(m,n)))[,],
             t(model.matrix(~0+factor(rep(1:n,m))))[,])
  x <- Variable(m*n)
  objective   <- Minimize(t(obj) %*% x)
  constraints <- list(x >= 0, A%*%x == c(mu,nu))
  problem     <- Problem(objective, constraints)

  kanto <- psolve(problem, solver = solver, ...)
  # status
  if(kanto$status != "optimal"){
    if(stop_if_fail){
      stop(sprintf("No optimal solution found: status %s \n", kanto$status))
    }else{
      warning(sprintf("No optimal solution found: status %s \n", kanto$status))
      return(kanto)
    }
  }
  # output
  out <- kanto$value
  if(solution) attr(out, "solution") <-
    matrix(kanto$getValue(x), nrow=m, byrow=TRUE)
  out
}
