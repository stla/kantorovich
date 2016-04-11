#' Computes Kantorovich distance with Lp_solve
#'
#' Kantorovich distance using the \code{lpSolve} package
#'
#' @param mu (row margins) probability measure in numeric mode
#' @param nu (column margins) probability measure in numeric mode
#' @param dist matrix, the distance to be minimized on average; if \code{NULL}, the 0-1 distance is used.
#' @param lp.object logical, if \code{FALSE}, the output is the Kantorovich distance; if \code{TRUE}, the output is a \code{\link[lpSolve]{lp.object}}
#' @param solution logical, to use only if \code{lp.object=FALSE}; if \code{TRUE} the solution is returned in the \code{"solution"} attributes of the output
#' @param ... arguments passed to \code{\link[lpSolve]{lp}}
#'
#' @examples
#' mu <- c(1/7,2/7,4/7)
#' nu <- c(1/4,1/4,1/2)
#' kantorovich_lp(mu, nu)
#'
#' @import lpSolve
#' @export
#'
kantorovich_lp <- function(mu, nu, dist=NULL, solution=FALSE, lp.object=FALSE, ...){
  m <- length(mu)
  n <- length(nu)
  # checks
  if(m != n) stop("mu and nu do not have the same length")
  if(!is.null(dist)){
    if(class(dist) != "matrix" || mode(dist) != "numeric") stop("dist must be a numeric matrix")
    if(nrow(dist)!=m || ncol(dist)!=m) stop("invalid dimensions of the dist matrix")
  }
  if(sum(mu)!=1 || sum(nu)!=1 || any(mu<0) || any(nu<0)){
    message("Warning: mu and/or nu are not probability measures")
  }
  #
  if(is.null(dist)) dist <- 1-diag(m)
  kanto <- lpSolve::lp(direction = "min", objective.in = c(t(dist)),
     const.mat = rbind(-diag(m*n), rbind(t(model.matrix(~0+gl(m,n)))[,], t(model.matrix(~0+factor(rep(1:n,m))))[,])),
     const.dir = c(rep("<=", m*n), rep("==", m+n)), const.rhs = c(rep(0,m*n), c(mu, nu)), ...)
  # status
  if(lp.object==FALSE){
    if(kanto$status == 0){
      message("Success")
    }else if(kanto$status ==2){
      cat("Error: no feasible solution found")
    }else{
      cat("Error: status", kanto$status, "\n")
    }
  }
  # output
  out <- if(lp.object) kanto else kanto$objval
  if(!lp.object && solution) attr(out, "solution") <- matrix(kanto$solution, nrow=m, byrow=TRUE)
  return(out)
}
