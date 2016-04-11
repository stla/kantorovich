#' Computes Kantorovich distance with GLPK
#'
#' Kantorovich distance using the \code{Rglpk} package
#'
#' @param mu (row margins) probability measure in numeric mode
#' @param nu (column margins) probability measure in numeric mode
#' @param dist matrix, the distance to be minimized on average; if \code{NULL}, the 0-1 distance is used.
#' @param solution logical; if \code{TRUE} the solution is returned in the \code{"solution"} attributes of the output
#' @param ... arguments passed to \code{\link[Rglpk]{Rglpk_solve_LP}}
#'
#' @examples
#' mu <- c(1/7,2/7,4/7)
#' nu <- c(1/4,1/4,1/2)
#' kantorovich_glpk(mu, nu)
#'
#' @import Rglpk
#' @export
#'
kantorovich_glpk <- function(mu, nu, dist=NULL, solution=FALSE, ...){
  # à faire : sortir les solutions
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
  kanto <- Rglpk_solve_LP(obj = c(t(dist)), mat = rbind(-diag(m*n), rbind(t(model.matrix(~0+gl(m,n)))[,], t(model.matrix(~0+factor(rep(1:n,m))))[,])),
                          dir = c(rep("<=", m*n), rep("==", m+n)), rhs =  c(rep(0,m*n), c(mu, nu)), ...)
  # status
  if(kanto$status == 0){
    message("Success")
  }else{
    warning(sprintf("No optimal solution found: status %s \n", kanto$status))
  }
  # output
  out <- kanto$optimum
  if(solution) attr(out, "solution") <- matrix(kanto$solution, nrow=m, byrow=TRUE)
  return(out)
}
