#' Convert numeric to rational
#'
#' @param x a numeric vector
#'
asab <- function(x) as.character(gmp::as.bigq(x))

#' Names for bigq
#'
#' @param x a \code{bigq} vector
#' @return the names of \code{x}
#'
#' @export
names.bigq <- function(x){
  if(!requireNamespace("gmp", quietly = TRUE)){
    stop("gmp needed for this function to work.", call. = FALSE)
  } else {
    attr(x, "names")[1:length(x)]
  }
}

#' Vectorize a function returning a bigq number
#'
#' @param f a function of two variables returing a bigq scalar
#' @return the vectorized version of \code{f}
#'
#' @examples
#' if(require(gmp)){
#'  f <- function(x,y){
#'    if(x==y) return(as.bigq(0)) else return(x+y)
#'  }
#'  x <- as.bigq(c(0,0)); y <- as.bigq(c(0,1))
#'  f(x,y)
#'  g <- Vectorize_bigq(f)
#'  g(x,y)
#' }
#' @export
Vectorize_bigq <- function(f){
  if(!requireNamespace("gmp", quietly = TRUE)){
    stop("gmp needed for this function to work.", call. = FALSE)
  } else {
    return(function(x,y) gmp::as.bigq(gmp::apply(cbind(x,y), 1,
                  FUN=function(row) as.character(f(row[1], row[2])))))
  }
}

#' Arrange names
#'
#' @param mu,nu the vectors to be arranged
#'
#' @export
arrange_names <- function(mu, nu){
  # check sum ==1
  if(sum(mu) != 1) stop("sum(mu) != 1")
  if(sum(nu) != 1) stop("sum(nu) != 1")
  # check class
  if(!all(c(class(mu), class(nu)) %in% c("integer", "numeric", "bigq"))) stop("mu and nu must have numeric or bigq class")
  if(class(mu) != class(nu)) stop("mu and nu have not the same class")
  #
  if(is.null(names(mu)) && is.null(names(nu)) && length(mu)==length(nu)){
    names(mu) <- seq_along(mu); names(nu) <- seq_along(nu)
    return(list(mu=mu, nu=nu))
  }
  #
  names_mu <- names(mu); names_nu <- names(nu)
  has_changed <- function(x, y) !identical(names(x), names_mu) || !identical(names(y), names_nu)
  if(is.null(names(mu))) names(mu) <- seq_along(mu)
  if(is.null(names(nu))) names(nu) <- seq_along(nu)
  if(setequal(names(mu), names(nu))){
    if(has_changed(mu,nu)) message("Caution: some names of mu and/or nu were missing or not compatible - automatic change")
    return(list(mu=mu, nu=nu))
  } else if(length(mu) == length(nu)){
    stop("Cannot deal with the names of mu and nu")
  } else if(length(mu) < length(nu)){
    if(all(names(mu) %in% names(nu))){
      message("Caution: some names of mu and/or nu were missing or not compatible - automatic change")
      if(class(mu)=="bigq"){
        mu_ch <- setNames(as.character(mu), names(mu))
        mu_ch[setdiff(names(nu), names(mu))] <- "0"
        mu <- setNames(gmp::as.bigq(mu_ch), names(mu_ch))
      } else {
        mu[setdiff(names(nu), names(mu))] <- 0
      }
      return(arrange_names(mu,nu))
    }else{
      stop("Cannot deal with the names of mu and nu")
    }
  }else if(length(mu) > length(nu)){
    return(setNames(arrange_names(nu,mu)[2:1], c("mu", "nu")))
  }else{
    stop("Cannot deal with the names of mu and nu")
  }
}

#' Discrete 0-1 distance
#'
#' Returns the 0-1 distance between  \code{x} and \code{y}
#'
#' @param x,y vectors
#' @param gmp logical, use exact arithmetic with the help of the \code{gmp} package
#'
#' @examples
#' discrete(2, 3)
#' if(require(gmp)){
#'  discrete(as.bigq(2), as.bigq(3), gmp=TRUE)
#' }
#'
#' @export
discrete <- function(x, y, gmp=FALSE){
  if(gmp && !requireNamespace("gmp", quietly = TRUE)) stop("gmp package is not installed")
  out <- if(gmp) gmp::as.bigq(x != y) else as.integer(x != y)
  return(out)
}

#' Extreme joinings
#'
#' Return extreme joinings between \code{mu} and \code{nu}
#'
#' @param mu row margins
#' @param nu column margins
#'
#' @return a list with the extreme joinings (matrices)
#'@examples
#' mu <- nu <- c(0.5, 0.5)
#' ejoinings(mu, nu)
#' if(require(gmp)){
#'  # use exact arithmetic
#'  library(gmp)
#'  mu <- nu <- as.bigq(c(0.5,0.5))
#'  ejoinings(mu, nu)
#' }
#'
#' @export
ejoinings <- function(mu, nu){
  munu <- arrange_names(mu, nu)
  mu <- munu$mu; nu <- munu$nu
  gmp <- requireNamespace("gmp", quietly = TRUE)
  if(class(mu) != class(nu)) stop("Enter mu and nu in numeric or (preferably) in rational with the gmp package.")
  if(gmp && class(mu) != "bigq") message("Message: You should enter mu and nu in rational with the gmp package.")
  if(length(mu)>1){
    if(length(nu)>1){
      B <- c(mu,nu)
    }else{ B <- mu }
  }else{ B <- nu }
  m <- length(mu)
  n <- length(nu)
  if(m>1){ M1 <- t(model.matrix(~0+gl(m,n)))[,] }else{ M1 <- NULL }
  if(n>1){ M2 <- t(model.matrix(~0+factor(rep(1:n,m))))[,] }else{ M2 <- NULL }
  M <- rbind(M1,M2)
  if(class(mu)=="bigq"){
    mH0 <- rcdd::makeH(a1=asab(-diag(m*n)), b1=asab(rep(0,m*n)), a2=asab(M), b2=asab(B))
  }else{
    mH0 <- rcdd::makeH(a1=-diag(m*n), b1=rep(0,m*n), a2=M, b2=B)
  }
  extremals <- rcdd::scdd(mH0)$output[,-c(1,2)]
  if(is.null(dim(extremals))) extremals <- matrix(extremals, nrow=1)
  extremals <- lapply(1:nrow(extremals), function(i) matrix(extremals[i,], ncol=n, byrow=TRUE) )
  lapply(extremals,
         function(M){
           rownames(M) <- names(mu)
           colnames(M) <- names(nu)
           return(M)
         })
}

#' Extremal distances
#'
#' Compute the distances at the extreme points
#'
#' @param mu row margins
#' @param nu column margins
#' @param dist function, the distance to be minimized on average. If \code{NULL}, the 0-1 distance is used.
#' @param ... arguments passed to \code{dist}
#'
#' @return a list with two components: the extreme joinings in a list and the distances in a vector
#'
#' @export
edistances <- function(mu, nu, dist=NULL, ...){
  tests <- ejoinings(mu, nu)
  n.tests <- length(tests)
  gmp <- requireNamespace("gmp", quietly = TRUE)
  use_gmp <- gmp && class(mu)=="bigq"
  if(is.null(dist)){
    rho <- function(x, y) discrete(x, y, gmp=use_gmp)
  } else{
    rho <- function(x, y) dist(x, y, ...)
  }
  distances <- if(use_gmp) gmp::as.bigq(numeric(n.tests)) else numeric(n.tests)
  for(k in 1:n.tests){
    test <- tests[[k]]
    if(use_gmp){
      Rho <- outer(rownames(test), colnames(test), FUN=Vectorize_bigq(rho))
      distances[k] <- sum(Rho * test)
    }else{
      Rho <- outer(rownames(test), colnames(test), FUN=rho)
      distances[k] <- sum(Rho * test)
    }
  }
  out <- list(joinings=tests, distances=distances)
  return(out)
}

#' Kantorovich distance
#'
#' Compute the Kantorovich distance
#'
#' @param mu row margins
#' @param nu column margins
#' @param dist function, the distance to be minimized on average. If \code{NULL}, the 0-1 distance is used.
#' @param ... arguments passed to \code{dist}
#'
#' @return the Kantorovich distance
#'
#' @examples
#' mu <- c(1/7, 2/7, 4/7)
#' nu <- c(1/4, 1/4, 1/2)
#' kantorovich(mu, nu)
#' if(require(gmp)){
#'  library(gmp)
#'  mu <- as.bigq(c(1,2,4), 7)
#'  nu <- as.bigq(c(1,1,1), c(4,4,2))
#'  kantorovich(mu, nu)
#' }
#' @export
kantorovich <- function(mu, nu, dist=NULL, ...){
  distances <- edistances(mu, nu, dist, ...)
  best <- which(distances$distances==min(distances$distances))
  # to do: return the joinings
  return(distances$distances[[best[1]]])
}
