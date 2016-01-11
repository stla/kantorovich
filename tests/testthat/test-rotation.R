# context("Rotation example")
#
# # ça marche ici mais pas avec devtool::test() pour une raison incompréhensible
# # ça marche si je rentre mu et nu à la main : mu <- as.bigq(c(1,8),9)
# test_that("Rotation example", {
#   if(requireNamespace("gmp", quietly = TRUE)){
#     library(gmp)
#     #
#     #   library(contfrac)
#     #   theta_cf <- c(0,4,2,3,2,3,4,5,4,3,2,4)
#     #   theta <- CF(theta_cf, finite=TRUE)
#     #   q <- convergents(theta_cf)$B
#     #   a <- a <- theta_cf[-1]
#     #
#     theta <- 0.225406088569612
#     q <- c(1, 4, 9, 31, 71, 244, 1047, 5479, 22963, 74368, 171699, 761164)
#     a <- c(4, 2, 3, 2, 3, 4, 5, 4, 3, 2, 4)
#     #
#     mu1 <- c(1-a[1]*theta, a[1]*theta)
#     mu2 <- c((a[2]+1)*theta, 1-(a[2]+1)*theta)
#     # noyau de 2 à 1
#     P21_0 <- as.bigq(c(q[1], q[3]-q[1]), q[3])
#     P21_1 <- as.bigq(c(0, 1), 1)
#     P21 <- matrix(c(P21_0, P21_1), byrow=TRUE, nrow=2)
#     # noyau de 3 à 2
#     P32_0 <- as.bigq(c(1, 0), 1)
#     P32_1 <- as.bigq(c(q[4]-q[2], q[2]), q[4])
#     P32 <- matrix(c(P32_0, P32_1), byrow=TRUE, nrow=2)
#     #
#     rho1 <- function(x,y) discrete(x, y, gmp=TRUE)
#     yy <- as.vector.bigq(P21[2,])
#     xx <- arrange_names(as.vector.bigq(P21[1,]), as.vector.bigq(P21[2,]))
#     kanto2 <- kantorovich(as.vector.bigq(P21[1,]), as.vector.bigq(P21[2,]), dist=rho1)
#    # kanto2 <- kantorovich(mu=as.vector.bigq(P21[1,]), nu=as.vector.bigq(P21[2,]))
# #    mu <- as.bigq(c(1,8),9)
# #    nu <- as.bigq(c(0,1),1)
# #     mu=as.vector.bigq(P21[1,])
# #     nu=as.vector.bigq(P21[2,])
# #    kanto2 <- kantorovich(mu, nu)
#     expect_identical(kanto2, as.bigq(1,9))
#     #   rho0 <- function(x,y){
#     #     if(x==y) return(0) else return(as.character(kanto2))
#     #   }
#     #   rho2 <- function(x,y){
#     #     as.bigq(apply(cbind(x,y), 1, FUN=function(row) rho0(row[1], row[2])))
#     #   }
#     rho <- function(x,y){
#       if(x==y) return(0) else return(as.character(kanto2))
#     }
#     kanto3 <- kantorovich(as.vector.bigq(P32[1,]), as.vector.bigq(P32[2,]), dist=rho)
#     expect_identical(kanto3, as.bigq(4,279))
#   }
# })
