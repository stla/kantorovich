library(kantorovich)
library(CVXR)

mu <- rep(c(1,2,4), 15)
nu <- rep(c(3,3,1), 15)
mu <- mu/sum(mu)
nu <- nu/sum(nu)

kantorovich_glpk(mu,nu)

m <- n <- 4

#dist <- 1 - diag(m)
obj = c(t(dist))
mat = rbind(-diag(m*n),
            rbind(t(model.matrix(~0+gl(m,n)))[,],
                  t(model.matrix(~0+factor(rep(1:n,m))))[,]))
rhs = c(rep(0,m*n), c(mu, nu))
dir = c(rep("<=", m*n), rep("==", m+n))

A <- -mat[1:(m*n),]
B <- tail(mat, m+n)


x <- Variable(m*m)
objective   <- Minimize(  linearize(t(obj) %*% x ))
constraints <- list(A%*%x >= 0, B%*%x == c(mu,nu))
problem     <- Problem(objective, constraints)

## Checking problem solution

solution <- psolve(problem, feastol = 1e-20)
solution$status
round(solution$getValue(x),3)
solution$value


#########################################
library(kantorovich)
library(microbenchmark)

m <- 50
mu <- rpois(m, 20)
mu <- mu/sum(mu)
nu <- rpois(m, 20)
nu <- nu/sum(nu)

kantorovich_CVX(mu, nu)
kantorovich_ompr(mu, nu)

microbenchmark(
  Rglpk = kantorovich_glpk(mu, nu),
  lpSolve = kantorovich_lp(mu, nu),
  ompr = kantorovich_ompr(mu, nu),
  CVXR = kantorovich_CVX(mu, nu),
  CVXR_GLPK = kantorovich_CVX(mu, nu, ignore_dcp = TRUE),
  CVXR_ECOS = kantorovich_CVX(mu, nu, solver = "ECOS_BB"),
  times = 5
)
