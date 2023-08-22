library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

mu <- c(1/7, 2/7, 4/7)
nu <- c(1/4, 1/4, 1/2)
n <- length(mu)

model <- MIPModel() |>
  add_variable(p[i, j], i = 1:n, j = 1:n, type = "continuous") |>
  add_constraint(p[i, j] >= 0, i = 1:n, j = 1:n) |>
  add_constraint(sum_over(p[i, j], j = 1:n) == mu[i], i = 1:n) |>
  add_constraint(sum_over(p[i, j], i = 1:n) == nu[j], j = 1:n) |>
  set_objective(sum_over(p[i, j], i = 1:n, j = 1:n, i != j), "min")

optimization <- model |>
  solve_model(with_ROI(solver = "glpk"))

objective_value(optimization)
