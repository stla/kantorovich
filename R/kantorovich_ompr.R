if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("i", "j", "p"))
}

#' Computes Kantorovich distance with 'ompr'
#'
#' Kantorovich distance using the \code{ompr} package
#'
#' @param mu (row margins) probability measure in numeric mode
#' @param nu (column margins) probability measure in numeric mode
#' @param dist matrix defining the distance to be minimized on average
#' @param solution logical; if \code{TRUE} the solution is returned in the
#' \code{"solution"} attributes of the output
#' @param stop_if_fail logical; if \code{TRUE}, an error is returned in the
#' case when no solution is found; if \code{FALSE}, the output of
#' \code{\link[ompr]{solve_model}} is returned with a warning
#'
#' @note The \code{glpk} solver is the one used to solve the problem.
#'
#' @examples
#' x <- c(1.5, 2, -3)
#' mu <- c(1/7, 2/7, 4/7)
#' y <- c(-4, 3.5, 0)
#' nu <- c(1/4, 1/4, 1/2)
#' M <- outer(x, y, FUN = function(x, y) abs(x - y))
#' kantorovich_ompr(mu, nu, dist = M)
#'
#' @import ompr
#' @importFrom ompr.roi with_ROI
#' @import ROI.plugin.glpk
#' @importFrom methods is
#' @importFrom utils capture.output
#' @export
kantorovich_ompr <- function(
    mu, nu, dist, solution = FALSE, stop_if_fail = TRUE
){
  m <- length(mu)
  n <- length(nu)
  # checks
  if(!is(dist, "matrix") || mode(dist) != "numeric")
    stop("`dist` must be a numeric matrix.")
  if(nrow(dist) != m || ncol(dist) != n)
    stop("invalid dimensions of the `dist` matrix.")
  if(sum(mu) != 1 || sum(nu) != 1 || any(mu<0) || any(nu<0)) {
    message("Warning: `mu` and/or `nu` are not probability measures.")
  }
  #
  #if(is.null(dist)) dist <- 1 - diag(m)

  #
  model <- MIPModel() |>
    add_variable(p[i, j], i = 1L:m, j = 1L:n, type = "continuous") |>
    add_constraint(p[i, j] >= 0, i = 1L:m, j = 1L:n) |>
    add_constraint(sum_over(p[i, j], j = 1L:n) == mu[i], i = 1L:m) |>
    add_constraint(sum_over(p[i, j], i = 1L:m) == nu[j], j = 1L:n) |>
    set_objective(
      sum_over(p[i, j] * dist[i, j], i = 1L:m, j = 1L:n, i != j), "min"
    )

  optimization <- model |>
    solve_model(with_ROI(solver = "glpk"))

  msg <- capture.output(optimization$model)
  message(paste0(msg, collapse = "\n"))

  status <- solver_status(optimization)
  if(status != "success" && status != "optimal") {
    if(stop_if_fail) {
      stop(
        sprintf("No optimal solution found: status %s. \n", dQuote(status))
      )
    } else {
      warning(
        sprintf("No optimal solution found: status %s. \n", dQuote(status))
      )
      return(optimization)
    }
  }

  # output
  out <- objective_value(optimization)
  if(solution) attr(out, "solution") <-
    optimization[["solution"]]
  out
}
