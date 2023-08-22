#' Computes Kantorovich distance with 'ompr'
#'
#' Kantorovich distance using the \code{ompr} package
#'
#' @param mu (row margins) probability measure in numeric mode
#' @param nu (column margins) probability measure in numeric mode
#' @param dist matrix, the distance to be minimized on average;
#' if \code{NULL}, the 0-1 distance is used.
#' @param solution logical; if \code{TRUE} the solution is returned in the
#' \code{"solution"} attributes of the output
#' @param stop_if_fail logical; if \code{TRUE}, an error is returned in the
#' case when no solution is found; if \code{FALSE}, the output of
#' \code{\link[ompr]{solve_model}} is returned with a warning
#'
#' @note The \code{glpk} solver is the one used to solve the problem.
#'
#' @examples
#' mu <- c(1/7,2/7,4/7)
#' nu <- c(1/4,1/4,1/2)
#' kantorovich_ompr(mu, nu)
#'
#' @import ompr
#' @importFrom ompr.roi with_ROI
#' @import ROI.plugin.glpk
#' @importFrom methods is
#' @importFrom utils capture.output
#' @export
kantorovich_ompr <- function(
    mu, nu, dist = NULL, solution = FALSE, stop_if_fail = TRUE
){
  m <- length(mu)
  n <- length(nu)
  # checks
  if(m != n) stop("`mu` and `nu` do not have the same length.")
  if(!is.null(dist)) {
    if(!is(dist, "matrix") || mode(dist) != "numeric")
      stop("`dist` must be a numeric matrix.")
    if(nrow(dist) != m || ncol(dist) != m)
      stop("invalid dimensions of the `dist` matrix.")
  }
  if(sum(mu) != 1 || sum(nu) != 1 || any(mu<0) || any(nu<0)) {
    message("Warning: `mu` and/or `nu` are not probability measures.")
  }
  #
  if(is.null(dist)) dist <- 1 - diag(m)

  #
  model <- MIPModel() |>
    add_variable(p[i, j], i = 1L:n, j = 1L:n, type = "continuous") |>
    add_constraint(p[i, j] >= 0, i = 1L:n, j = 1L:n) |>
    add_constraint(sum_over(p[i, j], j = 1L:n) == mu[i], i = 1L:n) |>
    add_constraint(sum_over(p[i, j], i = 1L:n) == nu[j], j = 1L:n) |>
    set_objective(
      sum_over(p[i, j] * dist[i, j], i = 1L:n, j = 1L:n, i != j), "min"
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
