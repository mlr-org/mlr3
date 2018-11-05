library(mlr3)
library(checkmate)
library(testthat)

lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]", full.names = TRUE), source)

with_plan = function(plan, expr) {
  oplan = future::plan()
  on.exit(future::plan(oplan), add = TRUE)
  future::plan(plan)
  force(expr)
}
