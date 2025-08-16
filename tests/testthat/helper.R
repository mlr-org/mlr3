library(mlr3)
library(checkmate)
library(testthat)

lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]", full.names = TRUE), source)

# Mlr3Component autotest
lapply(list.files(system.file("testthat", package = "mlr3misc"), pattern = "^helper.*\\.[rR]", full.names = TRUE),
  source
)
