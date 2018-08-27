#' @import checkmate
#' @import data.table
#' @import stringi
#' @import phng
#' @importFrom R6 R6Class
#' @importFrom stats setNames predict
NULL

# environment which holds constants and allows for reflections
mlr3 = new.env(parent = emptyenv())

mlr3$default.opts = list(
  mlr3.verbose = TRUE,
  mlr3.debug = FALSE,
  mlr3.keep.train.output = FALSE,
  mlr3.continue.on.learner.error = FALSE
)

.onLoad = function(libname, pkgname) { #nocov start
  utils::globalVariables(c("role"), package = "mlr3")

  backports::import(pkgname)
  backports::import(pkgname, "hasName", force = TRUE)

  # set default + config options if not already set in this session
  opts = insert(mlr3$default.opts, read_mlr3_config())
  opts = opts[match(names(opts), names(.Options), nomatch = 0L) == 0L]
  if (length(opts))
    options(opts)
} #nocov end
