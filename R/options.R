default_opts = list(
  mlr3.verbose = TRUE,
  mlr3.debug = FALSE,
  mlr3.use.future = TRUE
)

mlr_options = function() {
  opts = .Options[startsWith(names(.Options), "mlr3.")]
  names(opts) = substr(names(opts), 6L, 100L)
  opts
}

is_debug = function() {
  isTRUE(getOption("mlr3.debug"))
}

use_future = function() {
  isTRUE(getOption("mlr3.use.future"))
}
