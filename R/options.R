default_opts = list(
  mlr3.verbose = TRUE,
  mlr3.debug = FALSE
)

mlr_options = function() {
  opts = .Options[startsWith(names(.Options), "mlr3.")]
  names(opts) = substr(names(opts), 6L, 100L)
  opts
}
