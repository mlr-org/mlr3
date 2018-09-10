default_opts = list(
  mlr3.verbose = TRUE,
  mlr3.debug = FALSE
)

# creates a snapshot of options required on the slave
mlr_control = function() {
  list(
    verbose = assert_flag(getOption("mlr3.verbose")),
    debug = assert_flag(getOption("mlr3.debug"))
  )
}
