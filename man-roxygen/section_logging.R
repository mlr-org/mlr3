#' @section Logging:
#'
#' The \CRANpkg{mlr3} uses the \CRANpkg{lgr} package for logging.
#' \CRANpkg{lgr} supports multiple log levels which can be queried with
#' `getOption("lgr.log_levels")`.
#'
#' To suppress output and reduce verbosity, you can lower the log from the
#' default level `"info"` to `"warn"`:
#' ```
#' lgr::get_logger("mlr3")$set_threshold("warn")
#' ```
#'
#' To get additional log output for debugging, increase the log level to `"debug"`
#' or `"trace"`:
#' ```
#' lgr::get_logger("mlr3")$set_threshold("debug")
#' ```
#'
#' To obtain a more informative traceback when an iteration fails, set
#' `options(mlr3.debug = TRUE)`. This disables parallelization via \CRANpkg{future}
#' so that errors are raised in the main process.
#' Note that results computed in debug mode use a different seeding mechanism and are **not reproducible**.
#'
#' To log to a file or a data base, see the documentation of [lgr::lgr-package].
