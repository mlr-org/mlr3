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
#' To log to a file or a data base, see the documentation of [lgr::lgr-package].
