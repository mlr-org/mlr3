#' @title Configuration
#' @name mlr3-config
#'
#' @param conf [\code{character(1)}]\cr
#'   Location of the configuration file to load or write.
#'
#' @description
#' The following options are currently supported to be set via \code{\link[base]{options}}.
#' \describe{
#'   \item{\code{mlr3.verbose}}{
#'     Verbosity. Set to \code{FALSE} to suppress some output.
#'   }
#'   \item{\code{mlr3.debug}}{
#'     Debug mode. Set to \code{TRUE} to enable additional output and some (slower) checks.
#'   }
#'   \item{\code{mlr3.keep.train.output}}{
#'     Store the output of the models in the log file. Default: \code{FALSE}.
#'   }
#'   \item{\code{mlr3.continue.on.learner.error}}{
#'     Keep running if a learner encounters a problem. The resulting model will be a model fitted by a dummy learner.
#'   }
#' }
#'
#' These options may be set in a configuration file which is automatically parsed on package load.
#' To get the location according to your operating system, see the example.
#' \code{readMlrngConfig()} reads a configuration file and returns its settings in a named list,
#' \code{writeMlrngConfig()} writes a configuration file with all currently set options.
#' @examples
#' # Location of the config file for your system:
#' conf = file.path(rappdirs::user_config_dir("mlr3"), "config.yml")
#' print(conf)
#'
#' # Current settings:
#' print(readMlrngConfig())


#' @rdname mlr3-config
#' @export
readMlrngConfig = function(conf = file.path(rappdirs::user_config_dir("mlr3"), "config.yml")) {
  # keep it very simple and dep free here
  if (!file.exists(conf))
    return(list())

  message(sprintf("Reading mlr3 config file '%s'", conf))
  opts = try(yaml::read_yaml(conf))
  if (is.null(opts)) # empty config file
    return(list())
  if (inherits(opts, "try-error") || !is.list(opts)) {
    warning(sprintf("Config file '%s' seems to be syntactically invalid", conf))
    return(list())
  }

  names(opts) = sprintf("mlr3.%s", names(opts))
  return(opts)
}

#' @rdname mlr3-config
#' @export
writeMlrngConfig = function(conf = file.path(rappdirs::user_config_dir("mlr3"), "config.yml")) {
  assertPathForOutput(conf, overwrite = TRUE)
  if (!dir.exists(dirname(conf)))
    dir.create(dirname(conf), recursive = TRUE)
  opts = options()
  opts = opts[stri_startswith_fixed(names(opts), "mlr3.")]
  names(opts) = stri_sub(names(opts), from = 7L)
  yaml::write_yaml(opts, file = conf)
}
