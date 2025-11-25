#' @import data.table
#' @import checkmate
#' @import cli
#' @import paradox
#' @import mlr3misc
#' @import palmerpenguins
#' @importFrom R6 R6Class is.R6
#' @importFrom utils data head tail getFromNamespace packageVersion
#' @importFrom graphics plot
#' @importFrom stats predict rnorm runif sd contr.treatment model.frame terms quantile weighted.mean
#' @importFrom uuid UUIDgenerate
#' @importFrom parallelly availableCores
#' @importFrom future nbrOfWorkers plan
#' @importFrom methods formalArgs
#'
#' @section Learn mlr3:
#' * Book on mlr3: \url{https://mlr3book.mlr-org.com}

#' * Use cases and examples gallery: \url{https://mlr3gallery.mlr-org.com}
#' * Cheat Sheets: \url{https://github.com/mlr-org/mlr3cheatsheets}
#'
#' @section mlr3 extensions:
#' * Preprocessing and machine learning pipelines: \CRANpkg{mlr3pipelines}
#' * Analysis of benchmark experiments: \CRANpkg{mlr3benchmark}
#' * More classification and regression tasks: \CRANpkg{mlr3data}
#' * Connector to [OpenML](https://www.openml.org): \CRANpkg{mlr3oml}
#' * Solid selection of good classification and regression learners: \CRANpkg{mlr3learners}
#' * Even more learners: \url{https://github.com/mlr-org/mlr3extralearners}
#' * Tuning of hyperparameters: \CRANpkg{mlr3tuning}
#' * Hyperband tuner: \CRANpkg{mlr3hyperband}
#' * Visualizations for many \pkg{mlr3} objects: \CRANpkg{mlr3viz}
#' * Survival analysis and probabilistic regression: \CRANpkg{mlr3proba}
#' * Cluster analysis: \CRANpkg{mlr3cluster}
#' * Feature selection filters: \CRANpkg{mlr3filters}
#' * Feature selection wrappers: \CRANpkg{mlr3fselect}
#' * Interface to real (out-of-memory) data bases: \CRANpkg{mlr3db}
#' * Performance measures as plain functions: \CRANpkg{mlr3measures}
#' * Resampling methods for spatiotemporal data: \CRANpkg{mlr3spatiotempcv}
#' * Data storage and prediction support for spatial objects: \CRANpkg{mlr3spatial}
#'
#' @section Suggested packages:
#' * Parallelization framework: \CRANpkg{future}
#' * Progress bars: \CRANpkg{progressr}
#' * Encapsulated evaluation: \CRANpkg{evaluate}, \CRANpkg{callr} (external process)
#'
#' @section Package Options:
#' * `"mlr3.exec_random"`: Randomize the order of execution in [resample()] and [benchmark()] during
#'   parallelization with \CRANpkg{future}. Defaults to `TRUE`.
#'   Note that this does not affect the order of results.
#' * `"mlr3.exec_chunk_size"`: Number of iterations to perform in a single [future::future()] during
#'   parallelization with \CRANpkg{future}. Defaults to 1.
#' * `"mlr3.exec_chunk_bins"`: Number of bins to split the iterations into. If set, `"mlr3.exec_chunk_size"` is ignored.
#' * `"mlr3.debug"`: If set to `TRUE`, parallelization via \CRANpkg{future} is disabled to simplify
#'   debugging and provide more concise tracebacks.
#'   Note that results computed in debug mode use a different seeding mechanism and are **not reproducible**.
#' * `"mlr3.warn_version_mismatch"`: Set to `FALSE` to silence warnings raised during predict if a learner has been
#'   trained with a different version version of mlr3.
#' * `"mlr3.prob_as_default"`: Set to `TRUE` to set the predict type of classification learners to
#'   `"prob"` by default (if they support it).
#' * `"mlr3.mirai_parallelization"`: Compute profile to use for parallelization with \CRANpkg{mirai}.
#'   Defaults to `"mlr3_parallelization"`.
#' * `"mlr3.mirai_encapsulation"`: Compute profile to use for encapsulation with \CRANpkg{mirai}.
#'   Defaults to `"mlr3_encapsulation"`.
#'
#' @section Error Classes:
#' * `Mlr3Error`: The base mlr3 error class.
#' * `Mlr3ErrorConfig`: This error signals that the user has misconfigured something.
#'   By default, this error is not caught when the learner is encapsulated.
#' * `Mlr3ErrorInput`: This error signals that the input to the function is invalid.
#' * `Mlr3ErrorLearner`: The base error class for errors related to the learner.
#' * `Mlr3ErrorLearnerTrain`: This error signals that the learner failed to train the model.
#' * `Mlr3ErrorLearnerPredict`: This error signals that something went wrong during prediction.
#' * `Mlr3TimeoutError`: This error signals that the encapsulation during train or predict timed out.
#'
#' @section Warning Classes:
#' * `Mlr3Warning`: The base mlr3 warning class.
#' * `Mlr3WarningConfig`: This warning signals that the user has misconfigured something.
#' * `Mlr3WarningInput`: This warning signals that the input to the function is invalid.
#'
#' @references
#' `r tools::toRd(citation("mlr3"))`
"_PACKAGE"

dummy_import = function() {
  # nocov start
  # this function is required to silence R CMD check
  mlbench::mlbench.xor
  mlr3measures::mse
  evaluate::evaluate
} # nocov end


.onLoad = function(libname, pkgname) {
  # nocov start
  backports::import(pkgname)

  # callbacks
  x = utils::getFromNamespace("mlr_callbacks", ns = "mlr3misc")
  x$add("mlr3.model_extractor", load_callback_model_extractor)
  x$add("mlr3.holdout_task", load_callback_holdout_task)

  # setup logger
  lg = lgr::get_logger("mlr3/core")
  assign("lg", lg, envir = parent.env(environment()))
  f = function(event) {
    event$msg = paste0("[mlr3] ", event$msg)
    TRUE
  }
  lg$set_filters(list(f))
  if (Sys.getenv("IN_PKGDOWN") == "true") {
    lg$set_threshold("warn")
  }

  register_namespace_callback(pkgname, "mlr", function(...) {
    warning_mlr3("Packages 'mlr3' and 'mlr' are conflicting and should not be loaded in the same session")
  })
} # nocov end

leanify_package()
