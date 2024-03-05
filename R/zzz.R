#' @import data.table
#' @import checkmate
#' @import paradox
#' @import mlr3misc
#' @import palmerpenguins
#' @importFrom R6 R6Class is.R6
#' @importFrom utils data head tail getFromNamespace packageVersion
#' @importFrom graphics plot
#' @importFrom stats predict rnorm runif sd contr.treatment model.frame terms
#' @importFrom uuid UUIDgenerate
#' @importFrom parallelly availableCores
#' @importFrom future nbrOfWorkers plan
#' @importFrom RhpcBLASctl blas_set_num_threads blas_get_num_procs
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
#' * `"mlr3.allow_utf8_names"`: If set to `TRUE`, checks on the feature names are relaxed, allowing
#'   non-ascii characters in column names. This is an experimental and temporal option to
#'   pave the way for text analysis, and will likely be removed in a future version of the package.
#'   analysis.
#' * `"mlr3.warn_version_mismatch"`: Set to `FALSE` to silence warnings raised during predict if a learner has been
#'   trained with a different version version of mlr3.
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

  # setup logger
  lg = lgr::get_logger(pkgname)
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
    warning("Packages 'mlr3' and 'mlr' are conflicting and should not be loaded in the same session")
  })

  #register_namespace_callback(pkgname, "mlr3pipelines", function(...) {
  #  getFromNamespace(ns = "mlr3pipelines", "GraphLearner")$set("public", "initialize", overwrite = TRUE,
  #    function(graph, id = NULL, param_vals = list(), task_type = NULL, predict_type = NULL, clone_graph = TRUE) {
  #    graph = as_graph(graph, clone = assert_flag(clone_graph))
  #    graph$state = NULL

  #    id = assert_string(id, null.ok = TRUE) %??% paste(graph$ids(sorted = TRUE), collapse = ".")
  #    private$.graph = graph

  #    output = graph$output
  #    if (nrow(output) != 1) {
  #      stop("'graph' must have exactly one output channel")
  #    }
  #    if (!are_types_compatible(output$predict, "Prediction")) {
  #      stop("'graph' output type not 'Prediction' (or compatible with it)")
  #    }

  #    if (is.null(task_type)) {
  #      task_type = infer_task_type(graph)
  #    }
  #    assert_subset(task_type, mlr_reflections$task_types$type)

  #    super$initialize(id = id, task_type = task_type,
  #      feature_types = mlr_reflections$task_feature_types,
  #      predict_types = names(mlr_reflections$learner_predict_types[[task_type]]),
  #      packages = graph$packages,
  #      properties = setdiff(mlr_reflections$learner_properties[[task_type]], "validation"),
  #      man = "mlr3pipelines::GraphLearner"
  #    )

  #    if (length(param_vals)) {
  #      private$.graph$param_set$values = insert_named(private$.graph$param_set$values, param_vals)
  #    }
  #    if (!is.null(predict_type)) self$predict_type = predict_type
  #  })
  #})

  mlr_reflections$loggers[["mlr3"]] = lg
} # nocov end

leanify_package()
