#' @title Container for Results of `resample()`
#'
#' @include mlr_reflections.R
#'
#' @description
#' This is the result container object returned by [resample()].
#'
#' Note that all stored objects are accessed by reference.
#' Do not modify any object without cloning it first.
#'
#' @template param_measures
#'
#' @section S3 Methods:
#' * `as.data.table(rr, reassemble_learners = TRUE, convert_predictions = TRUE, predict_sets = "test")`\cr
#'   [ResampleResult] -> [data.table::data.table()]\cr
#'   Returns a tabular view of the internal data.
#' * `c(...)`\cr
#'   ([ResampleResult], ...) -> [BenchmarkResult]\cr
#'   Combines multiple objects convertible to [BenchmarkResult] into a new [BenchmarkResult].
#'
#' @export
#' @examples
#' task = tsk("iris")
#' learner = lrn("classif.rpart")
#' resampling = rsmp("cv", folds = 3)
#' rr = resample(task, learner, resampling)
#' print(rr)
#'
#' rr$aggregate(msr("classif.acc"))
#' rr$prediction()
#' rr$prediction()$confusion
#' rr$warnings
#' rr$errors
ResampleResult = R6Class("ResampleResult",
  public = list(
    #' @field data ([data.table::data.table()])\cr
    #' Internal data storage.
    #' We discourage users to directly work with this field.
    #' Use `as.table.table(ResampleResult)` instead.
    data = NULL,


    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' An alternative construction method is provided by [as_resample_result()].
    #'
    #' @param task ([Task])\cr
    #'   Single task all learners are trained on.
    #' @param learner ([Learner])\cr
    #'   Learner used to fit the individual models.
    #' @param states (`list()`)\cr
    #'   List of learner states (this includes the fitted models).
    #' @param resampling ([Resampling])\cr
    #'   Instantiated resampling.
    #'   Number of iterations (`resampling$iters`) must match the number of states.
    #' @param iterations (`integer()`)\cr
    #'   Iteration numbers.
    #' @param predictions (list of [Prediction])\cr
    #'   List of prediction objects.
    #' @param uhash (`character(1)`)\cr
    #'   Unique hash for this `ResampleResult`. If `NULL`, a new unique hash is generated.
    #'   This unique hash is primarily needed to group information in [BenchmarkResult]s.
    initialize = function(task, learner, states, resampling, iterations, predictions, uhash = NULL, data = NULL) {
      # TODO: add more assertions

      if (is.null(data)) {
        #  manual construction
        data = data.table(
          task = list(task),
          learner = list(learner),
          state = states,
          resampling = list(resampling),
          iteration = iterations,
          prediction = predictions,
          uhash = uhash %??% UUIDgenerate()
        )

        self$data = as_snowflake(data)
        # self$task = assert_task(task)
        # self$learner = assert_learner(learner)
        # self$resampling = assert_resampling(resampling)

        # self$data = snowflake_init()

        # self$data = data.table(
        #   iteration = assert_integer(iterations, any.missing = FALSE, lower = 1L, upper = resampling$iters),
        #   state = assert_list(states, len = length(iterations)),
        #   prediction = assert_list(predictions, len = length(iterations))
        # )
      } else if (inherits(data, "snowflake")) {
        self$data = data
      } else {
        assert_data_frame(data)
        assert_names(names(data), must.include =
          c("task", "learner", "resampling", "iteration", "uhash", "state", "prediction"))
        self$data = as_snowflake(data)
      }
    },

    #' @description
    #' Helper for print outputs.
    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    #' @description
    #' Printer.
    #' @param ... (ignored).
    print = function() {
      catf("%s of %i iterations", format(self), nrow(self$data$fact))
      catf(str_indent("* Task:", self$task$id))
      catf(str_indent("* Learner:", self$learner$id))

      warnings = self$warnings
      catf(str_indent("* Warnings:", sprintf("%i in %i iterations", nrow(warnings), uniqueN(warnings, by = "iteration"))))

      errors = self$errors
      catf(str_indent("* Errors:", sprintf("%i in %i iterations", nrow(errors), uniqueN(errors, by = "iteration"))))
    },

    #' @description
    #' Opens the corresponding help page referenced by field `$man`.
    help = function() {
      open_help("mlr3::ResampleResult")
    },

    #' @description
    #' Combined [Prediction] of all individual resampling iterations, and all provided predict sets.
    #' Note that performance measures do not operate on this object,
    #' but instead on each prediction object separately and then combine the performance scores
    #' with the aggregate function of the respective [Measure].
    #'
    #' @param predict_sets (`character()`)\cr
    #'   Subset of `{"train", "test"}`.
    #' @return [Prediction].
    prediction = function(predict_sets = "test") {
      do.call(c, self$predictions(predict_sets = predict_sets))
    },

    #' @description
    #' List of prediction objects, sorted by resampling iteration.
    #' If multiple sets are given, these are combined to a single one for each iteration.
    #'
    #' @param predict_sets (`character()`)\cr
    #'   Subset of `{"train", "test"}`.
    #' @return List of [Prediction] objects, one per element in `predict_sets`.
    predictions = function(predict_sets = "test") {
      as_predictions(self$data$fact$prediction, predict_sets)
    },

    #' @description
    #' Returns a table with one row for each resampling iteration, including all involved objects:
    #' [Task], [Learner], [Resampling], iteration number (`integer(1)`), and [Prediction].
    #' Additionally, a column with the individual (per resampling iteration) performance is added
    #' for each [Measure] in `measures`,
    #' named with the id of the respective measure id.
    #' If `measures` is `NULL`, `measures` defaults to the return value of [default_measures()].
    #'
    #' @param ids (`logical(1)`)\cr
    #'   If `ids` is `TRUE`, extra columns with the ids of objects (`"task_id"`, `"learner_id"`, `"resampling_id"`)
    #'   are added to the returned table.
    #'   These allow to subset more conveniently.
    #'
    #' @param predict_sets (`character()`)\cr
    #'   Vector of predict sets (`{"train", "test"}`) to construct the [Prediction] objects from.
    #'   Default is `"test"`.
    #'
    #' @return [data.table::data.table()].
    score = function(measures = NULL, ids = TRUE, predict_sets = "test") {
      measures = as_measures(measures, task_type = self$task$task_type)
      assert_measures(measures, task = self$task, learner = self$learner)
      assert_flag(ids)
      assert_subset(predict_sets, mlr_reflections$predict_sets)

      tab = score_measures(self, measures)

      if (ids) {
        set(tab, j = "task_id", value = ids(tab[["task"]]))
        set(tab, j = "learner_id", value = ids(tab[["learner"]]))
        set(tab, j = "resampling_id", value = ids(tab[["resampling"]]))
        setcolorder(tab, c("task", "task_id", "learner", "learner_id", "resampling", "resampling_id",
            "iteration", "prediction"))
      }

      # FIXME: inefficient
      # set(tab, j = "prediction", value = as_predictions(tab$prediction, predict_sets))
      remove_named(tab, "uhash")
    },

    #' @description
    #' Calculates and aggregates performance values for all provided measures, according to the
    #' respective aggregation function in [Measure].
    #' If `measures` is `NULL`, `measures` defaults to the return value of [default_measures()].
    #'
    #' @return Named `numeric()`.
    aggregate = function(measures = NULL) {
      measures = as_measures(measures, task_type = self$task$task_type)
      assert_measures(measures, task = self$task, learner = self$learner)
      set_names(map_dbl(measures, function(m) m$aggregate(self)), ids(measures))
    },

    #' @description
    #' Subsets the [ResampleResult], reducing it to only keep the iterations specified in `iters`.
    #'
    #' @param iters (`integer()`)\cr
    #'   Resampling iterations to keep.
    #'
    #' @return
    #' Returns the object itself, but modified **by reference**.
    #' You need to explicitly `$clone()` the object beforehand if you want to keeps
    #' the object in its previous state.
    filter = function(iters) {
      iters = assert_integerish(iters, min.len = 1L, lower = 1L, upper = self$resampling$iters,
        any.missing = FALSE, unique = TRUE, coerce = TRUE)

      self$data$fact = self$data$fact[list(iters), on = "iteration", nomatch = NULL]
      if (nrow(self$data$fact) == 0L) {
        self$data = snowflake_init()
      }
      invisible(self)
    }
  ),

  active = list(
    #' @field uhash (`character(1)`)\cr
    #' Unique hash for this object.
    uhash = function(rhs) {
      assert_ro_binding(rhs)
      self$data$uhash$uhash
    },

    #' @field task ([Task])\cr
    #' The task [resample()] operated on.
    task = function(rhs) {
      assert_ro_binding(rhs)
      reassemble_tasks(self$data$task_objs$task, self$data$tasks$feature_names)[[1L]]
    },

    #' @field learner ([Learner])\cr
    #' Learner prototype [resample()] operated on.
    #' For a list of **trained** learners, see methods `$learners()`.
    learner = function(rhs) {
      assert_ro_binding(rhs)
      reassemble_learners(self$data$learner_objs$learner, param_vals = self$data$learners$param_vals)[[1L]]
    },

    #' @field resampling ([Resampling])\cr
    #' Instantiated [Resampling] object which stores the splits into training and test.
    resampling = function(rhs) {
      assert_ro_binding(rhs)
      self$data$resamplings$resampling[[1L]]
    },

    #' @field learners (list of [Learner])\cr
    #' List of trained learners, sorted by resampling iteration.
    learners = function(rhs) {
      assert_ro_binding(rhs)
      reassemble_learners(rep_len(self$data$learner_objs$learner, nrow(self$data$fact)),
        states = self$data$fact$state, param_vals = self$data$learners$param_vals)
    },

    #' @field warnings ([data.table::data.table()])\cr
    #' A table with all warning messages.
    #' Column names are `"iteration"` and `"msg"`.
    #' Note that there can be multiple rows per resampling iteration if multiple warnings have been recorded.
    warnings = function(rhs) {
      assert_ro_binding(rhs)
      logs = map(self$data$fact$state, function(s) list(msg = get_log_condition(s, "warning")))
      rbindlist(logs, idcol = "iteration", use.names = TRUE)
    },

    #' @field errors ([data.table::data.table()])\cr
    #' A table with all error messages.
    #' Column names are `"iteration"` and `"msg"`.
    #' Note that there can be multiple rows per resampling iteration if multiple errors have been recorded.
    errors = function(rhs) {
      assert_ro_binding(rhs)
      logs = map(self$data$fact$state, function(s) list(msg = get_log_condition(s, "error")))
      rbindlist(logs, idcol = "iteration", use.names = TRUE)
    }
  ),

  private = list(
    deep_clone = function(name, value) {
      if (name == "data") copy(value) else value
    }
  )
)

#' @export
as.data.table.ResampleResult = function(x, ..., hashes = FALSE, predict_sets = "test") { # nolint
  remove_named(as.data.table(x$data, hashes = FALSE, predict_sets = predict_sets), "uhash")
}

#' @export
c.ResampleResult = function(...) {
  do.call(c, lapply(list(...), as_benchmark_result))
}

#' @title Convert to ResampleResult
#'
#' @description
#' Simple S3 method to convert objects to a [ResampleResult].
#'
#' @param x (`any`)\cr
#'  Object to dispatch on, e.g. a [ResampleResult].
#' @param ... (`any`)\cr
#'  Currently not used.
#'
#' @return ([ResampleResult]).
#' @export
as_resample_result = function(x, ...) {
  UseMethod("as_resample_result")
}

#' @rdname as_benchmark_result
#' @export
as_benchmark_result.ResampleResult = function(x, ...) { # nolint
  bmr = BenchmarkResult$new()
  bmr$data = snowflake_copy(x$data)
  bmr
}
