#' @title ResultData
#'
#' @description
#' Internal object to store results in list of data.tables, arranged in a star schema.
#' It is advised to not directly work on this data structure as it may be changed in the future
#' without further warnings.
#'
#' The main motivation of this data structure is the necessity to avoid storing duplicated [R6][R6::R6Class] objects.
#' While this is usually no problem in a single R session, serialization via [serialize()] (which is
#' used in [save()]/[saveRDS()] or during parallelization) leads to objects with unreasonable memory
#' requirements.
#'
#' @template param_view
#' @keywords internal
#' @export
#' @examples
#' # table overview
#' print(ResultData$new()$data)
ResultData = R6Class("ResultData",
  public = list(

    #' @field data (`list()`)\cr
    #' List of [data.table::data.table()], arranged in a star schema.
    #' Do not operate directly on this list.
    data = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' An alternative construction method is provided by [as_result_data()].
    #'
    #' @param data ([data.table::data.table()]) | `NULL`)\cr
    #'   Do not initialize this object yourself, use [as_result_data()] instead.
    #' @param data_extra (`list()`)\cr
    #'   Additional data to store.
    #'   This can be used to store additional information for each iteration.
    #' @param store_backends (`logical(1)`)\cr
    #'   If set to `FALSE`, the backends of the [Task]s provided in `data` are removed.
    initialize = function(data = NULL, data_extra = NULL, store_backends = TRUE) {
      assert_flag(store_backends)

      if (is.null(data)) {
        self$data = star_init()
      } else {
        assert_names(names(data), permutation.of = c("task", "learner", "learner_state", "resampling", "iteration", "param_values", "prediction", "uhash", "learner_hash"))

        if (nrow(data) == 0L) {
          self$data = star_init()
        } else {
          setcolorder(data, c("uhash", "iteration", "learner_state", "prediction", "task", "learner", "resampling", "param_values", "learner_hash"))
          uhashes = data.table(uhash = unique(data$uhash))
          setkeyv(data, c("uhash", "iteration"))

          task = learner = resampling = NULL
          data[, "task_hash" := task[[1]]$hash, by = "uhash"]
          data[, "learner_phash" := learner[[1]]$phash, by = "uhash"]
          data[, "resampling_hash" := resampling[[1]]$hash, by = "uhash"]

          tasks = data[, list(task = .SD$task[1L]),
            keyby = "task_hash"]
          learners = data[, list(learner = list(.SD$learner[[1L]]$reset())),
            keyby = "learner_phash"]
          resamplings = data[, list(resampling = .SD$resampling[1L]),
            keyby = "resampling_hash"]
          learner_components = data[, list(learner_param_vals = list(.SD$param_values[[1]])),
            keyby = "learner_hash"]

          set(data, j = "task", value = NULL)
          set(data, j = "learner", value = NULL)
          set(data, j = "resampling", value = NULL)
          set(data, j = "param_values", value = NULL)

          data_extras = if (!is.null(data_extra)) {
            assert_list(data_extra, len = nrow(data))
            data.table(uhash = data$uhash, iteration = data$iteration, data_extra = data_extra)
          } else {
            data.table(uhash = character(), iteration = integer(), data_extra = list())
          }
          # remove null rows in data_extra
          data_extras = data_extras[!map_lgl(data_extra, is.null)]
          setkeyv(data_extras, c("uhash", "iteration"))

          if (!store_backends) {
            set(tasks, j = "task", value = lapply(tasks$task, task_rm_backend))
          }

          self$data = list(fact = data, uhashes = uhashes, tasks = tasks, learners = learners,
            resamplings = resamplings, learner_components = learner_components, data_extras = data_extras)
        }
      }
    },

    #' @description
    #' Returns all unique hashes (`uhash` values) of all included [ResampleResult]s.
    #'
    #' @return `character()`.
    uhashes = function(view = NULL) {
      if (is.null(view)) {
        self$data$uhashes$uhash
      } else {
        intersect(self$data$uhashes$uhash, view)
      }
    },

    #' @description
    #' Returns a [`data.table`] with columns `uhash`, `learner_id`, `task_id` and `resampling_id`
    #' for the given view.
    #' The `uhash` uniquely identifies an individual [`ResampleResult`].
    #'
    #' @return `data.table()`
    uhash_table = function(view = NULL) {
      newtbl = list()
      tbl = self$data$fact[private$get_view_index(view), c("uhash", "learner_phash", "task_hash", "resampling_hash"), with = FALSE]
      tbl = tbl[!duplicated(get("uhash")), ]
      # retrieve learner id, task id and resampling id from self$data$learners, self$data$tasks and self$data$resamplings
      newtbl$learner_id = self$data$learners[list(tbl$learner_phash), ids(get("learner")), on = "learner_phash"]
      newtbl$task_id = self$data$tasks[list(tbl$task_hash), ids(get("task")), on = "task_hash"]
      newtbl$resampling_id = self$data$resamplings[list(tbl$resampling_hash), ids(get("resampling")), on = "resampling_hash"]
      newtbl$uhash = tbl$uhash
      setDT(newtbl)
      merge(self$data$uhashes, newtbl, by = "uhash", sort = FALSE)
    },

    #' @description
    #' Returns the number of recorded iterations / experiments.
    #'
    #' @return `integer(1)`.
    iterations = function(view = NULL) {
      if (is.null(view)) {
        nrow(self$data$fact)
      } else {
        length(private$get_view_index(view))
      }
    },

    #' @description
    #' Returns a table of included [Task]s.
    #'
    #' @return `data.table()` with columns `"task_hash"` (`character()`) and `"task"` ([Task]).
    tasks = function(view = NULL) {
      .__ii__ = private$get_view_index(view)
      tab = unique(self$data$fact[.__ii__, "task_hash", with = FALSE], by = "task_hash")
      merge(tab, self$data$tasks, by = "task_hash", sort = TRUE)
    },

    #' @description
    #' Returns a table of included [Learner]s.
    #'
    #' @param states (`logical(1)`)\cr
    #'   If `TRUE`, returns a learner for each iteration/experiment in the [ResultData] object.
    #'   If `FALSE`, returns an exemplary learner (without state) for each [ResampleResult].
    #' @param reassemble (`logical(1)`)\cr
    #'   Reassemble the learners, i.e. re-set the `state` and the hyperparameters which are stored separately before
    #'   returning the learners.
    #'
    #' @return `data.table()` with columns `"learner_hash"` (`character()`) and `"learner"` ([Learner]).
    learners = function(view = NULL, states = TRUE, reassemble = TRUE) {
      .__ii__ = private$get_view_index(view)

      if (states) {
        tab = self$data$fact[.__ii__, c("learner_hash", "learner_phash", "learner_state"), with = FALSE]
        tab = merge(tab, self$data$learners, by = "learner_phash", sort = FALSE)
        tab = merge(tab, self$data$learner_components, by = "learner_hash", sort = TRUE)
        set(tab, j = "learner",
          value = reassemble_learners(tab$learner, states = tab$learner_state, param_vals = tab$learner_param_vals))
      } else {
        tab = unique(self$data$fact[.__ii__, c("learner_hash", "learner_phash"), with = FALSE], by = "learner_hash")
        tab = merge(tab, self$data$learners, by = "learner_phash", sort = FALSE)

        if (reassemble) {
          tab = merge(tab, self$data$learner_components, by = "learner_hash", sort = TRUE)
          set(tab, j = "learner", value = reassemble_learners(tab$learner, param_vals = tab$learner_param_vals))
        } else {
          setkeyv(tab, "learner_hash")
        }
      }

      tab[, c("learner_hash", "learner"), with = FALSE]
    },

    #' @description
    #' Returns a list of states of included [Learner]s without reassembling the learners.
    #'
    #'  @return list of `list()`
    learner_states = function(view = NULL) {
      .__ii__ = private$get_view_index(view)
      learner_hash = learner_state = NULL
      self$data$fact[.__ii__][order(learner_hash), learner_state]
    },

    #' @description
    #' Returns a table of included [Resampling]s.
    #'
    #' @return `data.table()` with columns `"resampling_hash"` (`character()`) and `"resampling"` ([Resampling]).
    resamplings = function(view = NULL) {
      .__ii__ = private$get_view_index(view)
      tab = unique(self$data$fact[.__ii__, "resampling_hash", with = FALSE], by = "resampling_hash")
      merge(tab, self$data$resamplings, by = "resampling_hash", sort = TRUE)
    },

    #' @description
    #' Returns a list of [Prediction] objects.
    #'
    #' @template param_predict_sets
    #'
    #' @return `list()` of [Prediction].
    predictions = function(view = NULL, predict_sets = "test") {
      .__ii__ = private$get_view_index(view)
      as_predictions(self$data$fact[.__ii__, "prediction", with = FALSE][[1L]], predict_sets = predict_sets)
    },

    #' @description
    #' Returns a combined [Prediction] objects.
    #'
    #' @template param_predict_sets
    #'
    #' @return [Prediction].
    prediction = function(view = NULL, predict_sets = "test") {
      do.call(c, self$predictions(view = view, predict_sets = predict_sets))
    },

    #' @description
    #' Returns additional data stored.
    #'
    #' @return [data.table::data.table()].
    data_extra = function(view = NULL) {
      .__ii__ = private$get_view_index(view)
      tab = self$data$fact[.__ii__, c("uhash", "iteration"), with = FALSE]
      merge(tab, self$data$data_extras, by = c("uhash", "iteration"), all.x = TRUE, sort = TRUE)
    },

    #' @description
    #' Combines multiple [ResultData] objects, modifying `self` in-place.
    #'
    #' @param rdata ([ResultData]).
    #'
    #' @return `self` (invisibly).
    combine = function(rdata) {
      assert_class(rdata, "ResultData")

      rbind_if_new = function(x, y, on = key(x)) {
        if (nrow(x) == 0L) {
          return(y[, names(x), with = FALSE])
        }

        new_rows = y[!x, on = on]
        if (nrow(new_rows)) {
          setkeyv(rbindlist(list(x, new_rows), use.names = TRUE), on)
        } else {
          x
        }
      }

      self$data$uhashes = funion(self$data$uhashes, rdata$data$uhashes, all = FALSE)
      for (nn in setdiff(names(self$data), "uhashes")) {
        self$data[[nn]] = rbind_if_new(self$data[[nn]], rdata$data[[nn]])
      }

      invisible(self)
    },

    #' @description
    #' Updates the [ResultData] object, removing rows from all tables which are not referenced
    #' by the fact table anymore.
    #' E.g., can be called after filtering/subsetting the fact table.
    #'
    #' @return Modified `self` (invisibly).
    sweep = function() {
      fact = self$data$fact
      uhashes = unique(self$data$fact[, "uhash", with = FALSE])
      self$data$uhashes = uhashes[self$data$uhashes, on = "uhash", nomatch = NULL]

      for (nn in c("tasks", "learners", "learner_components", "resamplings")) {
        tab = self$data[[nn]]
        if (nrow(tab)) {
          keycol = key(tab)
          y = unique(fact[, keycol, with = FALSE], by = keycol)
          self$data[[nn]] = merge(tab, y, by = keycol, sort = TRUE)
        }
      }

      invisible(self)
    },

    #' @description
    #' Marshals all stored learner models.
    #' This will do nothing to models that are already marshaled.
    #' @param ... (any)\cr
    #'   Additional arguments passed to [`marshal_model()`].
    marshal = function(...) {
      learner_state = NULL
      self$data$fact[, learner_state := lapply(learner_state, function(x) marshal_state_if_model(.state = x, inplace = TRUE, ...))]
      invisible(self)
    },
    #' @description
    #' Unmarshals all stored learner models.
    #' This will do nothing to models which are not marshaled.
    #' @param ... (any)\cr
    #'   Additional arguments passed to [`unmarshal_model()`].
    unmarshal = function(...) {
      learner_state = NULL
      self$data$fact[, learner_state := lapply(learner_state, function(x) unmarshal_state_if_model(.state = x, inplace = TRUE, ...))]
      invisible(self)
    },

    #' @description
    #' Shrinks the object by discarding parts of the stored data.
    #'
    #' @param backends (`logical(1)`)\cr
    #'   If `TRUE`, the [DataBackend] is removed from all stored [Task]s.
    #' @param models (`logical(1)`)\cr
    #'   If `TRUE`, the stored model is removed from all [Learner]s.
    #'
    #' @return Modified `self` (invisibly).
    discard = function(backends = FALSE, models = FALSE) {
      if (assert_flag(backends)) {
        tab = self$data$tasks
        set(tab, j = "task", value = lapply(tab$task, task_rm_backend))
      }

      if (assert_flag(models)) {
        tab = self$data$fact
        set(tab, j = "learner_state", value = lapply(tab$learner_state, remove_named, "model"))
      }

      invisible(self)
    },

    #' @description
    #' Combines internal tables into a single flat [data.table()].
    #'
    #' @param reassemble_learners (`logical(1)`)\cr
    #'   Reassemble the tasks?
    #' @param convert_predictions (`logical(1)`)\cr
    #'   Convert [PredictionData] to [Prediction]?
    #' @template param_predict_sets
    as_data_table = function(view = NULL, reassemble_learners = TRUE, convert_predictions = TRUE, predict_sets = "test") {
      .__ii__ = private$get_view_index(view)

      tab = self$data$fact[.__ii__]
      tab = merge(tab, self$data$tasks, by = "task_hash", sort = FALSE)
      tab = merge(tab, self$data$learners, by = "learner_phash", sort = FALSE)
      tab = merge(tab, self$data$resamplings, by = "resampling_hash", sort = FALSE)
      tab = merge(tab, self$data$learner_components, by = "learner_hash", sort = FALSE)
      if (nrow(self$data$data_extras)) tab = merge(tab, self$data$data_extras, by = c("uhash", "iteration"), all.x = TRUE, sort = FALSE)

      if (nrow(tab)) {
        if (reassemble_learners) {
          set(tab, j = "learner", value = reassemble_learners(tab$learner, states = tab$learner_state, param_vals = tab$learner_param_vals))
        }

        if (convert_predictions) {
          set(tab, j = "prediction", value = as_predictions(tab$prediction, predict_sets = predict_sets))
        }

      }

      cns = c("uhash", "task", "task_hash", "learner", "learner_hash", "learner_param_vals", "resampling",
        "resampling_hash", "iteration", "prediction", if (nrow(self$data$data_extras)) "data_extra")
      merge(self$data$uhashes, tab[, cns, with = FALSE], by = "uhash", sort = FALSE)
    },

    #' @description
    #' Get a table of recorded learner logs.
    #'
    #' @param condition (`character(1)`)
    #'   The condition to extract. One of `"message"`, `"warning"` or `"error"`.
    #'
    #' @return [data.table::data.table()].
    logs = function(view = NULL, condition) {
      .__ii__ = private$get_view_index(view)
      learner_state = NULL
      logs = map(self$data$fact[.__ii__, learner_state], function(s) list(msg = get_log_condition(s, condition)))
      rbindlist(logs, idcol = "iteration", use.names = TRUE)
    },

    #' @description
    #' Sets the threshold for the response prediction of classification learners, given they have output a probability prediction.
    #' @template param_view
    #' @param threshold (`numeric(1)`)\cr
    #'   Threshold value.
    #' @template param_ties_method
    set_threshold = function(view = NULL, threshold, ties_method = "random") {
      assert_numeric(threshold, len = 1L, lower = 0, upper = 1)
      assert_choice(ties_method, c("random", "first", "last"))
      .__ii__ = private$get_view_index(view)
      self$data$fact[.__ii__, "prediction" := lapply(get("prediction"), function(ps) {
        list(lapply(ps, set_threshold_pdata, threshold, ties_method))
      })]
    }
  ),

  active = list(
    #' @field task_type (`character(1)`)\cr
    #'   Returns the task type of stored objects, e.g. `"classif"` or `"regr"`.
    #'   Returns `NULL` if the [ResultData] is empty.
    task_type = function() {
      tab = self$data$tasks
      if (nrow(tab)) {
        tab$task[[1L]]$task_type
      } else {
        NULL
      }
    }
  ),

  private = list(
    get_view_index = function(view) {
      if (is.null(view)) {
        return(TRUE)
      }
      self$data$fact[list(view), on = "uhash", nomatch = NULL, which = TRUE]
    },

    deep_clone = function(name, value) {
      if (name == "data") {
        lapply(self$data, copy)
      } else {
        value
      }
    }
  )
)

#######################################################################################################################
### constructor
#######################################################################################################################
star_init = function() {
  fact = data.table(
    uhash = character(),
    iteration = integer(),
    learner_state = list(),
    prediction = list(),


    learner_hash = character(),
    task_hash = character(),
    learner_phash = character(),
    resampling_hash = character(),

    key = c("uhash", "iteration")
  )

  uhashes = data.table(
    uhash = character()
  )

  tasks = data.table(
    task_hash = character(),
    task = list(),
    key = "task_hash"
  )

  learners = data.table(
    learner_phash = character(),
    learner = list(),
    key = "learner_phash"
  )

  resamplings = data.table(
    resampling_hash = character(),
    resampling = list(),
    key = "resampling_hash"
  )

  learner_components = data.table(
    learner_hash = character(),
    learner_param_vals = list(),
    key = "learner_hash"
  )

  data_extras = data.table(
    uhash = character(),
    iteration = integer(),
    data_extra = list(),
    key = c("uhash", "iteration")
  )

  list(
    fact = fact,
    uhashes = uhashes,
    tasks = tasks,
    learners = learners,
    resamplings = resamplings,
    learner_components = learner_components,
    data_extras = data_extras)
}


#' @title Sets the State and/or ParamSet values in a Learner
#'
#' @param learner ([Learner]).
#' @param states (`list()`).
#' @param param_vals (`list()`).
#'
#' @return list of ([Learner]) with updated state and param values.
#' @noRd
reassemble_learners = function(learners, states = NULL, param_vals = NULL) {
  learners = lapply(learners, function(l) l$clone(deep = TRUE))

  if (!is.null(states)) {
    Map(function(l, s) {
      l$state = s
    }, l = learners, s = states)
  }

  if (!is.null(param_vals)) {
    Map(function(l, pv) {
      l$param_set$values = pv
    }, l = learners, pv = param_vals)
  }
  learners
}