#' @title Stack for Hot Start Learners
#'
#' @description
#' This class stores learners for hot starting. When fitting a learner
#' repeatedly on the same task but with a different fidelity, hot starting
#' accelerates model fitting. The learner reuses a previously fitted model while
#' training e.g. adding trees to a random forest.
#'
#' The `HotstartStack` stores trained learners which can be potentially used to
#' hot start a learner. Learner automatically hot start while training if a
#' stack is attached to the `$hotstart_stack` field and the stack contains a
#' suitable learner (see examples).
#'
#' Hot starting is only supported by learners which have the property
#' `"hotstart_forward"` or `"hotstart_backward"`. For example, an xgboost model
#' can hot start forward by adding more boosting iterations and a random forest
#' can go backwards by removing trees. The fidelity parameters are tagged with
#' `"hotstart"` in learner's parameter set.
#'
#' @export
#' @examples
#' # train learner on pima task
#' task = tsk("pima")
#' learner = lrn("classif.debug", iter = 1)
#' learner$train(task)
#'
#' # initialize stack with previously fitted learner
#' hot = HotstartStack$new(list(learner))
#'
#' # retrieve learner with increased fidelity parameter
#' learner = lrn("classif.debug", iter = 2)
#'
#' # calculate cost of hot starting
#' hot$start_cost(learner, task$hash)
#'
#' # add stack with hot start learner
#' learner$hotstart_stack = hot
#'
#' # train automatically uses hot start learner while fitting the model
#' learner$train(task)
HotstartStack = R6Class("HotstartStack",
  public = list(

    #' @field stack [data.table::data.table()]\cr
    #' Stores hot start learners.
    stack = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param learners (List of [Learner]s).
    initialize = function(learners) {
      learners = assert_learners(as_learners(learners))
      self$stack = data.table(
        start_learner = learners,
        task_hash = map_chr(learners, function(l) l$state$task_hash),
        learner_hash = map_chr(learners, learner_hotstart_hash),
        key = c("task_hash", "learner_hash"))
    },

    #' @description
    #' Add learners to hot start stack.
    #'
    #' @param learners (List of [Learner]s).
    #'
    #' @return self (invisibly).
    add = function(learners) {
      learners = assert_learners(as_learners(learners))

      rows = data.table(
        start_learner = learners,
        task_hash = map_chr(learners, function(l) l$state$task_hash),
        learner_hash = map_chr(learners, learner_hotstart_hash))

      self$stack = rbindlist(list(self$stack, rows))
      setkeyv(self$stack, c("task_hash", "learner_hash"))
      invisible(self)
    },

    #' @description
    #' Calculates the cost for each learner of the stack to hot start `learner`.
    #'
    #' The following cost values can be returned:
    #'
    #' * `NA_real_`: Learner is unsuitable to hot start `learner`.
    #' * `-1`: Learner in the stack and `learner` are identical.
    #' * `0` Cost for hot starting backwards are always 0.
    #' * `> 0` Cost for hot starting forward.
    #'
    #' @param learner [Learner].
    #' @param task_hash [Task].
    #'
    # @return `numeric()`.
    start_cost = function(learner, task_hash) {
      .learner_hash = learner_hotstart_hash(assert_learner(learner))
      .task_hash = assert_string(task_hash)
      hotstart_id = learner$param_set$ids(tags = "hotstart")

      set(self$stack, j = "cost", value = NA_real_)
      cost = self$stack[list(.task_hash, .learner_hash), "cost" := map_dbl(get("start_learner"), function(l) calculate_cost(l, learner, hotstart_id)) , on = c("task_hash", "learner_hash")
        ][, get("cost")]
      self$stack[, "cost" := NULL]
      cost
    }
  ),

  private = list(

    # Queries the stack for the start learner with the lowest cost of hot
    # starting `learner`. The start learner's state is copied to `learner` and
    # `learner` is returned. This method is internally used by `Learner$train()`
    # `resample()` and `benchmark()` which call `learner_train(learner, task,
    # row_ids, mode = 'retrain')` with the returned learner.
    .start_learner = function(learner, task_hash) {
      .learner_hash = learner_hotstart_hash(assert_learner(learner))
      .task_hash = assert_character(task_hash, len = 1)
      hotstart_id = learner$param_set$ids(tags = "hotstart")

      start_learner = self$stack[list(.task_hash, .learner_hash), on = c("task_hash", "learner_hash"), nomatch = NULL
        ][, "cost" := map_dbl(start_learner, function(l) calculate_cost(l, learner, hotstart_id))
        ][which_min(get("cost"), na_rm = TRUE), start_learner]

      if (!length(start_learner)) return(NULL)
      learner$state = start_learner[[1]]$state
      learner
    }
  )
)

calculate_cost = function(start_learner, learner, hotstart_id) {
  if (is.null(start_learner)) return(NA_real_)

  cost = learner$param_set$values[[hotstart_id]] - start_learner$param_set$values[[hotstart_id]]
  if (cost == 0) return(-1)

  if ("hotstart_backward" %in% learner$properties && "hotstart_forward" %in% learner$properties) {
    if (cost < 0) 0 else cost
  } else if ("hotstart_backward" %in% learner$properties) {
    if (cost < 0) 0 else NA_real_
  } else {
    if (cost > 0) cost else NA_real_
  }
}
