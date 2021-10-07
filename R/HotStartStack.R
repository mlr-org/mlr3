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
      # self$stack = unique(stack, by = c("task_hash", "learner_hash"))
      setkeyv(self$stack, c("task_hash", "learner_hash"))
      invisible(self)
    },

    #' @description
    #' Calculates the cost for each learner of the stack to hot start `learner`.
    #' 
    #' The following special cost values can be returned:
    #' * `NA_real_`: Learner is unsuitable to hot start `learner`.
    #' * `-1`: Learner in the stack and `learner` are identical. 
    #' * `0` The costs for hot starting backwards are always 0.
    #'
    #' @param learner [Learner].
    #' @param task_hash [Task].
    #' @param stack [data.table::data.table()]\cr
    #'   Optionally, prefiltered stack. Must only contain learners of the same
    #'   type as `learner`. Quickens calculation of costs.
    #'
    # @return `numeric()`.
    start_cost = function(learner, task_hash, stack = data.table()) {
      .learner_hash = learner_hotstart_hash(assert_learner(learner))
      .task_hash = assert_string(task_hash)
      hotstart_id = learner$param_set$ids(tags = "hotstart")

      cost = if (nrow(stack) == 0L) {
        # calculation of cost on complete stack
        pmap_dbl(self$stack, function(start_learner, learner_hash, task_hash) {
          if (learner_hash == .learner_hash && task_hash == .task_hash) {
            learner$param_set$values[[hotstart_id]] - start_learner$param_set$values[[hotstart_id]]
          } else {
            # no hot start learner found
            Inf
          }
        })
      } else {
        # calculation of cost on prefiltered stack
        assert_data_table(stack, min.rows = 1)
        assert_names(names(stack), permutation.of = c("start_learner", "learner_hash", "task_hash"))

        map_dbl(stack$start_learner, function(l) {
          learner$param_set$values[[hotstart_id]] - l$param_set$values[[hotstart_id]]
        })
      }

      constant = cost == 0
      cost = if ("adapt_backward" %in% learner$properties && "adapt_forward" %in% learner$properties) {
        cost = pmax(cost, 0)
        ifelse(is.infinite(cost), NA_real_, cost)
      } else if ("adapt_backward" %in% learner$properties) {
        ifelse(cost > 0, NA_real_, 0)
      } else {
        ifelse(cost < 0 | is.infinite(cost), NA_real_, cost)
      }
      cost[constant] = -1
      cost
    }
  ),

  private = list(
    
    # Internally used to query the stack for the learner with the lowest cost of
    # hot starting `learner`. The state of the learner is copied to `learner` and
    # `learner` is returned.
    .start_learner = function(learner, task_hash) {
      .learner_hash = learner_hotstart_hash(assert_learner(learner))
      .task_hash = assert_character(task_hash, len = 1)
      stack = self$stack[list( .task_hash, .learner_hash), nomatch = NULL]
      if (nrow(stack) == 0L) return(NULL)

      cost = self$start_cost(learner, task_hash, stack)
      if (allMissing(cost)) return(NULL)

      start_learner = stack[which_min(cost, na_rm = TRUE), get("start_learner")][[1]]
      learner$state = start_learner$state
      learner
    }
  )
)
