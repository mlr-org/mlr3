#' @title Stack for Hot Start Learners
#' 
#' @description 
#' This class stores [Learner]s for hot starting other learners.
#' 
#' @export
HotStartStack = R6Class("HotStartStack",
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
        learner_hash = map_chr(learners, learner_train_adapt_hash),
        task_hash = map_chr(learners, function(l) l$state$task_hash))
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
        learner_hash= map_chr(learners, learner_train_adapt_hash),
        task_hash = map_chr(learners, function(l) l$state$task_hash))

      self$stack = rbindlist(list(self$stack, rows))
      invisible(self)
    },

    #' @description
    #' Calculates costs of hot starting `learner`. Returns `NA` if learner is 
    #  unsuitable for hot starting.
    #'
    #' @param learner [Learner].
    #' @param task_hash [Task].
    #' @param stack [data.table::data.table()]\cr
    #'   Optionally, prefiltered stack. Must only contain learners of the same
    #'   type as `learner`. Quickens calculation of costs.
    #'
    # @return `numeric()`.
    adaption_cost = function(learner, task_hash, stack = NULL) {
      .learner_hash = learner_train_adapt_hash(assert_learner(learner))
      .task_hash = assert_character(task_hash, len = 1)
      train_adapt_id = learner$param_set$ids(tags = "train_adapt")

      cost = if (is.null(stack)) {
        # calculation of cost on complete stack
        pmap_dbl(self$stack, function(start_learner, learner_hash, task_hash) {
          if (learner_hash == .learner_hash && task_hash == .task_hash) {
            cost = learner$param_set$values[[train_adapt_id]] - start_learner$param_set$values[[train_adapt_id]]
            if (cost == 0) NA_real_ else cost
          } else {
            # no hot start learner found
            NA_real_
          }
        })
      } else {
        # calculation of cost on prefiltered stack
        assert_data_table(stack, min.rows = 1)
        assert_names(names(stack), permutation.of = c("start_learner", "learner_hash", "task_hash"))

        map_dbl(stack$start_learner, function(l) {
          cost = learner$param_set$values[[train_adapt_id]] - l$param_set$values[[train_adapt_id]]
          if (cost == 0) NA_real_ else cost
        })
      }

      if ("adapt_backward" %in% learner$properties && "adapt_forward" %in% learner$properties) {
        as.numeric(ifelse(cost > 0, cost, 0))
      } else if ("adapt_backward" %in% learner$properties) {
        as.numeric(ifelse(cost > 0, NA_real_, 0))
      } else {
        as.numeric(ifelse(cost < 0, NA_real_, cost))
      }
    },

    #' @description
    #' Returns learner with the lowest cost of hot starting `learner`. If no
    #' learner is found, returns `NULL`. 
    #'
    #' @param learner [Learner].
    #' @param task_hash [Task].
    #'
    # @return [Learner].
    adaption_learner = function(learner, task_hash) {
      .learner_hash = learner_train_adapt_hash(assert_learner(learner))
      .task_hash = assert_character(task_hash, len = 1)

      stack = self$stack[list(.task_hash,  .learner_hash), on = c("task_hash", "learner_hash")]
      if (is.null(stack$start_learner[[1]])) return(NULL)

      cost = self$adaption_cost(learner, task_hash, stack)
      if (all(is.na(cost))) return(NULL)
      
      stack[which.min(cost), get("start_learner")][[1]]
    }
  )
)
