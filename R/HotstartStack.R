#' @export
HotstartStack = R6Class("HotstartStack",
  public = list(

    #' @field stack [data.table::data.table()]\cr
    #' Stores hotstart learners with learner and task hashes.
    stack = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(learners) {
      learners = assert_learners(as_learners(learners))
      self$stack = data.table(
        hotstart_learners = learners,
        learner_train_adapt_hashes = map_chr(learners, learner_train_adapt_hash),
        task_hashes = map_chr(learners, function(l) l$state$task_hash))
    },

    #' @description
    #' Add learners to hotstart stack.
    #'
    #' @param learner [Learner].
    add = function(learners) {
      learners = assert_learners(as_learners(learners))

      rows = data.table(
        hotstart_learners = learners,
        learner_train_adapt_hashes = map_chr(learners, learner_train_adapt_hash),
        task_hashes = map_chr(learners, function(l) l$state$task_hash))

      self$stack = rbindlist(list(self$stack, rows))
    },

    #' @description
    #' Returns the costs for hot starting `learner` with the learners of the
    #' hotstart stack.
    #' 
    #' @param learner [Learner].
    #' @param task_hash [Task].
    #' @param hotstart_stack [data.table::data.table()]\cr
    #'   Optionally, prefiltered hotstart stack table.
    #'
    # @return `numeric()`
    adaption_cost = function(learner, task_hash, hotstart_stack = NULL) {
      assert_learner(learner)
      assert_character(task_hash, len = 1)
      train_adapt_id = learner$param_set$ids(tags = "train_adapt")
      learner_hash = learner_train_adapt_hash(learner)

      cost = if (is.null(hotstart_stack)) {
        pmap_dbl(self$stack, function(hotstart_learners, learner_train_adapt_hashes, task_hashes) {
          if (learner_hash == learner_train_adapt_hashes && task_hash == task_hashes) {
            cost = learner$param_set$values[[train_adapt_id]] - hotstart_learners$param_set$values[[train_adapt_id]]
            if (cost == 0) NA_real_ else cost
          } else {
            NA_real_
          }
        })
      } else {
        assert_data_table(hotstart_stack, min.rows = 1)
        assert_names(names(hotstart_stack), permutation.of = c("hotstart_learners", "learner_train_adapt_hashes", "task_hashes"))

        map_dbl(hotstart_stack$hotstart_learners, function(l) {
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
    #' Returns the learner of the hotstart stack that has the lowest cost of 
    #' hot starting `learner`.
    #' 
    #' @param learner [Learner].
    #' @param task_hash [Task].
    #'
    # @return [Learner]
    adaption_learner = function(learner, task_hash) {
      hotstart_stack = self$stack[.(task_hash,  learner_train_adapt_hash(learner)), on = c("task_hashes", "learner_train_adapt_hashes")]
      if (is.null(hotstart_stack$hotstart_learners[[1]])) return(NULL)
      cost = self$adaption_cost(learner, task_hash, hotstart_stack)
      if (all(is.na(cost))) return(NULL)
      hotstart_stack[which.min(cost), hotstart_learners][[1]]
    }
  )
)

#' @description 
#' Hash (unique identifier) for learner object, excluding parameter values
#' tagged with `train_adapt`.
#' 
#' @param learner [Learner].
#' 
#' @noRd
learner_train_adapt_hash = function(learner) {
  param_vals = learner$param_set$values
  train_adapt_id = learner$param_set$ids(tags = "train_adapt")
  train_ids = setdiff(learner$param_set$ids(tags = "train"), train_adapt_id)
  train_vals = param_vals[names(param_vals) %in% train_ids]

  calculate_hash(class(learner), learner$id, learner$predict_type, learner$fallback$hash, train_vals)
}

