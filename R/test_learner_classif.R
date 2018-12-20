#' @title Runs Various Unit Tests on a Learner
#'
#' @description
#' A learner is tested on many different tasks which cover the learner's specified feature types and properties.
#'
#' @param learner ([Learner]):\cr
#'   Object of type [Learner].
#' @return None.
#' @import testthat
#' @export
#' @examples
#'   lrn = LearnerClassifRpart$new()
#'   test_learner_classif(lrn)
# #' \dontshow{
# #'    logger::log_threshold(.threshold, namespace = "mlr3")
# #' }
test_learner_classif = function(learner) {
  assert_learner(learner)
  tasks = make_classif_test_tasks(learner)
  lapply(unlist(tasks), function(x) run_classif_tests(learner = learner, task = x))
  return(invisible())
}

# Helper function. Runs unit tests on a learner and a task.
# If predict_type prob is available, this will be tested as well.
run_classif_tests = function(learner, task) {
  id = paste0("learner ", learner$id, " on task ", task$id)
  e = Experiment$new(task, learner)
  test_train_predict(e, id = id)
  #test predict type prob
  if ("prob" %in% learner$predict_types) {
    learner2 = learner$clone()
    learner2$predict_type = "prob"
    e2 = Experiment$new(task, learner2)
    test_train_predict(e2, id = id)
    expect_true(!is.null(e2$prediction$prob), info = id)
    expect_true(all(e2$prediction$prob >= 0 & e2$prediction$prob <= 1), info = id)
  }
}

