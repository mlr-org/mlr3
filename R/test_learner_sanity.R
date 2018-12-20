#' @title Runs a Small Benchmark on a Learner
#'
#' @description
#' This function tests if a learner actually produces sensible predictions.
#'
#' @param learner ([Learner]):\cr
#'   Object of type [Learner].
#' @return None.
#' @import testthat
#' @export
#' @examples
#'   lrn = LearnerClassifRpart$new()
#'   test_learner_sanity(lrn)
# #' \dontshow{
# #'    logger::log_threshold(.threshold, namespace = "mlr3")
# #' }
# test_learner_sanity = function(learner) {
#   resampling_featureless = sanity_dictionary$mget(c("iris", "sonar", "zoo"))
#   assert_learner(learner)
#   feature_types = learner$feature_types
#   classif_tasks = list()
#   if (learner$task_type == "classif") {
#     #add multiclass tasks
#     if ("multiclass" %in% learner$properties) {
#       zoo = mlr_tasks$mget("zoo")
#       if (all(zoo$feature_types$type %in% feature_types)) classif_tasks = c(classif_tasks, zoo)
#       iris = mlr_tasks$mget("iris")
#       if (all(iris$feature_types$type %in% feature_types)) classif_tasks = c(classif_tasks, iris)
#     }
#     #add twoclass tasks
#     sonar = mlr_tasks$mget("sonar")
#     if (all(sonar$feature_types$type %in% feature_types)) classif_tasks = c(classif_tasks, sonar)
#
#     #error, if no tasks are added
#     if (length(classif_tasks) == 0) stop("your learner does not support standard classification tasks")
#
#     #benchmark
#     resamplings = mlr_resamplings$mget("cv")
#     measures = mlr_measures$mget("acc")
#     res = data.frame(bmr$aggregated)
#
#     #test if learner beats featureless
#     if ("zoo" %in% res$task_id) expect_beat_featureless_classif("zoo", res)
#     if ("iris" %in% res$task_id) expect_beat_featureless_classif("iris", res)
#     if ("sonar" %in% res$task_id) expect_beat_featureless_classif("sonar", res)
#   }
#
#   if (learner$task_type == "regr") {
#     #FIXME: add regr functionality
#   }
#
#   return(invisible())
# }
#
#
# #create resampling information once
# set.seed(123)
# resamplings_featureless = list()
# library(mlr3)
# resample_featureless = function(task) {
#   task = mlr_tasks$get(task)
#   resampling = mlr_resamplings$get("cv")
#   resampling$instantiate(task)
#   lrn = LearnerClassifFeatureless$new()
#   measures = mlr_measures$mget("acc")
#   resample(task, lrn, resampling, measures)
# }
#
# resamplings_featureless = c(resamplings_featureless, list(zoo = resample_featureless("zoo")))
# resamplings_featureless = c(resamplings_featureless, list(iris = resample_featureless("iris")))
# resamplings_featureless = c(resamplings_featureless, list(sonar = resample_featureless("sonar")))
#




#helper function. Tests if the learner beats the featureless learner.
#args:
#task: character(1): task name.
#res: dataframe. dataframe from benchmark$aggregated.
# expect_beat_featureless_classif = function(task = character(), res) {
#   res_task = res[res$task_id == task, ]
#   acc_task_featureless = res_task[res_task$learner_id == "classif.featureless", "acc"]
#   acc_task_learner = res_task[res_task$learner_id == learner$id, "acc"]
#   info = paste("Your learner does not beat the featureless learner on the", task, "task.")
#   testthat::expect_true(acc_task_learner > acc_task_featureless, info = info)
# }
#
