#' @title Spam Classification Task
#'
#' @name mlr_tasks_spam
#' @format [R6::R6Class] inheriting from [TaskClassif].
#' @include mlr_tasks.R
#'
#' @description
#' A classification task for the [kernlab::spam] data set.
#' Positive class is set to "spam".
mlr_tasks$add("spam", function(id = "spam") {
  b = as_data_backend(load_dataset("spam", "kernlab"))
  b$hash = "_mlr_tasks_kernlab_"
  TaskClassif$new(id, b, target = "type", positive = "spam")
})
