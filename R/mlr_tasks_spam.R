#' @title Spam Classification Task
#' @name mlr_tasks_spam
#' @description
#' A classification task for the [kernlab::spam] data set.
#' Positive class is set to "spam".
#' @include mlr_tasks.R
mlr_tasks$add("spam", function() {
  b = as_data_backend(load_dataset("spam", "kernlab"))
  b$hash = "_mlr_tasks_kernlab_"
  TaskClassif$new("spam", b, target = "type", positive = "spam")
})
