#' @title Spam Classification Task
#'
#' @name mlr_tasks_spam
#' @format [R6::R6Class] inheriting from [TaskClassif].
#' @include mlr_tasks.R
#'
#' @description
#' Spam data set from the UCI machine learning repository (\url{http://archive.ics.uci.edu/ml/datasets/spambase}).
#' Data set collected at Hewlett-Packard Labs to classify emails as spam or non-spam.
#' 57 variables indicate the frequency of certain words and characters in the e-mail.
#' The positive class is set to "spam".
#'
#' @templateVar id spam
#' @template task
#'
#' @source
#' Creators:
#' Mark Hopkins, Erik Reeber, George Forman, Jaap Suermondt.
#' Hewlett-Packard Labs, 1501 Page Mill Rd., Palo Alto, CA 94304
#'
#' Donor:
#' George Forman (gforman at nospam hpl.hp.com) 650-857-7835
#'
#' Preprocessing:
#' Columns have been renamed. Preprocessed data taken from the \CRANpkg{kernlab} package.
#'
#' @references
#' `r format_bib("dua_2017")`
#'
#' @template seealso_task
NULL

load_task_spam = function(id = "spam") {
  b = as_data_backend(readRDS(system.file("extdata", "spam.rds", package = "mlr3")))
  task = TaskClassif$new(id, b, target = "type", positive = "spam",
    label = "HP Spam Detection")
  b$hash = task$man = "mlr3::mlr_tasks_spam"
  task
}

#' @include mlr_tasks.R
mlr_tasks$add("spam", load_task_spam)
