#' @title Documentation of mlr3 test helpers
#'
#' @description
#' The mlr3 package contains various helper functions to test the validity of objects such as learners.
#' These functions are not contained in the mlr3 namespaces and are instead located in the `inst/testthat`
#' directory of the source package or the `testthat` directory of the installed package.
#'
#' These files can be sourced with the following line of code:
#' ```
#' lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]", full.names = TRUE), source)
#' ```
#' Other extension packages such as `mlr3proba` have similar files that can be sourced accordingly.
#'
#' This manual page documents the most important helper functions that are relevant when users implement their own
#' custom learners.
#'
#' @section run_autotest():
#' This function runs a Learner's automatic test suite.
#'
#' During the autotests, multiple tasks are generated depending on the properties of the learner.
#' The `run_autotest()` function then trains the learner on each task and predicts with all supported predict types.
#' (see argument `predict_types`).
#' To debug, simply run `result = run_autotest(learner)` and proceed with investigating
#' the task, learner and prediction of the returned `result`.
#'
#' For example usages you can look at the autotests in various mlr3 source repositories such as mlr3learners.
#'
#' **Parameters**:
#'
#' * `learner` ([`Learner`])\cr
#'   The learner to check.
#' * `N` (`integer(1)`)\cr
#'   The number of rows of the generated tasks.
#' * `exclude` (`character()`)\cr
#'   Each task on which the learner is trained has an id.
#'   If for some reason, one or more such tests ought to be disabled, this argument takes in a regular expression
#'   that disables all tasks whose id matches the regular expression.
#' * `predict_types` (`character()`)\cr
#'   The predict types of the learner to check.
#'   Defaults to all predict typpes of the learner.
#' * `check_replicable` (`logical(1)`)\cr
#'   Whether to check that running the learner twice with the same seed should result in identical predictions.
#'   Default is `TRUE`.
#'
#' @section run_paramtest():
#'
#' **Description**:
#'
#' Checks parameters of mlr3 Learners against parameters defined in the upstream functions of the respective learner.
#' The goal is to detect if parameters have been dropped or added in the upstream implementation.
#' Some learners do not have all of their parameters stored within the learner function that is called during training.
#' Sometimes learners come with a "control" function, e.g. `glmnet.control()` from package \CRANpkg{glmnet}.
#' Such learners need to be checked as well since they make up the full ParamSet of the respective learner.
#'
#' To work nicely with the defined ParamSet, certain parameters need to be
#' excluded because these are only present in either the "control" object or the
#' actual top-level function call. Such exclusions should go into argument
#' `exclude` with a comment for the reason of the exclusion. See examples for
#' more information.
#'
#' For example usages you can look at the parameter tests in various mlr3 source repositories such as \CRANpkg{mlr3learners}.
#'
#' **Parameters**:
#'
#' * `learner` (`Learner`)\cr
#'   The learner whose parameter set is being checked.
#' * `fun` (`function()` or list of `functions()`s)\cr
#'   The function(s) containing the parameters that must be implemented by the learner.
#' * `exclude` (`character()`)\cr
#'   Argument names that specified through this argument are exempt from checking.
#'   This can be used when parameters that are available in the `fun` function(s) are not implemented in the learner,
#'   or when the learner implements additional parameters that are not available in the `fun` function(s).
#' * `tag` (`character(1)`)\cr
#'   Only parameters that are tagged with this tag are being checked.
#'   If `NULL` (default), all parameters are checked.
#'
#'
#' @section expect_learner():
#'
#' Checks various properties that learners have to satisfy.
#' Used for testing learner implementations, especially if all methods and fields are implement as document.
#'
#' **Parameters**
#'
#' * `lrn` :: ([`Learner`])\cr
#'    The learner whose properties are being verified.
#' * `tsk` :: ([`Task`])\cr
#'    Optional argument (default is `NULL`).
#'    If provided, some additional checks are being run that check the compatibility of the learner and task.
#' * `check_man` :: (`logical(1)`)\cr
#'    Whether to check if the learner has a man page.
#'
#' @name mlr_test_helpers
NULL
