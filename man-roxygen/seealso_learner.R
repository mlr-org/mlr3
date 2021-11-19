#' @seealso
#'
#' * Chapter in the [mlr3book](https://mlr3book.mlr-org.com/):
#'   \url{https://mlr3book.mlr-org.com/basics.html#learners}
#' * Package \CRANpkg{mlr3learners} for a solid collection of essential learners.
#' * Package [mlr3extralearners](https://github.com/mlr-org/mlr3extralearners) for more learners.
#' * [Dictionary][mlr3misc::Dictionary] of [Learners][Learner]: [mlr_learners]
#' * `as.data.table(mlr_learners)` for a table of available [Learners][Learner] in the running session (depending on the loaded packages).
#' * \CRANpkg{mlr3pipelines} to combine learners with pre- and postprocessing steps.
#' * Package \CRANpkg{mlr3viz} for some generic visualizations.
#' * Extension packages for additional task types:
#'    * \CRANpkg{mlr3proba} for probabilistic supervised regression and survival analysis.
#'    * \CRANpkg{mlr3cluster} for unsupervised clustering.
#' * \CRANpkg{mlr3tuning} for tuning of hyperparameters, \CRANpkg{mlr3tuningspaces}
#'   for established default tuning spaces.
#'
#' @family Learner
