% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_task_regr.R
\name{as_task_regr}
\alias{as_task_regr}
\alias{as_task_regr.TaskRegr}
\alias{as_task_regr.data.frame}
\alias{as_task_regr.DataBackend}
\alias{as_task_regr.TaskClassif}
\title{Convert to a Regression Task}
\usage{
as_task_regr(x, ...)

\method{as_task_regr}{TaskRegr}(x, clone = FALSE, ...)

\method{as_task_regr}{data.frame}(x, target, id = deparse(substitute(x)), ...)

\method{as_task_regr}{DataBackend}(x, target, id = deparse(substitute(x)), ...)

\method{as_task_regr}{TaskClassif}(
  x,
  target = NULL,
  drop_original_target = FALSE,
  drop_levels = TRUE,
  ...
)
}
\arguments{
\item{x}{(\code{any})\cr
Object to convert.}

\item{...}{(\code{any})\cr
Additional arguments.}

\item{clone}{(\code{logical(1)})\cr
If \code{TRUE}, ensures that the returned object is not the same as the input \code{x}.}

\item{target}{(\code{character(1)})\cr
Name of the target column.}

\item{id}{(\code{character(1)})\cr
Id for the new task.
Defaults to the (deparsed and substituted) name of \code{x}.}

\item{drop_original_target}{(\code{logical(1)})\cr
If \code{FALSE} (default), the original target is added as a feature.
Otherwise the original target is dropped.}

\item{drop_levels}{(\code{logical(1)})\cr
If \code{TRUE} (default), unused levels of the new target variable are dropped.}
}
\value{
\link{TaskRegr}.
}
\description{
Convert object to a \link{TaskRegr}.
This is a S3 generic, specialized for at least the following objects:
\enumerate{
\item \link{TaskRegr}: ensure the identity
\item \code{\link[=data.frame]{data.frame()}} and \link{DataBackend}: provides an alternative to the constructor of \link{TaskRegr}.
\item \link{TaskClassif}: Calls \code{\link[=convert_task]{convert_task()}}.
}
}
\examples{
as_task_regr(datasets::mtcars, target = "mpg")
}
