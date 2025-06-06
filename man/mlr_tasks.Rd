% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mlr_tasks.R
\docType{data}
\name{mlr_tasks}
\alias{mlr_tasks}
\title{Dictionary of Tasks}
\format{
\link[R6:R6Class]{R6::R6Class} object inheriting from \link[mlr3misc:Dictionary]{mlr3misc::Dictionary}.
}
\description{
A simple \link[mlr3misc:Dictionary]{mlr3misc::Dictionary} storing objects of class \link{Task}.
Each task has an associated help page, see \code{mlr_tasks_[id]}.

This dictionary can get populated with additional tasks by add-on packages,
e.g. \CRANpkg{mlr3data}, \CRANpkg{mlr3proba} or \CRANpkg{mlr3cluster}.
\CRANpkg{mlr3oml} allows to interact with \href{https://www.openml.org}{OpenML}.

For a more convenient way to retrieve and construct tasks, see \code{\link[=tsk]{tsk()}}/\code{\link[=tsks]{tsks()}}.
}
\section{Methods}{

See \link[mlr3misc:Dictionary]{mlr3misc::Dictionary}.
}

\section{S3 methods}{

\itemize{
\item \code{as.data.table(dict, ..., objects = FALSE)}\cr
\link[mlr3misc:Dictionary]{mlr3misc::Dictionary} -> \code{\link[data.table:data.table]{data.table::data.table()}}\cr
Returns a \code{\link[data.table:data.table]{data.table::data.table()}} with columns "key", "label", "task_type", "nrow", "ncol", "properties",
and the number of features of type "lgl", "int", "dbl", "chr", "fct" and "ord", respectively.
If \code{objects} is set to \code{TRUE}, the constructed objects are returned in the list column named \code{object}.
}
}

\examples{
as.data.table(mlr_tasks)
task = mlr_tasks$get("penguins") # same as tsk("penguins")
head(task$data())

# Add a new task, based on a subset of penguins:
data = palmerpenguins::penguins
data$species = factor(ifelse(data$species == "Adelie", "1", "0"))
task = TaskClassif$new("penguins.binary", data, target = "species", positive = "1")

# add to dictionary
mlr_tasks$add("penguins.binary", task)

# list available tasks
mlr_tasks$keys()

# retrieve from dictionary
mlr_tasks$get("penguins.binary")

# remove task again
mlr_tasks$remove("penguins.binary")
}
\seealso{
Sugar functions: \code{\link[=tsk]{tsk()}}, \code{\link[=tsks]{tsks()}}

Extension Packages: \CRANpkg{mlr3data}

Other Dictionary: 
\code{\link{mlr_learners}},
\code{\link{mlr_measures}},
\code{\link{mlr_resamplings}},
\code{\link{mlr_task_generators}}

Other Task: 
\code{\link{Task}},
\code{\link{TaskClassif}},
\code{\link{TaskRegr}},
\code{\link{TaskSupervised}},
\code{\link{TaskUnsupervised}},
\code{\link{california_housing}},
\code{\link{mlr_tasks_breast_cancer}},
\code{\link{mlr_tasks_german_credit}},
\code{\link{mlr_tasks_iris}},
\code{\link{mlr_tasks_mtcars}},
\code{\link{mlr_tasks_penguins}},
\code{\link{mlr_tasks_pima}},
\code{\link{mlr_tasks_sonar}},
\code{\link{mlr_tasks_spam}},
\code{\link{mlr_tasks_wine}},
\code{\link{mlr_tasks_zoo}}
}
\concept{Dictionary}
\concept{Task}
\keyword{datasets}
