% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TaskClassif_breast_cancer.R
\name{mlr_tasks_breast_cancer}
\alias{mlr_tasks_breast_cancer}
\title{Wisconsin Breast Cancer Classification Task}
\format{
\link[R6:R6Class]{R6::R6Class} inheriting from \link{TaskClassif}.
}
\description{
A classification task for the \link[mlbench:BreastCancer]{mlbench::BreastCancer} data set.
\itemize{
\item Column \code{"Id"} has been removed.
\item Column names have been converted to \code{snake_case}.
\item Positive class is set to \code{"malignant"}.
\item 16 incomplete cases have been removed from the data set.
\item All factor features have been converted to ordered factors.
}
}
\section{Construction}{
\preformatted{mlr_tasks$get("breast_cancer")
tsk("breast_cancer")
}
}

\section{Meta Information}{

\itemize{
\item Task type: \dQuote{classif}
\item Dimensions: 683x10
\item Properties: \dQuote{twoclass}
\item Has Missings: \code{FALSE}
\item Target: \dQuote{class}
\item Features: \dQuote{bare_nuclei}, \dQuote{bl_cromatin}, \dQuote{cell_shape}, \dQuote{cell_size}, \dQuote{cl_thickness}, \dQuote{epith_c_size}, \dQuote{marg_adhesion}, \dQuote{mitoses}, \dQuote{normal_nucleoli}
}
}

\seealso{
\itemize{
\item Chapter in the \href{https://mlr3book.mlr-org.com/}{mlr3book}:
\url{https://mlr3book.mlr-org.com/tasks.html}
\item Package \CRANpkg{mlr3data} for more toy tasks.
\item Package \CRANpkg{mlr3oml} for downloading tasks from \url{https://openml.org}.
\item Package \CRANpkg{mlr3viz} for some generic visualizations.
\item \link[mlr3misc:Dictionary]{Dictionary} of \link[=Task]{Tasks}: \link{mlr_tasks}
\item \code{as.data.table(mlr_tasks)} for a table of available \link[=Task]{Tasks} in the running session (depending on the loaded packages).
\item Extension packages for additional task types:
\itemize{
\item \CRANpkg{mlr3proba} for probabilistic supervised regression and survival analysis.
\item \CRANpkg{mlr3cluster} for unsupervised clustering.
}
}

Other Task: 
\code{\link{TaskClassif}},
\code{\link{TaskRegr}},
\code{\link{TaskSupervised}},
\code{\link{TaskUnsupervised}},
\code{\link{Task}},
\code{\link{mlr_tasks_boston_housing}},
\code{\link{mlr_tasks_german_credit}},
\code{\link{mlr_tasks_iris}},
\code{\link{mlr_tasks_mtcars}},
\code{\link{mlr_tasks_penguins}},
\code{\link{mlr_tasks_pima}},
\code{\link{mlr_tasks_sonar}},
\code{\link{mlr_tasks_spam}},
\code{\link{mlr_tasks_wine}},
\code{\link{mlr_tasks_zoo}},
\code{\link{mlr_tasks}}
}
\concept{Task}
