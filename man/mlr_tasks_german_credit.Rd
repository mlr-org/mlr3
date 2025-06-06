% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TaskClassif_german_credit.R
\name{mlr_tasks_german_credit}
\alias{mlr_tasks_german_credit}
\title{German Credit Classification Task}
\format{
\link[R6:R6Class]{R6::R6Class} inheriting from \link{TaskClassif}.
}
\source{
Data set originally published on \href{http://archive.ics.uci.edu/dataset/144/statlog+german+credit+data}{UCI}.
This is the preprocessed version taken from package \CRANpkg{rchallenge} with
factors instead of dummy variables, and corrected as proposed by Ulrike
Grömping.

Donor:
Professor Dr. Hans Hofmann\cr
Institut für Statistik und Ökonometrie\cr
Universität Hamburg\cr
FB Wirtschaftswissenschaften\cr
Von-Melle-Park 5\cr
2000 Hamburg 13
}
\description{
A classification task for the German credit data set.
The aim is to predict creditworthiness, labeled as "good" and "bad".
Positive class is set to label "good".

See example for the creation of a \link{MeasureClassifCosts} as described misclassification costs.
}
\section{Dictionary}{

This \link{Task} can be instantiated via the \link[mlr3misc:Dictionary]{dictionary} \link{mlr_tasks} or with the associated sugar function \code{\link[=tsk]{tsk()}}:

\if{html}{\out{<div class="sourceCode">}}\preformatted{mlr_tasks$get("german_credit")
tsk("german_credit")
}\if{html}{\out{</div>}}
}

\section{Meta Information}{

\itemize{
\item Task type: \dQuote{classif}
\item Dimensions: 1000x21
\item Properties: \dQuote{twoclass}
\item Has Missings: \code{FALSE}
\item Target: \dQuote{credit_risk}
\item Features: \dQuote{age}, \dQuote{amount}, \dQuote{credit_history}, \dQuote{duration}, \dQuote{employment_duration}, \dQuote{foreign_worker}, \dQuote{housing}, \dQuote{installment_rate}, \dQuote{job}, \dQuote{number_credits}, \dQuote{other_debtors}, \dQuote{other_installment_plans}, \dQuote{people_liable}, \dQuote{personal_status_sex}, \dQuote{present_residence}, \dQuote{property}, \dQuote{purpose}, \dQuote{savings}, \dQuote{status}, \dQuote{telephone}
}
}

\examples{
task = tsk("german_credit")
costs = matrix(c(0, 1, 5, 0), nrow = 2)
dimnames(costs) = list(predicted = task$class_names, truth = task$class_names)
measure = msr("classif.costs", id = "german_credit_costs", costs = costs)
print(measure)
}
\references{
Grömping U (2019).
\dQuote{South German Credit Data: Correcting a Widely Used Data Set.}
Reports in Mathematics, Physics and Chemistry 4, Department II, Beuth University of Applied Sciences Berlin.
}
\seealso{
\itemize{
\item Chapter in the \href{https://mlr3book.mlr-org.com/}{mlr3book}:
\url{https://mlr3book.mlr-org.com/chapters/chapter2/data_and_basic_modeling.html}
\item Package \CRANpkg{mlr3data} for more toy tasks.
\item Package \CRANpkg{mlr3oml} for downloading tasks from \url{https://www.openml.org}.
\item Package \CRANpkg{mlr3viz} for some generic visualizations.
\item \link[mlr3misc:Dictionary]{Dictionary} of \link[=Task]{Tasks}: \link{mlr_tasks}
\item \code{as.data.table(mlr_tasks)} for a table of available \link[=Task]{Tasks} in the running session (depending on the loaded packages).
\item \CRANpkg{mlr3fselect} and \CRANpkg{mlr3filters} for feature selection and feature filtering.
\item Extension packages for additional task types:
\itemize{
\item Unsupervised clustering: \CRANpkg{mlr3cluster}
\item Probabilistic supervised regression and survival analysis: \url{https://mlr3proba.mlr-org.com/}.
}
}

Other Task: 
\code{\link{Task}},
\code{\link{TaskClassif}},
\code{\link{TaskRegr}},
\code{\link{TaskSupervised}},
\code{\link{TaskUnsupervised}},
\code{\link{california_housing}},
\code{\link{mlr_tasks}},
\code{\link{mlr_tasks_breast_cancer}},
\code{\link{mlr_tasks_iris}},
\code{\link{mlr_tasks_mtcars}},
\code{\link{mlr_tasks_penguins}},
\code{\link{mlr_tasks_pima}},
\code{\link{mlr_tasks_sonar}},
\code{\link{mlr_tasks_spam}},
\code{\link{mlr_tasks_wine}},
\code{\link{mlr_tasks_zoo}}
}
\concept{Task}
