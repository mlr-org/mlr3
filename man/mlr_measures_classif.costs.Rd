% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MeasureClassifCosts.R
\name{mlr_measures_classif.costs}
\alias{mlr_measures_classif.costs}
\alias{MeasureClassifCosts}
\title{Cost-sensitive Classification Measure}
\description{
Uses a cost matrix to create a classification measure.
True labels must be arranged in columns, predicted labels must be arranged  in rows.
The cost matrix is stored as slot \verb{$costs}.

For calculation of the score, the confusion matrix is multiplied element-wise with the cost matrix.
The costs are then summed up (and potentially divided by the number of observations if \code{normalize} is set to \code{TRUE} (default)).
}
\section{Dictionary}{

This \link{Measure} can be instantiated via the \link[mlr3misc:Dictionary]{dictionary} \link{mlr_measures} or with the associated sugar function \code{\link[=msr]{msr()}}:

\if{html}{\out{<div class="sourceCode">}}\preformatted{mlr_measures$get("classif.costs")
msr("classif.costs")
}\if{html}{\out{</div>}}
}

\section{Meta Information}{

\itemize{
\item Task type: \dQuote{classif}
\item Range: \eqn{(-\infty, \infty)}{(-Inf, Inf)}
\item Minimize: TRUE
\item Average: macro
\item Required Prediction: \dQuote{response}
\item Required Packages: \CRANpkg{mlr3}
}
}

\section{Parameters}{
\tabular{llll}{
   Id \tab Type \tab Default \tab Levels \cr
   normalize \tab logical \tab - \tab TRUE, FALSE \cr
}
}

\examples{
# get a cost sensitive task
task = tsk("german_credit")

# cost matrix as given on the UCI page of the german credit data set
# https://archive.ics.uci.edu/ml/datasets/statlog+(german+credit+data)
costs = matrix(c(0, 5, 1, 0), nrow = 2)
dimnames(costs) = list(truth = task$class_names, predicted = task$class_names)
print(costs)

# mlr3 needs truth in columns, predictions in rows
costs = t(costs)

# create a cost measure which calculates the absolute costs
m = msr("classif.costs", id = "german_credit_costs", costs = costs, normalize = FALSE)

# fit models and evaluate with the cost measure
learner = lrn("classif.rpart")
rr = resample(task, learner, rsmp("cv", folds = 3))
rr$aggregate(m)
}
\seealso{
\itemize{
\item Chapter in the \href{https://mlr3book.mlr-org.com/}{mlr3book}:
\url{https://mlr3book.mlr-org.com/chapters/chapter2/data_and_basic_modeling.html#sec-eval}
\item Package \CRANpkg{mlr3measures} for the scoring functions.
\link[mlr3misc:Dictionary]{Dictionary} of \link[=Measure]{Measures}: \link{mlr_measures}
\code{as.data.table(mlr_measures)} for a table of available \link[=Measure]{Measures} in the running session (depending on the loaded packages).
\item Extension packages for additional task types:
\itemize{
\item \CRANpkg{mlr3proba} for probabilistic supervised regression and survival analysis.
\item \CRANpkg{mlr3cluster} for unsupervised clustering.
}
}

Other Measure: 
\code{\link{Measure}},
\code{\link{MeasureClassif}},
\code{\link{MeasureRegr}},
\code{\link{MeasureSimilarity}},
\code{\link{mlr_measures}},
\code{\link{mlr_measures_aic}},
\code{\link{mlr_measures_bic}},
\code{\link{mlr_measures_debug_classif}},
\code{\link{mlr_measures_elapsed_time}},
\code{\link{mlr_measures_internal_valid_score}},
\code{\link{mlr_measures_oob_error}},
\code{\link{mlr_measures_regr.pinball}},
\code{\link{mlr_measures_regr.rqr}},
\code{\link{mlr_measures_regr.rsq}},
\code{\link{mlr_measures_selected_features}}

Other classification measures: 
\code{\link{mlr_measures_classif.acc}},
\code{\link{mlr_measures_classif.auc}},
\code{\link{mlr_measures_classif.bacc}},
\code{\link{mlr_measures_classif.bbrier}},
\code{\link{mlr_measures_classif.ce}},
\code{\link{mlr_measures_classif.dor}},
\code{\link{mlr_measures_classif.fbeta}},
\code{\link{mlr_measures_classif.fdr}},
\code{\link{mlr_measures_classif.fn}},
\code{\link{mlr_measures_classif.fnr}},
\code{\link{mlr_measures_classif.fomr}},
\code{\link{mlr_measures_classif.fp}},
\code{\link{mlr_measures_classif.fpr}},
\code{\link{mlr_measures_classif.logloss}},
\code{\link{mlr_measures_classif.mauc_au1p}},
\code{\link{mlr_measures_classif.mauc_au1u}},
\code{\link{mlr_measures_classif.mauc_aunp}},
\code{\link{mlr_measures_classif.mauc_aunu}},
\code{\link{mlr_measures_classif.mauc_mu}},
\code{\link{mlr_measures_classif.mbrier}},
\code{\link{mlr_measures_classif.mcc}},
\code{\link{mlr_measures_classif.npv}},
\code{\link{mlr_measures_classif.ppv}},
\code{\link{mlr_measures_classif.prauc}},
\code{\link{mlr_measures_classif.precision}},
\code{\link{mlr_measures_classif.recall}},
\code{\link{mlr_measures_classif.sensitivity}},
\code{\link{mlr_measures_classif.specificity}},
\code{\link{mlr_measures_classif.tn}},
\code{\link{mlr_measures_classif.tnr}},
\code{\link{mlr_measures_classif.tp}},
\code{\link{mlr_measures_classif.tpr}}

Other multiclass classification measures: 
\code{\link{mlr_measures_classif.acc}},
\code{\link{mlr_measures_classif.bacc}},
\code{\link{mlr_measures_classif.ce}},
\code{\link{mlr_measures_classif.logloss}},
\code{\link{mlr_measures_classif.mauc_au1p}},
\code{\link{mlr_measures_classif.mauc_au1u}},
\code{\link{mlr_measures_classif.mauc_aunp}},
\code{\link{mlr_measures_classif.mauc_aunu}},
\code{\link{mlr_measures_classif.mauc_mu}},
\code{\link{mlr_measures_classif.mbrier}},
\code{\link{mlr_measures_classif.mcc}}
}
\concept{Measure}
\concept{classification measures}
\concept{multiclass classification measures}
\section{Super classes}{
\code{\link[mlr3:Measure]{mlr3::Measure}} -> \code{\link[mlr3:MeasureClassif]{mlr3::MeasureClassif}} -> \code{MeasureClassifCosts}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{costs}}{(numeric \code{matrix()})\cr
Matrix of costs (truth in columns, predicted response in rows).}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-MeasureClassifCosts-new}{\code{MeasureClassifCosts$new()}}
\item \href{#method-MeasureClassifCosts-clone}{\code{MeasureClassifCosts$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="mlr3" data-topic="Measure" data-id="aggregate"><a href='../../mlr3/html/Measure.html#method-Measure-aggregate'><code>mlr3::Measure$aggregate()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3" data-topic="Measure" data-id="format"><a href='../../mlr3/html/Measure.html#method-Measure-format'><code>mlr3::Measure$format()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3" data-topic="Measure" data-id="help"><a href='../../mlr3/html/Measure.html#method-Measure-help'><code>mlr3::Measure$help()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3" data-topic="Measure" data-id="print"><a href='../../mlr3/html/Measure.html#method-Measure-print'><code>mlr3::Measure$print()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3" data-topic="Measure" data-id="score"><a href='../../mlr3/html/Measure.html#method-Measure-score'><code>mlr3::Measure$score()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MeasureClassifCosts-new"></a>}}
\if{latex}{\out{\hypertarget{method-MeasureClassifCosts-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of this \link[R6:R6Class]{R6} class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MeasureClassifCosts$new()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MeasureClassifCosts-clone"></a>}}
\if{latex}{\out{\hypertarget{method-MeasureClassifCosts-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MeasureClassifCosts$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
