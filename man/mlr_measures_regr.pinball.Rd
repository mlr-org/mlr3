% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MeasureRegrPinball.R
\name{mlr_measures_regr.pinball}
\alias{mlr_measures_regr.pinball}
\alias{MeasureRegrPinball}
\title{Average Pinball Loss}
\description{
Measure to compare true observed response with predicted response in regression tasks.
}
\details{
The pinball loss for quantile regression is defined as \deqn{
\text{Average Pinball Loss} = \frac{1}{n} \sum_{i=1}^{n} w_{i}
\begin{cases}
q \cdot (t_i - r_i) & \text{if } t_i \geq r_i \\
(1 - q) \cdot (r_i - t_i) & \text{if } t_i < r_i
\end{cases}
}
where \eqn{q} is the quantile and \eqn{w_i} are normalized sample weights.
}
\section{Dictionary}{

This \link{Measure} can be instantiated via the \link[mlr3misc:Dictionary]{dictionary} \link{mlr_measures} or with the associated sugar function \code{\link[=msr]{msr()}}:

\if{html}{\out{<div class="sourceCode">}}\preformatted{mlr_measures$get("regr.pinball")
msr("regr.pinball")
}\if{html}{\out{</div>}}
}

\section{Meta Information}{

\itemize{
\item Task type: \dQuote{regr}
\item Range: \eqn{(-\infty, \infty)}{(-Inf, Inf)}
\item Minimize: TRUE
\item Average: macro
\item Required Prediction: \dQuote{quantiles}
\item Required Packages: \CRANpkg{mlr3}
}
}

\section{Parameters}{
\tabular{llll}{
   Id \tab Type \tab Default \tab Range \cr
   alpha \tab numeric \tab - \tab \eqn{[0, 1]}{[0, 1]} \cr
}
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
\code{\link{mlr_measures_classif.costs}},
\code{\link{mlr_measures_debug_classif}},
\code{\link{mlr_measures_elapsed_time}},
\code{\link{mlr_measures_internal_valid_score}},
\code{\link{mlr_measures_oob_error}},
\code{\link{mlr_measures_regr.rqr}},
\code{\link{mlr_measures_regr.rsq}},
\code{\link{mlr_measures_selected_features}}
}
\concept{Measure}
\section{Super classes}{
\code{\link[mlr3:Measure]{mlr3::Measure}} -> \code{\link[mlr3:MeasureRegr]{mlr3::MeasureRegr}} -> \code{MeasureRegrPinball}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-MeasureRegrPinball-new}{\code{MeasureRegrPinball$new()}}
\item \href{#method-MeasureRegrPinball-clone}{\code{MeasureRegrPinball$clone()}}
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
\if{html}{\out{<a id="method-MeasureRegrPinball-new"></a>}}
\if{latex}{\out{\hypertarget{method-MeasureRegrPinball-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of this \link[R6:R6Class]{R6} class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MeasureRegrPinball$new(alpha = 0.5)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{alpha}}{\code{numeric(1)}\cr
The quantile to compute the pinball loss.
Must be one of the quantiles that the Learner was trained on.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MeasureRegrPinball-clone"></a>}}
\if{latex}{\out{\hypertarget{method-MeasureRegrPinball-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MeasureRegrPinball$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
