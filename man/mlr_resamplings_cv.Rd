% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ResamplingCV.R
\name{mlr_resamplings_cv}
\alias{mlr_resamplings_cv}
\alias{ResamplingCV}
\title{Cross-Validation Resampling}
\description{
Splits data using a \code{folds}-folds (default: 10 folds) cross-validation.
}
\section{Dictionary}{

This \link{Resampling} can be instantiated via the \link[mlr3misc:Dictionary]{dictionary} \link{mlr_resamplings} or with the associated sugar function \code{\link[=rsmp]{rsmp()}}:\preformatted{mlr_resamplings$get("cv")
rsmp("cv")
}
}

\section{Parameters}{

\itemize{
\item \code{folds} (\code{integer(1)})\cr
Number of folds.
}
}

\examples{
# Create a task with 10 observations
task = tsk("penguins")
task$filter(1:10)

# Instantiate Resampling
cv = rsmp("cv", folds = 3)
cv$instantiate(task)

# Individual sets:
cv$train_set(1)
cv$test_set(1)

# Disjunct sets:
intersect(cv$train_set(1), cv$test_set(1))

# Internal storage:
cv$instance # table
}
\references{
Bischl B, Mersmann O, Trautmann H, Weihs C (2012).
\dQuote{Resampling Methods for Meta-Model Validation with Recommendations for Evolutionary Computation.}
\emph{Evolutionary Computation}, \bold{20}(2), 249--275.
\doi{10.1162/evco_a_00069}.
}
\seealso{
\itemize{
\item Chapter in the \href{https://mlr3book.mlr-org.com/}{mlr3book}:
\url{https://mlr3book.mlr-org.com/resampling.html}
\item Package \CRANpkg{mlr3spatiotempcv} for spatio-temporal resamplings.
\item \link[mlr3misc:Dictionary]{Dictionary} of \link[=Resampling]{Resamplings}: \link{mlr_resamplings}
\item \code{as.data.table(mlr_resamplings)} for a table of available \link[=Resampling]{Resamplings} in the running session (depending on the loaded packages).
}

Other Resampling: 
\code{\link{Resampling}},
\code{\link{mlr_resamplings_bootstrap}},
\code{\link{mlr_resamplings_custom_cv}},
\code{\link{mlr_resamplings_custom}},
\code{\link{mlr_resamplings_holdout}},
\code{\link{mlr_resamplings_insample}},
\code{\link{mlr_resamplings_loo}},
\code{\link{mlr_resamplings_repeated_cv}},
\code{\link{mlr_resamplings_subsampling}},
\code{\link{mlr_resamplings}}
}
\concept{Resampling}
\section{Super class}{
\code{\link[mlr3:Resampling]{mlr3::Resampling}} -> \code{ResamplingCV}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{iters}}{(\code{integer(1)})\cr
Returns the number of resampling iterations, depending on the values stored in the \code{param_set}.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{ResamplingCV$new()}}
\item \href{#method-clone}{\code{ResamplingCV$clone()}}
}
}
\if{html}{
\out{<details ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Resampling" data-id="format">}\href{../../mlr3/html/Resampling.html#method-format}{\code{mlr3::Resampling$format()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Resampling" data-id="help">}\href{../../mlr3/html/Resampling.html#method-help}{\code{mlr3::Resampling$help()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Resampling" data-id="instantiate">}\href{../../mlr3/html/Resampling.html#method-instantiate}{\code{mlr3::Resampling$instantiate()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Resampling" data-id="print">}\href{../../mlr3/html/Resampling.html#method-print}{\code{mlr3::Resampling$print()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Resampling" data-id="test_set">}\href{../../mlr3/html/Resampling.html#method-test_set}{\code{mlr3::Resampling$test_set()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Resampling" data-id="train_set">}\href{../../mlr3/html/Resampling.html#method-train_set}{\code{mlr3::Resampling$train_set()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of this \link[R6:R6Class]{R6} class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ResamplingCV$new()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ResamplingCV$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
