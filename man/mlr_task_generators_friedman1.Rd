% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TaskGeneratorFriedman1.R
\name{mlr_task_generators_friedman1}
\alias{mlr_task_generators_friedman1}
\alias{TaskGeneratorFriedman1}
\title{Friedman1 Regression Task Generator}
\description{
A \link{TaskGenerator} for the friedman1 task in \code{\link[mlbench:mlbench.friedman1]{mlbench::mlbench.friedman1()}}.
}
\section{Dictionary}{

This \link{TaskGenerator} can be instantiated via the \link[mlr3misc:Dictionary]{dictionary} \link{mlr_task_generators} or with the associated sugar function \code{\link[=tgen]{tgen()}}:

\if{html}{\out{<div class="sourceCode">}}\preformatted{mlr_task_generators$get("friedman1")
tgen("friedman1")
}\if{html}{\out{</div>}}
}

\section{Parameters}{
\tabular{llll}{
   Id \tab Type \tab Default \tab Range \cr
   sd \tab numeric \tab 1 \tab \eqn{[0, \infty)}{[0, Inf)} \cr
}
}

\examples{
generator = tgen("friedman1")
task = generator$generate(200)
str(task$data())
}
\seealso{
\itemize{
\item \link[mlr3misc:Dictionary]{Dictionary} of \link[=TaskGenerator]{TaskGenerators}: \link{mlr_task_generators}
\item \code{as.data.table(mlr_task_generators)}  for a table of available \link[=TaskGenerator]{TaskGenerators} in the running session (depending on the loaded packages).
\item Extension packages for additional task types:
\itemize{
\item \CRANpkg{mlr3proba} for probabilistic supervised regression and survival analysis.
\item \CRANpkg{mlr3cluster} for unsupervised clustering.
}
}

Other TaskGenerator: 
\code{\link{TaskGenerator}},
\code{\link{mlr_task_generators}},
\code{\link{mlr_task_generators_2dnormals}},
\code{\link{mlr_task_generators_cassini}},
\code{\link{mlr_task_generators_circle}},
\code{\link{mlr_task_generators_moons}},
\code{\link{mlr_task_generators_peak}},
\code{\link{mlr_task_generators_simplex}},
\code{\link{mlr_task_generators_smiley}},
\code{\link{mlr_task_generators_spirals}},
\code{\link{mlr_task_generators_xor}}
}
\concept{TaskGenerator}
\section{Super class}{
\code{\link[mlr3:TaskGenerator]{mlr3::TaskGenerator}} -> \code{TaskGeneratorFriedman1}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-TaskGeneratorFriedman1-new}{\code{TaskGeneratorFriedman1$new()}}
\item \href{#method-TaskGeneratorFriedman1-clone}{\code{TaskGeneratorFriedman1$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="mlr3" data-topic="TaskGenerator" data-id="format"><a href='../../mlr3/html/TaskGenerator.html#method-TaskGenerator-format'><code>mlr3::TaskGenerator$format()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3" data-topic="TaskGenerator" data-id="generate"><a href='../../mlr3/html/TaskGenerator.html#method-TaskGenerator-generate'><code>mlr3::TaskGenerator$generate()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3" data-topic="TaskGenerator" data-id="print"><a href='../../mlr3/html/TaskGenerator.html#method-TaskGenerator-print'><code>mlr3::TaskGenerator$print()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TaskGeneratorFriedman1-new"></a>}}
\if{latex}{\out{\hypertarget{method-TaskGeneratorFriedman1-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of this \link[R6:R6Class]{R6} class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TaskGeneratorFriedman1$new()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TaskGeneratorFriedman1-clone"></a>}}
\if{latex}{\out{\hypertarget{method-TaskGeneratorFriedman1-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TaskGeneratorFriedman1$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
