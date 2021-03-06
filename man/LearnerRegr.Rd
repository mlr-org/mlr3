% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LearnerRegr.R
\name{LearnerRegr}
\alias{LearnerRegr}
\title{Regression Learner}
\description{
This Learner specializes \link{Learner} for regression problems:
\itemize{
\item \code{task_type} is set to \code{"regr"}.
\item Creates \link{Prediction}s of class \link{PredictionRegr}.
\item Possible values for \code{predict_types} are:
\itemize{
\item \code{"response"}: Predicts a numeric response for each observation in the test set.
\item \code{"se"}: Predicts the standard error for each value of response for each observation in the test set.
\item \code{"distr"}: Probability distribution as \link[distr6:VectorDistribution]{distr6::VectorDistribution} object (requires package \CRANpkg{distr6}).
}
}

Predefined learners can be found in the \link[mlr3misc:Dictionary]{dictionary} \link{mlr_learners}.
Essential regression learners can be found in this dictionary after loading \CRANpkg{mlr3learners}.
Additional learners are implement in the Github package \url{https://github.com/mlr-org/mlr3extralearners}.
}
\examples{
# get all regression learners from mlr_learners:
lrns = mlr_learners$mget(mlr_learners$keys("^regr"))
names(lrns)

# get a specific learner from mlr_learners:
mlr_learners$get("regr.rpart")
lrn("classif.featureless")
}
\seealso{
\itemize{
\item Chapter in the \href{https://mlr3book.mlr-org.com/}{mlr3book}:
\url{https://mlr3book.mlr-org.com/learners.html}
\item Package \CRANpkg{mlr3learners} for a solid collection of essential learners.
\item Package \href{https://github.com/mlr-org/mlr3extralearners}{mlr3extralearners} for more learners.
\item \link[mlr3misc:Dictionary]{Dictionary} of \link[=Learner]{Learners}: \link{mlr_learners}
\item \code{as.data.table(mlr_learners)} for a table of available \link[=Learner]{Learners} in the running session (depending on the loaded packages).
\item Extension packages for additional task types:
\itemize{
\item \CRANpkg{mlr3proba} for probabilistic supervised regression and survival analysis.
\item \CRANpkg{mlr3cluster} for unsupervised clustering.
}
}

Other Learner: 
\code{\link{LearnerClassif}},
\code{\link{Learner}},
\code{\link{mlr_learners_classif.debug}},
\code{\link{mlr_learners_classif.featureless}},
\code{\link{mlr_learners_classif.rpart}},
\code{\link{mlr_learners_regr.featureless}},
\code{\link{mlr_learners_regr.rpart}},
\code{\link{mlr_learners}}
}
\concept{Learner}
\section{Super class}{
\code{\link[mlr3:Learner]{mlr3::Learner}} -> \code{LearnerRegr}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{LearnerRegr$new()}}
\item \href{#method-clone}{\code{LearnerRegr$clone()}}
}
}
\if{html}{
\out{<details ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="base_learner">}\href{../../mlr3/html/Learner.html#method-base_learner}{\code{mlr3::Learner$base_learner()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="format">}\href{../../mlr3/html/Learner.html#method-format}{\code{mlr3::Learner$format()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="help">}\href{../../mlr3/html/Learner.html#method-help}{\code{mlr3::Learner$help()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="predict">}\href{../../mlr3/html/Learner.html#method-predict}{\code{mlr3::Learner$predict()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="predict_newdata">}\href{../../mlr3/html/Learner.html#method-predict_newdata}{\code{mlr3::Learner$predict_newdata()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="print">}\href{../../mlr3/html/Learner.html#method-print}{\code{mlr3::Learner$print()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="reset">}\href{../../mlr3/html/Learner.html#method-reset}{\code{mlr3::Learner$reset()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="mlr3" data-topic="Learner" data-id="train">}\href{../../mlr3/html/Learner.html#method-train}{\code{mlr3::Learner$train()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of this \link[R6:R6Class]{R6} class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LearnerRegr$new(
  id,
  param_set = ps(),
  predict_types = "response",
  feature_types = character(),
  properties = character(),
  data_formats = "data.table",
  packages = character(),
  man = NA_character_
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{id}}{(\code{character(1)})\cr
Identifier for the new instance.}

\item{\code{param_set}}{(\link[paradox:ParamSet]{paradox::ParamSet})\cr
Set of hyperparameters.}

\item{\code{predict_types}}{(\code{character()})\cr
Supported predict types. Must be a subset of \code{\link[=mlr_reflections]{mlr_reflections$learner_predict_types}}.}

\item{\code{feature_types}}{(\code{character()})\cr
Feature types the learner operates on. Must be a subset of \code{\link[=mlr_reflections]{mlr_reflections$task_feature_types}}.}

\item{\code{properties}}{(\code{character()})\cr
Set of properties of the \link{Learner}.
Must be a subset of \code{\link[=mlr_reflections]{mlr_reflections$learner_properties}}.
The following properties are currently standardized and understood by learners in \CRANpkg{mlr3}:
\itemize{
\item \code{"missings"}: The learner can handle missing values in the data.
\item \code{"weights"}: The learner supports observation weights.
\item \code{"importance"}: The learner supports extraction of importance scores, i.e. comes with an \verb{$importance()} extractor function (see section on optional extractors in \link{Learner}).
\item \code{"selected_features"}: The learner supports extraction of the set of selected features, i.e. comes with a \verb{$selected_features()} extractor function (see section on optional extractors in \link{Learner}).
\item \code{"oob_error"}: The learner supports extraction of estimated out of bag error, i.e. comes with a \code{oob_error()} extractor function (see section on optional extractors in \link{Learner}).
}}

\item{\code{data_formats}}{(\code{character()})\cr
Set of supported data formats which can be processed during \verb{$train()} and \verb{$predict()},
e.g. \code{"data.table"}.}

\item{\code{packages}}{(\code{character()})\cr
Set of required packages.
A warning is signaled by the constructor if at least one of the packages is not installed,
but loaded (not attached) later on-demand via \code{\link[=requireNamespace]{requireNamespace()}}.}

\item{\code{man}}{(\code{character(1)})\cr
String in the format \verb{[pkg]::[topic]} pointing to a manual page for this object.
The referenced help package can be opened via method \verb{$help()}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LearnerRegr$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
