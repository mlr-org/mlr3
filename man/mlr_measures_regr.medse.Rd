% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MeasureSimple.R
\name{mlr_measures_regr.medse}
\alias{mlr_measures_regr.medse}
\title{Median Squared Error}
\description{
Measure to compare true observed response with predicted response in regression tasks.
}
\details{
The Median Squared Error is defined as \deqn{
  \mathop{\mathrm{median}} \left[ \left( t_i - r_i \right)^2 \right].
}{
  median((t - r)^2).
}
}
\note{
The score function calls \code{\link[mlr3measures:medse]{mlr3measures::medse()}} from package \CRANpkg{mlr3measures}.

If the measure is undefined for the input, \code{NaN} is returned.
This can be customized by setting the field \code{na_value}.
}
\section{Dictionary}{

This \link{Measure} can be instantiated via the \link[mlr3misc:Dictionary]{dictionary} \link{mlr_measures} or with the associated sugar function \code{\link[=msr]{msr()}}:

\if{html}{\out{<div class="sourceCode">}}\preformatted{mlr_measures$get("regr.medse")
msr("regr.medse")
}\if{html}{\out{</div>}}
}

\section{Parameters}{

Empty ParamSet
}

\section{Meta Information}{


\itemize{
\item Type: \code{"regr"}
\item Range: \eqn{[0, \infty)}{[0, Inf)}
\item Minimize: \code{TRUE}
\item Required prediction: \code{response}
}

}

\seealso{
\link[mlr3misc:Dictionary]{Dictionary} of \link[=Measure]{Measures}: \link{mlr_measures}

\code{as.data.table(mlr_measures)} for a complete table of all (also dynamically created) \link{Measure} implementations.

Other regression measures: 
\code{\link{mlr_measures_regr.bias}},
\code{\link{mlr_measures_regr.ktau}},
\code{\link{mlr_measures_regr.mae}},
\code{\link{mlr_measures_regr.mape}},
\code{\link{mlr_measures_regr.maxae}},
\code{\link{mlr_measures_regr.medae}},
\code{\link{mlr_measures_regr.mse}},
\code{\link{mlr_measures_regr.msle}},
\code{\link{mlr_measures_regr.pbias}},
\code{\link{mlr_measures_regr.rmse}},
\code{\link{mlr_measures_regr.rmsle}},
\code{\link{mlr_measures_regr.sae}},
\code{\link{mlr_measures_regr.smape}},
\code{\link{mlr_measures_regr.srho}},
\code{\link{mlr_measures_regr.sse}}
}
\concept{regression measures}
