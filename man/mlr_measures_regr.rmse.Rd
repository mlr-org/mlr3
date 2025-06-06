% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MeasureSimple.R
\name{mlr_measures_regr.rmse}
\alias{mlr_measures_regr.rmse}
\title{Root Mean Squared Error}
\description{
Measure to compare true observed response with predicted response in regression tasks.
}
\details{
The Root Mean Squared Error is defined as \deqn{
  \sqrt{\frac{1}{n} \sum_{i=1}^n w_i \left( t_i - r_i \right)^2},
}{
  sqrt(weighted.mean((t - r)^2, w)),
}
where \eqn{w_i} are normalized sample weights.
}
\note{
The score function calls \code{\link[mlr3measures:rmse]{mlr3measures::rmse()}} from package \CRANpkg{mlr3measures}.

If the measure is undefined for the input, \code{NaN} is returned.
This can be customized by setting the field \code{na_value}.
}
\section{Dictionary}{

This \link{Measure} can be instantiated via the \link[mlr3misc:Dictionary]{dictionary} \link{mlr_measures} or with the associated sugar function \code{\link[=msr]{msr()}}:

\if{html}{\out{<div class="sourceCode">}}\preformatted{mlr_measures$get("regr.rmse")
msr("regr.rmse")
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
\code{\link{mlr_measures_regr.medse}},
\code{\link{mlr_measures_regr.mse}},
\code{\link{mlr_measures_regr.msle}},
\code{\link{mlr_measures_regr.pbias}},
\code{\link{mlr_measures_regr.rmsle}},
\code{\link{mlr_measures_regr.sae}},
\code{\link{mlr_measures_regr.smape}},
\code{\link{mlr_measures_regr.srho}},
\code{\link{mlr_measures_regr.sse}}
}
\concept{regression measures}
