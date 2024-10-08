% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MeasureSimple.R
\name{mlr_measures_classif.acc}
\alias{mlr_measures_classif.acc}
\title{Classification Accuracy}
\description{
Measure to compare true observed labels with predicted
labels
in multiclass classification tasks.
}
\details{
The Classification Accuracy is defined as
\deqn{
  \frac{1}{n} \sum_{i=1}^n w_i \mathbf{1} \left( t_i = r_i \right),
}{1 / n * sum(wi * 1(ti = ri))}
where \eqn{w_i} are normalized weights for all observations \eqn{x_i}.
}
\note{
The score function calls \code{\link[mlr3measures:acc]{mlr3measures::acc()}} from package \CRANpkg{mlr3measures}.

If the measure is undefined for the input, \code{NaN} is returned.
This can be customized by setting the field \code{na_value}.
}
\section{Dictionary}{

This \link{Measure} can be instantiated via the \link[mlr3misc:Dictionary]{dictionary} \link{mlr_measures} or with the associated sugar function \code{\link[=msr]{msr()}}:

\if{html}{\out{<div class="sourceCode">}}\preformatted{mlr_measures$get("classif.acc")
msr("classif.acc")
}\if{html}{\out{</div>}}
}

\section{Parameters}{

Empty ParamSet
}

\section{Meta Information}{


\itemize{
\item Type: \code{"classif"}
\item Range: \eqn{[0, 1]}{[0, 1]}
\item Minimize: \code{FALSE}
\item Required prediction: \code{response}
}

}

\seealso{
\link[mlr3misc:Dictionary]{Dictionary} of \link[=Measure]{Measures}: \link{mlr_measures}

\code{as.data.table(mlr_measures)} for a complete table of all (also dynamically created) \link{Measure} implementations.

Other classification measures: 
\code{\link{mlr_measures_classif.auc}},
\code{\link{mlr_measures_classif.bacc}},
\code{\link{mlr_measures_classif.bbrier}},
\code{\link{mlr_measures_classif.ce}},
\code{\link{mlr_measures_classif.costs}},
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
\code{\link{mlr_measures_classif.bacc}},
\code{\link{mlr_measures_classif.ce}},
\code{\link{mlr_measures_classif.costs}},
\code{\link{mlr_measures_classif.logloss}},
\code{\link{mlr_measures_classif.mauc_au1p}},
\code{\link{mlr_measures_classif.mauc_au1u}},
\code{\link{mlr_measures_classif.mauc_aunp}},
\code{\link{mlr_measures_classif.mauc_aunu}},
\code{\link{mlr_measures_classif.mauc_mu}},
\code{\link{mlr_measures_classif.mbrier}},
\code{\link{mlr_measures_classif.mcc}}
}
\concept{classification measures}
\concept{multiclass classification measures}
