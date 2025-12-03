# Matthews Correlation Coefficient

Measure to compare true observed labels with predicted labels in
multiclass classification tasks.

## Details

In the binary case, the Matthews Correlation Coefficient is defined as
\$\$ \frac{\mathrm{TP} \cdot \mathrm{TN} - \mathrm{FP} \cdot
\mathrm{FN}}{\sqrt{(\mathrm{TP} + \mathrm{FP}) (\mathrm{TP} +
\mathrm{FN}) (\mathrm{TN} + \mathrm{FP}) (\mathrm{TN} + \mathrm{FN})}},
\$\$ where \\TP\\, \\FP\\, \\TN\\, \\TP\\ are the number of true
positives, false positives, true negatives, and false negatives
respectively.

In the multi-class case, the Matthews Correlation Coefficient is defined
for a multi-class confusion matrix \\C\\ with \\K\\ classes: \$\$
\frac{c \cdot s - \sum_k^K p_k \cdot t_k}{\sqrt{(s^2 - \sum_k^K p_k^2)
\cdot (s^2 - \sum_k^K t_k^2)}}, \$\$ where

- \\s = \sum_i^K \sum_j^K C\_{ij}\\: total number of samples

- \\c = \sum_k^K C\_{kk}\\: total number of correctly predicted samples

- \\t_k = \sum_i^K C\_{ik}\\: number of predictions for each class \\k\\

- \\p_k = \sum_j^K C\_{kj}\\: number of true occurrences for each class
  \\k\\.

The above formula is undefined if any of the four sums in the
denominator is 0 in the binary case and more generally if either \\s^2 -
\sum_k^K p_k^2\\ or \\s^2 - \sum_k^K t_k^2)\\ is equal to 0. The
denominator is then set to 1.

When there are more than two classes, the MCC will no longer range
between -1 and +1. Instead, the minimum value will be between -1 and 0
depending on the true distribution. The maximum value is always +1.

## Note

The score function calls
[`mlr3measures::mcc()`](https://mlr3measures.mlr-org.com/reference/mcc.html)
from package
[mlr3measures](https://CRAN.R-project.org/package=mlr3measures).

If the measure is undefined for the input, `NaN` is returned. This can
be customized by setting the field `na_value`.

## Dictionary

This [Measure](https://mlr3.mlr-org.com/reference/Measure.md) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_measures](https://mlr3.mlr-org.com/reference/mlr_measures.md) or
with the associated sugar function
[`msr()`](https://mlr3.mlr-org.com/reference/mlr_sugar.md):

    mlr_measures$get("classif.mcc")
    msr("classif.mcc")

## Parameters

Empty ParamSet

## Meta Information

- Type: `"classif"`

- Range: \\\[-1, 1\]\\

- Minimize: `FALSE`

- Required prediction: `response`

## See also

[Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html) of
[Measures](https://mlr3.mlr-org.com/reference/Measure.md):
[mlr_measures](https://mlr3.mlr-org.com/reference/mlr_measures.md)

`as.data.table(mlr_measures)` for a complete table of all (also
dynamically created)
[Measure](https://mlr3.mlr-org.com/reference/Measure.md)
implementations.

Other classification measures:
[`mlr_measures_classif.acc`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.acc.md),
[`mlr_measures_classif.auc`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.auc.md),
[`mlr_measures_classif.bacc`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.bacc.md),
[`mlr_measures_classif.bbrier`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.bbrier.md),
[`mlr_measures_classif.ce`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.ce.md),
[`mlr_measures_classif.costs`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.costs.md),
[`mlr_measures_classif.dor`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.dor.md),
[`mlr_measures_classif.fbeta`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.fbeta.md),
[`mlr_measures_classif.fdr`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.fdr.md),
[`mlr_measures_classif.fn`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.fn.md),
[`mlr_measures_classif.fnr`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.fnr.md),
[`mlr_measures_classif.fomr`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.fomr.md),
[`mlr_measures_classif.fp`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.fp.md),
[`mlr_measures_classif.fpr`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.fpr.md),
[`mlr_measures_classif.logloss`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.logloss.md),
[`mlr_measures_classif.mauc_au1p`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.mauc_au1p.md),
[`mlr_measures_classif.mauc_au1u`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.mauc_au1u.md),
[`mlr_measures_classif.mauc_aunp`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.mauc_aunp.md),
[`mlr_measures_classif.mauc_aunu`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.mauc_aunu.md),
[`mlr_measures_classif.mauc_mu`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.mauc_mu.md),
[`mlr_measures_classif.mbrier`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.mbrier.md),
[`mlr_measures_classif.npv`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.npv.md),
[`mlr_measures_classif.ppv`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.ppv.md),
[`mlr_measures_classif.prauc`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.prauc.md),
[`mlr_measures_classif.precision`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.precision.md),
[`mlr_measures_classif.recall`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.recall.md),
[`mlr_measures_classif.sensitivity`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.sensitivity.md),
[`mlr_measures_classif.specificity`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.specificity.md),
[`mlr_measures_classif.tn`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.tn.md),
[`mlr_measures_classif.tnr`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.tnr.md),
[`mlr_measures_classif.tp`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.tp.md),
[`mlr_measures_classif.tpr`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.tpr.md)

Other multiclass classification measures:
[`mlr_measures_classif.acc`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.acc.md),
[`mlr_measures_classif.bacc`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.bacc.md),
[`mlr_measures_classif.ce`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.ce.md),
[`mlr_measures_classif.costs`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.costs.md),
[`mlr_measures_classif.logloss`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.logloss.md),
[`mlr_measures_classif.mauc_au1p`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.mauc_au1p.md),
[`mlr_measures_classif.mauc_au1u`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.mauc_au1u.md),
[`mlr_measures_classif.mauc_aunp`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.mauc_aunp.md),
[`mlr_measures_classif.mauc_aunu`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.mauc_aunu.md),
[`mlr_measures_classif.mauc_mu`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.mauc_mu.md),
[`mlr_measures_classif.mbrier`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.mbrier.md)
