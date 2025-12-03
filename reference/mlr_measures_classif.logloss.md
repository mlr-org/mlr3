# Log Loss

Measure to compare true observed labels with predicted probabilities in
multiclass classification tasks.

## Details

The Log Loss (a.k.a Benoulli Loss, Logistic Loss, Cross-Entropy Loss) is
defined as \$\$ -\frac{1}{n} \sum\_{i=1}^n w_i \log \left( p_i \right )
\$\$ where \\p_i\\ is the probability for the true class of observation
\\i\\ and \\w_i\\ are normalized weights for each observation \\x_i\\.

## Note

The score function calls
[`mlr3measures::logloss()`](https://mlr3measures.mlr-org.com/reference/logloss.html)
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

    mlr_measures$get("classif.logloss")
    msr("classif.logloss")

## Parameters

Empty ParamSet

## Meta Information

- Type: `"classif"`

- Range: \\\[0, \infty)\\

- Minimize: `TRUE`

- Required prediction: `prob`

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
[`mlr_measures_classif.mauc_au1p`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.mauc_au1p.md),
[`mlr_measures_classif.mauc_au1u`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.mauc_au1u.md),
[`mlr_measures_classif.mauc_aunp`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.mauc_aunp.md),
[`mlr_measures_classif.mauc_aunu`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.mauc_aunu.md),
[`mlr_measures_classif.mauc_mu`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.mauc_mu.md),
[`mlr_measures_classif.mbrier`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.mbrier.md),
[`mlr_measures_classif.mcc`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.mcc.md),
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
[`mlr_measures_classif.mauc_au1p`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.mauc_au1p.md),
[`mlr_measures_classif.mauc_au1u`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.mauc_au1u.md),
[`mlr_measures_classif.mauc_aunp`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.mauc_aunp.md),
[`mlr_measures_classif.mauc_aunu`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.mauc_aunu.md),
[`mlr_measures_classif.mauc_mu`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.mauc_mu.md),
[`mlr_measures_classif.mbrier`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.mbrier.md),
[`mlr_measures_classif.mcc`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.mcc.md)
