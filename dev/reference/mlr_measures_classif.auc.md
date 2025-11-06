# Area Under the ROC Curve

Measure to compare true observed labels with predicted probabilities in
binary classification tasks.

## Details

Computes the area under the Receiver Operator Characteristic (ROC)
curve. The AUC can be interpreted as the probability that a randomly
chosen positive observation has a higher predicted probability than a
randomly chosen negative observation.

This measure is undefined if the true values are either all positive or
all negative.

## Note

The score function calls
[`mlr3measures::auc()`](https://mlr3measures.mlr-org.com/reference/auc.html)
from package
[mlr3measures](https://CRAN.R-project.org/package=mlr3measures).

If the measure is undefined for the input, `NaN` is returned. This can
be customized by setting the field `na_value`.

## Dictionary

This [Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_measures](https://mlr3.mlr-org.com/dev/reference/mlr_measures.md)
or with the associated sugar function
[`msr()`](https://mlr3.mlr-org.com/dev/reference/mlr_sugar.md):

    mlr_measures$get("classif.auc")
    msr("classif.auc")

## Parameters

Empty ParamSet

## Meta Information

- Type: `"binary"`

- Range: \\\[0, 1\]\\

- Minimize: `FALSE`

- Required prediction: `prob`

## See also

[Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html) of
[Measures](https://mlr3.mlr-org.com/dev/reference/Measure.md):
[mlr_measures](https://mlr3.mlr-org.com/dev/reference/mlr_measures.md)

`as.data.table(mlr_measures)` for a complete table of all (also
dynamically created)
[Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md)
implementations.

Other classification measures:
[`mlr_measures_classif.acc`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.acc.md),
[`mlr_measures_classif.bacc`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.bacc.md),
[`mlr_measures_classif.bbrier`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.bbrier.md),
[`mlr_measures_classif.ce`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.ce.md),
[`mlr_measures_classif.costs`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.costs.md),
[`mlr_measures_classif.dor`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.dor.md),
[`mlr_measures_classif.fbeta`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.fbeta.md),
[`mlr_measures_classif.fdr`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.fdr.md),
[`mlr_measures_classif.fn`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.fn.md),
[`mlr_measures_classif.fnr`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.fnr.md),
[`mlr_measures_classif.fomr`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.fomr.md),
[`mlr_measures_classif.fp`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.fp.md),
[`mlr_measures_classif.fpr`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.fpr.md),
[`mlr_measures_classif.logloss`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.logloss.md),
[`mlr_measures_classif.mauc_au1p`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.mauc_au1p.md),
[`mlr_measures_classif.mauc_au1u`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.mauc_au1u.md),
[`mlr_measures_classif.mauc_aunp`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.mauc_aunp.md),
[`mlr_measures_classif.mauc_aunu`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.mauc_aunu.md),
[`mlr_measures_classif.mauc_mu`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.mauc_mu.md),
[`mlr_measures_classif.mbrier`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.mbrier.md),
[`mlr_measures_classif.mcc`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.mcc.md),
[`mlr_measures_classif.npv`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.npv.md),
[`mlr_measures_classif.ppv`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.ppv.md),
[`mlr_measures_classif.prauc`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.prauc.md),
[`mlr_measures_classif.precision`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.precision.md),
[`mlr_measures_classif.recall`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.recall.md),
[`mlr_measures_classif.sensitivity`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.sensitivity.md),
[`mlr_measures_classif.specificity`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.specificity.md),
[`mlr_measures_classif.tn`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.tn.md),
[`mlr_measures_classif.tnr`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.tnr.md),
[`mlr_measures_classif.tp`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.tp.md),
[`mlr_measures_classif.tpr`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.tpr.md)

Other binary classification measures:
[`mlr_measures_classif.bbrier`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.bbrier.md),
[`mlr_measures_classif.dor`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.dor.md),
[`mlr_measures_classif.fbeta`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.fbeta.md),
[`mlr_measures_classif.fdr`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.fdr.md),
[`mlr_measures_classif.fn`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.fn.md),
[`mlr_measures_classif.fnr`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.fnr.md),
[`mlr_measures_classif.fomr`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.fomr.md),
[`mlr_measures_classif.fp`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.fp.md),
[`mlr_measures_classif.fpr`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.fpr.md),
[`mlr_measures_classif.npv`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.npv.md),
[`mlr_measures_classif.ppv`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.ppv.md),
[`mlr_measures_classif.prauc`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.prauc.md),
[`mlr_measures_classif.precision`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.precision.md),
[`mlr_measures_classif.recall`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.recall.md),
[`mlr_measures_classif.sensitivity`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.sensitivity.md),
[`mlr_measures_classif.specificity`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.specificity.md),
[`mlr_measures_classif.tn`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.tn.md),
[`mlr_measures_classif.tnr`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.tnr.md),
[`mlr_measures_classif.tp`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.tp.md),
[`mlr_measures_classif.tpr`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.tpr.md)
