# Root Mean Squared Log Error

Measure to compare true observed response with predicted response in
regression tasks.

## Details

The Root Mean Squared Log Error is defined as \$\$ \sqrt{\frac{1}{n}
\sum\_{i=1}^n w_i \left( \ln (1 + t_i) - \ln (1 + r_i) \right)^2}, \$\$
where \\w_i\\ are normalized sample weights.

This measure is undefined if any element of \\t\\ or \\r\\ is less than
or equal to \\-1\\.

## Note

The score function calls
[`mlr3measures::rmsle()`](https://mlr3measures.mlr-org.com/reference/rmsle.html)
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

    mlr_measures$get("regr.rmsle")
    msr("regr.rmsle")

## Parameters

Empty ParamSet

## Meta Information

- Type: `"regr"`

- Range: \\\[0, \infty)\\

- Minimize: `TRUE`

- Required prediction: `response`

## See also

[Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html) of
[Measures](https://mlr3.mlr-org.com/dev/reference/Measure.md):
[mlr_measures](https://mlr3.mlr-org.com/dev/reference/mlr_measures.md)

`as.data.table(mlr_measures)` for a complete table of all (also
dynamically created)
[Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md)
implementations.

Other regression measures:
[`mlr_measures_regr.bias`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_regr.bias.md),
[`mlr_measures_regr.ktau`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_regr.ktau.md),
[`mlr_measures_regr.mae`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_regr.mae.md),
[`mlr_measures_regr.mape`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_regr.mape.md),
[`mlr_measures_regr.maxae`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_regr.maxae.md),
[`mlr_measures_regr.medae`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_regr.medae.md),
[`mlr_measures_regr.medse`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_regr.medse.md),
[`mlr_measures_regr.mse`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_regr.mse.md),
[`mlr_measures_regr.msle`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_regr.msle.md),
[`mlr_measures_regr.pbias`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_regr.pbias.md),
[`mlr_measures_regr.rmse`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_regr.rmse.md),
[`mlr_measures_regr.sae`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_regr.sae.md),
[`mlr_measures_regr.smape`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_regr.smape.md),
[`mlr_measures_regr.srho`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_regr.srho.md),
[`mlr_measures_regr.sse`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_regr.sse.md)
