# Phi Coefficient Similarity

Measure to compare two or more sets w.r.t. their similarity.

## Details

The Phi Coefficient is defined as the Pearson correlation between the
binary representation of two sets \\A\\ and \\B\\. The binary
representation for \\A\\ is a logical vector of length \\p\\ with the
i-th element being 1 if the corresponding element is in \\A\\, and 0
otherwise.

If more than two sets are provided, the mean of all pairwise scores is
calculated.

This measure is undefined if one set contains none or all possible
elements.

## Note

This measure requires learners with property `"selected_features"`. The
extracted feature sets are passed to
[`mlr3measures::phi()`](https://mlr3measures.mlr-org.com/reference/phi.html)
from package
[mlr3measures](https://CRAN.R-project.org/package=mlr3measures).

If the measure is undefined for the input, `NaN` is returned. This can
be customized by setting the field `na_value`.

## Parameters

|     |         |         |                  |
|-----|---------|---------|------------------|
| Id  | Type    | Default | Range            |
| p   | integer | \-      | \\\[1, \infty)\\ |

## Dictionary

This [Measure](https://mlr3.mlr-org.com/reference/Measure.md) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_measures](https://mlr3.mlr-org.com/reference/mlr_measures.md) or
with the associated sugar function
[`msr()`](https://mlr3.mlr-org.com/reference/mlr_sugar.md):

    mlr_measures$get("sim.phi")
    msr("sim.phi")

## Meta Information

- Type: `"similarity"`

- Range: \\\[-1, 1\]\\

- Minimize: `FALSE`

## See also

[Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html) of
[Measures](https://mlr3.mlr-org.com/reference/Measure.md):
[mlr_measures](https://mlr3.mlr-org.com/reference/mlr_measures.md)

`as.data.table(mlr_measures)` for a complete table of all (also
dynamically created)
[Measure](https://mlr3.mlr-org.com/reference/Measure.md)
implementations.

Other similarity measures:
[`mlr_measures_sim.jaccard`](https://mlr3.mlr-org.com/reference/mlr_measures_sim.jaccard.md)
