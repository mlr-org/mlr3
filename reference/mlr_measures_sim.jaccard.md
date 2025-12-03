# Jaccard Similarity Index

Measure to compare two or more sets w.r.t. their similarity.

## Details

For two sets \\A\\ and \\B\\, the Jaccard Index is defined as \$\$ J(A,
B) = \frac{\|A \cap B\|}{\|A \cup B\|}. \$\$ If more than two sets are
provided, the mean of all pairwise scores is calculated.

This measure is undefined if two or more sets are empty.

## Note

This measure requires learners with property `"selected_features"`. The
extracted feature sets are passed to
[`mlr3measures::jaccard()`](https://mlr3measures.mlr-org.com/reference/jaccard.html)
from package
[mlr3measures](https://CRAN.R-project.org/package=mlr3measures).

If the measure is undefined for the input, `NaN` is returned. This can
be customized by setting the field `na_value`.

## Parameters

Empty ParamSet

## Dictionary

This [Measure](https://mlr3.mlr-org.com/reference/Measure.md) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_measures](https://mlr3.mlr-org.com/reference/mlr_measures.md) or
with the associated sugar function
[`msr()`](https://mlr3.mlr-org.com/reference/mlr_sugar.md):

    mlr_measures$get("sim.jaccard")
    msr("sim.jaccard")

## Meta Information

- Type: `"similarity"`

- Range: \\\[0, 1\]\\

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
[`mlr_measures_sim.phi`](https://mlr3.mlr-org.com/reference/mlr_measures_sim.phi.md)
