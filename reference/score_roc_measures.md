# Calculate ROC Measures

Calculate a set of roc performance measures based on the confusion
matrix.

- `tpr` True positive rate (Sensitivity, Recall)

- `fpr` False positive rate (Fall-out)

- `fnr` False negative rate (Miss rate)

- `tnr` True negative rate (Specificity)

- `ppv` Positive predictive value (Precision)

- `fomr` False omission rate

- `lrp` Positive likelihood ratio (LR+)

- `fdr` False discovery rate

- `npv` Negative predictive value

- `acc` Accuracy

- `lrm` Negative likelihood ratio (LR-)

- `dor` Diagnostic odds ratio

## Usage

``` r
score_roc_measures(pred)
```

## Arguments

- pred:

  ([PredictionClassif](https://mlr3.mlr-org.com/reference/PredictionClassif.md))  
  The prediction object.

## Value

[`list()`](https://rdrr.io/r/base/list.html)  
A list containing two elements `confusion_matrix` which is the 2 times 2
confusion matrix of absolute frequencies and `measures`, a list of the
above mentioned measures.

## Examples

``` r
learner = lrn("classif.rpart", predict_type = "prob")
splits = partition(task = tsk("pima"), ratio = 0.7)
task = tsk("pima")
learner$train(task)
pred = learner$predict(task)
score_roc_measures(pred)
#> 
#> ── ROC Measures ──
#> 
#>      predicted
#> true  pos       neg                                 
#>   pos 207       73         tpr: 0.77      fnr: 0.23 
#>   neg 61        427        fpr: 0.15      tnr: 0.85 
#>       ppv: 0.74 fomr: 0.12 lr_plus: 5.29  acc: 0.83 
#>       fdr: 0.26 npv: 0.88  lr_minus: 0.27 dor: 19.85
#> 
#> ── Abbreviations: 
#> • tpr - True positive rate (Sensitivity, Recall)
#> • fpr - False positive rate (Fall-out)
#> • fnr - False negative rate (Miss rate)
#> • tnr - True negative rate (Specificity)
#> • ppv - Positive predictive value (Precision)
#> • fomr - False omission rate
#> • lrp - Positive likelihood ratio (LR+)
#> • fdr - False discovery rate
#> • npv - Negative predictive value
#> • acc - Accuracy
#> • lrm - Negative likelihood ratio (LR-)
#> • dor - Diagnostic odds ratio
```
