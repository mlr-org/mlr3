# Dictionary of Performance Measures

A simple
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
storing objects of class
[Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md). Each
measure has an associated help page, see `mlr_measures_[id]`.

This dictionary can get populated with additional measures by add-on
packages. E.g.,
[mlr3proba](https://CRAN.R-project.org/package=mlr3proba) adds survival
measures and
[mlr3cluster](https://CRAN.R-project.org/package=mlr3cluster) adds
cluster analysis measures.

For a more convenient way to retrieve and construct measures, see
[`msr()`](https://mlr3.mlr-org.com/dev/reference/mlr_sugar.md)/[`msrs()`](https://mlr3.mlr-org.com/dev/reference/mlr_sugar.md).

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html).

## Methods

See
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html).

## S3 methods

- `as.data.table(dict, ..., objects = FALSE)`  
  [mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
  -\>
  [`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)  
  Returns a
  [`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
  with fields "key", "label", "task_type", "packages", "predict_type",
  and "task_properties" as columns. If `objects` is set to `TRUE`, the
  constructed objects are returned in the list column named `object`.

## See also

Sugar functions:
[`msr()`](https://mlr3.mlr-org.com/dev/reference/mlr_sugar.md),
[`msrs()`](https://mlr3.mlr-org.com/dev/reference/mlr_sugar.md)

Implementation of most measures:
[mlr3measures](https://CRAN.R-project.org/package=mlr3measures)

Other Dictionary:
[`mlr_learners`](https://mlr3.mlr-org.com/dev/reference/mlr_learners.md),
[`mlr_resamplings`](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings.md),
[`mlr_task_generators`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators.md),
[`mlr_tasks`](https://mlr3.mlr-org.com/dev/reference/mlr_tasks.md)

Other Measure:
[`Measure`](https://mlr3.mlr-org.com/dev/reference/Measure.md),
[`MeasureClassif`](https://mlr3.mlr-org.com/dev/reference/MeasureClassif.md),
[`MeasureRegr`](https://mlr3.mlr-org.com/dev/reference/MeasureRegr.md),
[`MeasureSimilarity`](https://mlr3.mlr-org.com/dev/reference/MeasureSimilarity.md),
[`mlr_measures_aic`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_aic.md),
[`mlr_measures_bic`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_bic.md),
[`mlr_measures_classif.costs`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.costs.md),
[`mlr_measures_debug_classif`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_debug_classif.md),
[`mlr_measures_elapsed_time`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_elapsed_time.md),
[`mlr_measures_internal_valid_score`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_internal_valid_score.md),
[`mlr_measures_oob_error`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_oob_error.md),
[`mlr_measures_regr.pinball`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_regr.pinball.md),
[`mlr_measures_regr.rqr`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_regr.rqr.md),
[`mlr_measures_regr.rsq`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_regr.rsq.md),
[`mlr_measures_selected_features`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_selected_features.md)

## Examples

``` r
as.data.table(mlr_measures)
#> Key: <key>
#>                      key                                               label
#>                   <char>                                              <char>
#>  1:                  aic                        Akaike Information Criterion
#>  2:                  bic                      Bayesian Information Criterion
#>  3:          classif.acc                             Classification Accuracy
#>  4:          classif.auc                            Area Under the ROC Curve
#>  5:         classif.bacc                                   Balanced Accuracy
#>  6:       classif.bbrier                                  Binary Brier Score
#>  7:           classif.ce                                Classification Error
#>  8:        classif.costs                       Cost-sensitive Classification
#>  9:          classif.dor                               Diagnostic Odds Ratio
#> 10:        classif.fbeta                                        F-beta score
#> 11:          classif.fdr                                False Discovery Rate
#> 12:           classif.fn                                     False Negatives
#> 13:          classif.fnr                                 False Negative Rate
#> 14:         classif.fomr                                 False Omission Rate
#> 15:           classif.fp                                     False Positives
#> 16:          classif.fpr                                 False Positive Rate
#> 17:      classif.logloss                                            Log Loss
#> 18:    classif.mauc_au1p             Weighted average 1 vs. 1 multiclass AUC
#> 19:    classif.mauc_au1u                      Average 1 vs. 1 multiclass AUC
#> 20:    classif.mauc_aunp          Weighted average 1 vs. rest multiclass AUC
#> 21:    classif.mauc_aunu                   Average 1 vs. rest multiclass AUC
#> 22:      classif.mauc_mu                                   Multiclass mu AUC
#> 23:       classif.mbrier                              Multiclass Brier Score
#> 24:          classif.mcc                    Matthews Correlation Coefficient
#> 25:          classif.npv                           Negative Predictive Value
#> 26:          classif.ppv                           Positive Predictive Value
#> 27:        classif.prauc                              Precision-Recall Curve
#> 28:    classif.precision                                           Precision
#> 29:       classif.recall                                              Recall
#> 30:  classif.sensitivity                                         Sensitivity
#> 31:  classif.specificity                                         Specificity
#> 32:           classif.tn                                      True Negatives
#> 33:          classif.tnr                                  True Negative Rate
#> 34:           classif.tp                                      True Positives
#> 35:          classif.tpr                                  True Positive Rate
#> 36:        debug_classif                        Debug Classification Measure
#> 37: internal_valid_score                           Internal Validation Score
#> 38:            oob_error                                    Out-of-bag Error
#> 39:            regr.bias                                                Bias
#> 40:            regr.ktau                                       Kendall's tau
#> 41:             regr.mae                                 Mean Absolute Error
#> 42:            regr.mape                         Mean Absolute Percent Error
#> 43:           regr.maxae                                  Max Absolute Error
#> 44:           regr.medae                               Median Absolute Error
#> 45:           regr.medse                                Median Squared Error
#> 46:             regr.mse                                  Mean Squared Error
#> 47:            regr.msle                              Mean Squared Log Error
#> 48:           regr.pbias                                        Percent Bias
#> 49:         regr.pinball                                                <NA>
#> 50:            regr.rmse                             Root Mean Squared Error
#> 51:           regr.rmsle                         Root Mean Squared Log Error
#> 52:             regr.rqr                                                <NA>
#> 53:             regr.rsq                                                <NA>
#> 54:             regr.sae                              Sum of Absolute Errors
#> 55:           regr.smape               Symmetric Mean Absolute Percent Error
#> 56:            regr.srho                                      Spearman's rho
#> 57:             regr.sse                               Sum of Squared Errors
#> 58:    selected_features Absolute or Relative Frequency of Selected Features
#> 59:          sim.jaccard                            Jaccard Similarity Index
#> 60:              sim.phi                          Phi Coefficient Similarity
#> 61:            time_both                                        Elapsed Time
#> 62:         time_predict                                        Elapsed Time
#> 63:           time_train                                        Elapsed Time
#>                      key                                               label
#>                   <char>                                              <char>
#>     task_type          packages predict_type
#>        <char>            <list>       <char>
#>  1:      <NA>              mlr3         <NA>
#>  2:      <NA>              mlr3         <NA>
#>  3:   classif mlr3,mlr3measures     response
#>  4:   classif mlr3,mlr3measures         prob
#>  5:   classif mlr3,mlr3measures     response
#>  6:   classif mlr3,mlr3measures         prob
#>  7:   classif mlr3,mlr3measures     response
#>  8:   classif              mlr3     response
#>  9:   classif mlr3,mlr3measures     response
#> 10:   classif mlr3,mlr3measures     response
#> 11:   classif mlr3,mlr3measures     response
#> 12:   classif mlr3,mlr3measures     response
#> 13:   classif mlr3,mlr3measures     response
#> 14:   classif mlr3,mlr3measures     response
#> 15:   classif mlr3,mlr3measures     response
#> 16:   classif mlr3,mlr3measures     response
#> 17:   classif mlr3,mlr3measures         prob
#> 18:   classif mlr3,mlr3measures         prob
#> 19:   classif mlr3,mlr3measures         prob
#> 20:   classif mlr3,mlr3measures         prob
#> 21:   classif mlr3,mlr3measures         prob
#> 22:   classif mlr3,mlr3measures         prob
#> 23:   classif mlr3,mlr3measures         prob
#> 24:   classif mlr3,mlr3measures     response
#> 25:   classif mlr3,mlr3measures     response
#> 26:   classif mlr3,mlr3measures     response
#> 27:   classif mlr3,mlr3measures         prob
#> 28:   classif mlr3,mlr3measures     response
#> 29:   classif mlr3,mlr3measures     response
#> 30:   classif mlr3,mlr3measures     response
#> 31:   classif mlr3,mlr3measures     response
#> 32:   classif mlr3,mlr3measures     response
#> 33:   classif mlr3,mlr3measures     response
#> 34:   classif mlr3,mlr3measures     response
#> 35:   classif mlr3,mlr3measures     response
#> 36:      <NA>              mlr3     response
#> 37:      <NA>              mlr3         <NA>
#> 38:      <NA>              mlr3         <NA>
#> 39:      regr mlr3,mlr3measures     response
#> 40:      regr mlr3,mlr3measures     response
#> 41:      regr mlr3,mlr3measures     response
#> 42:      regr mlr3,mlr3measures     response
#> 43:      regr mlr3,mlr3measures     response
#> 44:      regr mlr3,mlr3measures     response
#> 45:      regr mlr3,mlr3measures     response
#> 46:      regr mlr3,mlr3measures     response
#> 47:      regr mlr3,mlr3measures     response
#> 48:      regr mlr3,mlr3measures     response
#> 49:      regr              mlr3    quantiles
#> 50:      regr mlr3,mlr3measures     response
#> 51:      regr mlr3,mlr3measures     response
#> 52:      regr              mlr3    quantiles
#> 53:      regr              mlr3     response
#> 54:      regr mlr3,mlr3measures     response
#> 55:      regr mlr3,mlr3measures     response
#> 56:      regr mlr3,mlr3measures     response
#> 57:      regr mlr3,mlr3measures     response
#> 58:      <NA>              mlr3         <NA>
#> 59:      <NA> mlr3,mlr3measures         <NA>
#> 60:      <NA> mlr3,mlr3measures         <NA>
#> 61:      <NA>              mlr3         <NA>
#> 62:      <NA>              mlr3         <NA>
#> 63:      <NA>              mlr3         <NA>
#>     task_type          packages predict_type
#>        <char>            <list>       <char>
#>                                                               properties
#>                                                                   <list>
#>  1:      na_score,requires_learner,requires_model,requires_no_prediction
#>  2:      na_score,requires_learner,requires_model,requires_no_prediction
#>  3:                                                     weights,obs_loss
#>  4:                                                                     
#>  5:                                                              weights
#>  6:                                                     weights,obs_loss
#>  7:                                                     weights,obs_loss
#>  8:                                                              weights
#>  9:                                                                     
#> 10:                                                                     
#> 11:                                                                     
#> 12:                                                                     
#> 13:                                                                     
#> 14:                                                                     
#> 15:                                                                     
#> 16:                                                                     
#> 17:                                                     weights,obs_loss
#> 18:                                                                     
#> 19:                                                                     
#> 20:                                                                     
#> 21:                                                                     
#> 22:                                                                     
#> 23:                                                                     
#> 24:                                                                     
#> 25:                                                                     
#> 26:                                                                     
#> 27:                                                                     
#> 28:                                                                     
#> 29:                                                                     
#> 30:                                                                     
#> 31:                                                                     
#> 32:                                                                     
#> 33:                                                                     
#> 34:                                                                     
#> 35:                                                                     
#> 36:                                                             na_score
#> 37:                     na_score,requires_learner,requires_no_prediction
#> 38:                     na_score,requires_learner,requires_no_prediction
#> 39:                                                              weights
#> 40:                                                                     
#> 41:                                                     weights,obs_loss
#> 42:                                                     weights,obs_loss
#> 43:                                                             obs_loss
#> 44:                                                             obs_loss
#> 45:                                                             obs_loss
#> 46:                                                     weights,obs_loss
#> 47:                                                     weights,obs_loss
#> 48:                                                              weights
#> 49:                                                                     
#> 50:                                                     weights,obs_loss
#> 51:                                                              weights
#> 52:                                                               [NULL]
#> 53:                                                              weights
#> 54:                                                     weights,obs_loss
#> 55:                                                                     
#> 56:                                                                     
#> 57:                                                     weights,obs_loss
#> 58: requires_task,requires_learner,requires_model,requires_no_prediction
#> 59:                                requires_model,requires_no_prediction
#> 60:                                requires_model,requires_no_prediction
#> 61:                              requires_learner,requires_no_prediction
#> 62:                              requires_learner,requires_no_prediction
#> 63:                              requires_learner,requires_no_prediction
#>                                                               properties
#>                                                                   <list>
#>     task_properties
#>              <list>
#>  1:                
#>  2:                
#>  3:                
#>  4:        twoclass
#>  5:                
#>  6:        twoclass
#>  7:                
#>  8:                
#>  9:        twoclass
#> 10:        twoclass
#> 11:        twoclass
#> 12:        twoclass
#> 13:        twoclass
#> 14:        twoclass
#> 15:        twoclass
#> 16:        twoclass
#> 17:                
#> 18:                
#> 19:                
#> 20:                
#> 21:                
#> 22:                
#> 23:                
#> 24:                
#> 25:        twoclass
#> 26:        twoclass
#> 27:        twoclass
#> 28:        twoclass
#> 29:        twoclass
#> 30:        twoclass
#> 31:        twoclass
#> 32:        twoclass
#> 33:        twoclass
#> 34:        twoclass
#> 35:        twoclass
#> 36:                
#> 37:                
#> 38:                
#> 39:                
#> 40:                
#> 41:                
#> 42:                
#> 43:                
#> 44:                
#> 45:                
#> 46:                
#> 47:                
#> 48:                
#> 49:                
#> 50:                
#> 51:                
#> 52:                
#> 53:                
#> 54:                
#> 55:                
#> 56:                
#> 57:                
#> 58:                
#> 59:                
#> 60:                
#> 61:                
#> 62:                
#> 63:                
#>     task_properties
#>              <list>
mlr_measures$get("classif.ce")
#> 
#> ── <MeasureClassifSimple> (classif.ce): Classification Error ───────────────────
#> • Packages: mlr3 and mlr3measures
#> • Range: [0, 1]
#> • Minimize: TRUE
#> • Average: macro
#> • Parameters: list()
#> • Properties: weights and obs_loss
#> • Predict type: response
#> • Predict sets: test
#> • Aggregator: mean()
msr("regr.mse")
#> 
#> ── <MeasureRegrSimple> (regr.mse): Mean Squared Error ──────────────────────────
#> • Packages: mlr3 and mlr3measures
#> • Range: [0, Inf]
#> • Minimize: TRUE
#> • Average: macro
#> • Parameters: list()
#> • Properties: weights and obs_loss
#> • Predict type: response
#> • Predict sets: test
#> • Aggregator: mean()
```
