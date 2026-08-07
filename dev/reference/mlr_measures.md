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
  [`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)  
  Returns a
  [`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
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
[`mlr_measures_best_valid_score`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_best_valid_score.md),
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
#>  2:     best_valid_score                               Best Validation Score
#>  3:                  bic                      Bayesian Information Criterion
#>  4:          classif.acc                             Classification Accuracy
#>  5:          classif.auc                            Area Under the ROC Curve
#>  6:         classif.bacc                                   Balanced Accuracy
#>  7:       classif.bbrier                                  Binary Brier Score
#>  8:           classif.ce                                Classification Error
#>  9:        classif.costs                       Cost-sensitive Classification
#> 10:          classif.dor                               Diagnostic Odds Ratio
#> 11:        classif.fbeta                                        F-beta score
#> 12:          classif.fdr                                False Discovery Rate
#> 13:           classif.fn                                     False Negatives
#> 14:          classif.fnr                                 False Negative Rate
#> 15:         classif.fomr                                 False Omission Rate
#> 16:           classif.fp                                     False Positives
#> 17:          classif.fpr                                 False Positive Rate
#> 18:      classif.logloss                                            Log Loss
#> 19:    classif.mauc_au1p             Weighted average 1 vs. 1 multiclass AUC
#> 20:    classif.mauc_au1u                      Average 1 vs. 1 multiclass AUC
#> 21:    classif.mauc_aunp          Weighted average 1 vs. rest multiclass AUC
#> 22:    classif.mauc_aunu                   Average 1 vs. rest multiclass AUC
#> 23:      classif.mauc_mu                                   Multiclass mu AUC
#> 24:       classif.mbrier                              Multiclass Brier Score
#> 25:          classif.mcc                    Matthews Correlation Coefficient
#> 26:          classif.npv                           Negative Predictive Value
#> 27:          classif.ppv                           Positive Predictive Value
#> 28:        classif.prauc                              Precision-Recall Curve
#> 29:    classif.precision                                           Precision
#> 30:       classif.recall                                              Recall
#> 31:  classif.sensitivity                                         Sensitivity
#> 32:  classif.specificity                                         Specificity
#> 33:           classif.tn                                      True Negatives
#> 34:          classif.tnr                                  True Negative Rate
#> 35:           classif.tp                                      True Positives
#> 36:          classif.tpr                                  True Positive Rate
#> 37:        debug_classif                        Debug Classification Measure
#> 38: internal_valid_score                           Internal Validation Score
#> 39:            oob_error                                    Out-of-bag Error
#> 40:            regr.bias                                                Bias
#> 41:            regr.ktau                                       Kendall's tau
#> 42:             regr.mae                                 Mean Absolute Error
#> 43:            regr.mape                         Mean Absolute Percent Error
#> 44:           regr.maxae                                  Max Absolute Error
#> 45:           regr.medae                               Median Absolute Error
#> 46:           regr.medse                                Median Squared Error
#> 47:             regr.mse                                  Mean Squared Error
#> 48:            regr.msle                              Mean Squared Log Error
#> 49:           regr.pbias                                        Percent Bias
#> 50:         regr.pinball                                Average Pinball Loss
#> 51:            regr.rmse                             Root Mean Squared Error
#> 52:           regr.rmsle                         Root Mean Squared Log Error
#> 53:             regr.rqr                   R-Squared for Quantile Regression
#> 54:             regr.rsq                                                <NA>
#> 55:             regr.sae                              Sum of Absolute Errors
#> 56:           regr.smape               Symmetric Mean Absolute Percent Error
#> 57:            regr.srho                                      Spearman's rho
#> 58:             regr.sse                               Sum of Squared Errors
#> 59:    selected_features Absolute or Relative Frequency of Selected Features
#> 60:          sim.jaccard                            Jaccard Similarity Index
#> 61:              sim.phi                          Phi Coefficient Similarity
#> 62:            time_both                                        Elapsed Time
#> 63:         time_predict                                        Elapsed Time
#> 64:           time_train                                        Elapsed Time
#>                      key                                               label
#>                   <char>                                              <char>
#>     task_type          packages predict_type
#>        <char>            <list>       <char>
#>  1:      <NA>              mlr3         <NA>
#>  2:      <NA>              mlr3         <NA>
#>  3:      <NA>              mlr3         <NA>
#>  4:   classif mlr3,mlr3measures     response
#>  5:   classif mlr3,mlr3measures         prob
#>  6:   classif mlr3,mlr3measures     response
#>  7:   classif mlr3,mlr3measures         prob
#>  8:   classif mlr3,mlr3measures     response
#>  9:   classif              mlr3     response
#> 10:   classif mlr3,mlr3measures     response
#> 11:   classif mlr3,mlr3measures     response
#> 12:   classif mlr3,mlr3measures     response
#> 13:   classif mlr3,mlr3measures     response
#> 14:   classif mlr3,mlr3measures     response
#> 15:   classif mlr3,mlr3measures     response
#> 16:   classif mlr3,mlr3measures     response
#> 17:   classif mlr3,mlr3measures     response
#> 18:   classif mlr3,mlr3measures         prob
#> 19:   classif mlr3,mlr3measures         prob
#> 20:   classif mlr3,mlr3measures         prob
#> 21:   classif mlr3,mlr3measures         prob
#> 22:   classif mlr3,mlr3measures         prob
#> 23:   classif mlr3,mlr3measures         prob
#> 24:   classif mlr3,mlr3measures         prob
#> 25:   classif mlr3,mlr3measures     response
#> 26:   classif mlr3,mlr3measures     response
#> 27:   classif mlr3,mlr3measures     response
#> 28:   classif mlr3,mlr3measures         prob
#> 29:   classif mlr3,mlr3measures     response
#> 30:   classif mlr3,mlr3measures     response
#> 31:   classif mlr3,mlr3measures     response
#> 32:   classif mlr3,mlr3measures     response
#> 33:   classif mlr3,mlr3measures     response
#> 34:   classif mlr3,mlr3measures     response
#> 35:   classif mlr3,mlr3measures     response
#> 36:   classif mlr3,mlr3measures     response
#> 37:      <NA>              mlr3     response
#> 38:      <NA>              mlr3         <NA>
#> 39:      <NA>              mlr3         <NA>
#> 40:      regr mlr3,mlr3measures     response
#> 41:      regr mlr3,mlr3measures     response
#> 42:      regr mlr3,mlr3measures     response
#> 43:      regr mlr3,mlr3measures     response
#> 44:      regr mlr3,mlr3measures     response
#> 45:      regr mlr3,mlr3measures     response
#> 46:      regr mlr3,mlr3measures     response
#> 47:      regr mlr3,mlr3measures     response
#> 48:      regr mlr3,mlr3measures     response
#> 49:      regr mlr3,mlr3measures     response
#> 50:      regr              mlr3    quantiles
#> 51:      regr mlr3,mlr3measures     response
#> 52:      regr mlr3,mlr3measures     response
#> 53:      regr              mlr3    quantiles
#> 54:      regr              mlr3     response
#> 55:      regr mlr3,mlr3measures     response
#> 56:      regr mlr3,mlr3measures     response
#> 57:      regr mlr3,mlr3measures     response
#> 58:      regr mlr3,mlr3measures     response
#> 59:      <NA>              mlr3         <NA>
#> 60:      <NA> mlr3,mlr3measures         <NA>
#> 61:      <NA> mlr3,mlr3measures         <NA>
#> 62:      <NA>              mlr3         <NA>
#> 63:      <NA>              mlr3         <NA>
#> 64:      <NA>              mlr3         <NA>
#>     task_type          packages predict_type
#>        <char>            <list>       <char>
#>                                                               properties
#>                                                                   <list>
#>  1:      na_score,requires_learner,requires_model,requires_no_prediction
#>  2:                     na_score,requires_learner,requires_no_prediction
#>  3:      na_score,requires_learner,requires_model,requires_no_prediction
#>  4:                                                     weights,obs_loss
#>  5:                                                              weights
#>  6:                                                              weights
#>  7:                                                     weights,obs_loss
#>  8:                                                     weights,obs_loss
#>  9:                                                              weights
#> 10:                                                              weights
#> 11:                                                              weights
#> 12:                                                              weights
#> 13:                                                              weights
#> 14:                                                              weights
#> 15:                                                              weights
#> 16:                                                              weights
#> 17:                                                              weights
#> 18:                                                     weights,obs_loss
#> 19:                                                                     
#> 20:                                                                     
#> 21:                                                                     
#> 22:                                                                     
#> 23:                                                                     
#> 24:                                                              weights
#> 25:                                                              weights
#> 26:                                                              weights
#> 27:                                                              weights
#> 28:                                                                     
#> 29:                                                              weights
#> 30:                                                              weights
#> 31:                                                              weights
#> 32:                                                              weights
#> 33:                                                              weights
#> 34:                                                              weights
#> 35:                                                              weights
#> 36:                                                              weights
#> 37:                                                             na_score
#> 38:                     na_score,requires_learner,requires_no_prediction
#> 39:                     na_score,requires_learner,requires_no_prediction
#> 40:                                                              weights
#> 41:                                                                     
#> 42:                                                     weights,obs_loss
#> 43:                                                     weights,obs_loss
#> 44:                                                             obs_loss
#> 45:                                                             obs_loss
#> 46:                                                             obs_loss
#> 47:                                                     weights,obs_loss
#> 48:                                                     weights,obs_loss
#> 49:                                                              weights
#> 50:                                                              weights
#> 51:                                                     weights,obs_loss
#> 52:                                                              weights
#> 53:                                                               [NULL]
#> 54:                                                              weights
#> 55:                                                     weights,obs_loss
#> 56:                                                              weights
#> 57:                                                                     
#> 58:                                                     weights,obs_loss
#> 59: requires_task,requires_learner,requires_model,requires_no_prediction
#> 60:                                requires_model,requires_no_prediction
#> 61:                                requires_model,requires_no_prediction
#> 62:                              requires_learner,requires_no_prediction
#> 63:                              requires_learner,requires_no_prediction
#> 64:                              requires_learner,requires_no_prediction
#>                                                               properties
#>                                                                   <list>
#>     task_properties
#>              <list>
#>  1:                
#>  2:                
#>  3:                
#>  4:                
#>  5:        twoclass
#>  6:                
#>  7:        twoclass
#>  8:                
#>  9:                
#> 10:        twoclass
#> 11:        twoclass
#> 12:        twoclass
#> 13:        twoclass
#> 14:        twoclass
#> 15:        twoclass
#> 16:        twoclass
#> 17:        twoclass
#> 18:                
#> 19:                
#> 20:                
#> 21:                
#> 22:                
#> 23:                
#> 24:                
#> 25:                
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
#> 36:        twoclass
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
#> 64:                
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
