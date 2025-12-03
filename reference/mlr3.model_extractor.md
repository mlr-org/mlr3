# Model Extractor Callback

This
[CallbackResample](https://mlr3.mlr-org.com/reference/CallbackResample.md)
extracts information from the model after training with a user-defined
function. This way information can be extracted from the model without
saving the model (`store_models = FALSE`). The `fun` must be a function
that takes a learner as input and returns the extracted information as
named list (see example). The callback is very helpful to call
`$selected_features()`, `$importance()`, `$oob_error()` on the learner.

## Arguments

- fun:

  (`function(learner)`)  
  Function to extract information from the learner. The function must
  have the argument `learner`. The function must return a named list.

## Examples

``` r
task = tsk("pima")
learner = lrn("classif.rpart")
resampling = rsmp("cv", folds = 3)

# define function to extract selected features
selected_features = function(learner) list(selected_features = learner$selected_features())

# create callback
callback = clbk("mlr3.model_extractor", fun = selected_features)

rr = resample(task, learner, resampling = resampling, store_models = FALSE, callbacks = callback)

rr$data_extra
#> Key: <uhash, iteration>
#>                                   uhash iteration data_extra
#>                                  <char>     <int>     <list>
#> 1: 89a0436d-8eca-448a-83cc-3fc36e5cb2a5         1  <list[1]>
#> 2: 89a0436d-8eca-448a-83cc-3fc36e5cb2a5         2  <list[1]>
#> 3: 89a0436d-8eca-448a-83cc-3fc36e5cb2a5         3  <list[1]>
```
