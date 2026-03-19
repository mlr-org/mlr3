# Callback Holdout Task

This
[CallbackResample](https://mlr3.mlr-org.com/dev/reference/CallbackResample.md)
predicts on an additional holdout task after training.

## Arguments

- task:

  ([Task](https://mlr3.mlr-org.com/dev/reference/Task.md))  
  The holdout task.

## Examples

``` r
task = tsk("pima")
task_holdout = task$clone()
learner = lrn("classif.rpart")
resampling = rsmp("cv", folds = 3)
splits = partition(task, 0.7)

task$filter(splits$train)
task_holdout$filter(splits$test)

callback = clbk("mlr3.holdout_task", task = task_holdout)

rr = resample(task, learner, resampling = resampling, callbacks = callback)

rr$data_extra
#> Key: <uhash, iteration>
#>                                   uhash iteration data_extra
#>                                  <char>     <int>     <list>
#> 1: 1fb11190-2cd2-4e36-9e39-94bcd571208e         1  <list[1]>
#> 2: 1fb11190-2cd2-4e36-9e39-94bcd571208e         2  <list[1]>
#> 3: 1fb11190-2cd2-4e36-9e39-94bcd571208e         3  <list[1]>
```
