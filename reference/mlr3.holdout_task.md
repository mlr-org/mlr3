# Callback Holdout Task

This
[CallbackResample](https://mlr3.mlr-org.com/reference/CallbackResample.md)
predicts on an additional holdout task after training.

## Arguments

- task:

  ([Task](https://mlr3.mlr-org.com/reference/Task.md))  
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
#> 1: 67aa18f2-de87-49b6-a63a-f44d3980d6ae         1  <list[1]>
#> 2: 67aa18f2-de87-49b6-a63a-f44d3980d6ae         2  <list[1]>
#> 3: 67aa18f2-de87-49b6-a63a-f44d3980d6ae         3  <list[1]>
```
