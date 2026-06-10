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
#> 1: 0c3e8913-5503-4188-9dda-012c039128e2         1  <list[1]>
#> 2: 0c3e8913-5503-4188-9dda-012c039128e2         2  <list[1]>
#> 3: 0c3e8913-5503-4188-9dda-012c039128e2         3  <list[1]>
```
