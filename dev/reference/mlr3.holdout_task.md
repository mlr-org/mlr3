# Callback Holdout Task

This
[CallbackResample](https://mlr3.mlr-org.com/dev/reference/CallbackResample.md)
predicts on an additional holdout task after training. The prediction is
not included in the predict time of the resampling iteration.

## Parameters

- `task`:

  ([Task](https://mlr3.mlr-org.com/dev/reference/Task.md))  
  The holdout task.

## Examples

``` r
task = tsk("sonar")
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
#> 1: 401d3962-d216-44bf-be1a-bd6995896488         1  <list[1]>
#> 2: 401d3962-d216-44bf-be1a-bd6995896488         2  <list[1]>
#> 3: 401d3962-d216-44bf-be1a-bd6995896488         3  <list[1]>
```
