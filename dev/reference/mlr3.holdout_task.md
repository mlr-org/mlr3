# Callback Holdout Task

This
[CallbackResample](https://mlr3.mlr-org.com/dev/reference/CallbackResample.md)
predicts on an additional holdout task after training.

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
#> 1: a7bd02d2-5ea7-4a0b-a2f0-618b65fac5ae         1  <list[1]>
#> 2: a7bd02d2-5ea7-4a0b-a2f0-618b65fac5ae         2  <list[1]>
#> 3: a7bd02d2-5ea7-4a0b-a2f0-618b65fac5ae         3  <list[1]>
```
