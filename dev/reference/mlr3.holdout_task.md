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
#> 1: 97ac9e6b-417b-47e3-a196-acef3d6ee58f         1  <list[1]>
#> 2: 97ac9e6b-417b-47e3-a196-acef3d6ee58f         2  <list[1]>
#> 3: 97ac9e6b-417b-47e3-a196-acef3d6ee58f         3  <list[1]>
```
