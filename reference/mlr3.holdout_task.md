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
#> 1: 27a1b9a9-8e5d-4c0a-bbb3-762d41bb1500         1  <list[1]>
#> 2: 27a1b9a9-8e5d-4c0a-bbb3-762d41bb1500         2  <list[1]>
#> 3: 27a1b9a9-8e5d-4c0a-bbb3-762d41bb1500         3  <list[1]>
```
