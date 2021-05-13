library(mlr3)
library(mlr3learners)

set.seed(2)

task = tsk("pima")
learner = lrn("classif.xgboost", eval_metric = "logloss")

indices = list(train = list(1, 2, 3, 4, 5), test = list(6, 7, 8, 9, 10))

rc = rsmp("custom")
rc$instantiate(task, indices$train, indices$test)

rr = resample(task, learner, rc)
rr$aggregate(msr("classif.fbeta", average = "micro"))
