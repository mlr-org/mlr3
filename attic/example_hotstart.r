devtools::load_all(".")
devtools::load_all("../mlr3learners")
devtools::load_all("../mlr3tuning")
devtools::load_all("../mlr3hyperband")

library(future)
plan(multisession) # 8 worker

set.seed(7832)

learner = lrn("classif.xgboost",
  nrounds = to_tune(p_int(lower = 128, upper = 4096, tags = "budget")),
  colsample_bytree = to_tune(),
  eval_metric = "logloss"
)

instance_1 = tune(
  method = "hyperband",
  task = tsk("spam"),
  learner = learner,
  resampling = rsmp("cv", folds = 3),
  measure = msr("classif.ce"),
  eta = 2,
  allow_hotstart = TRUE)
