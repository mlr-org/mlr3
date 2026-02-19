task_classif = tsk("pima") # mlr3 1.4.0
saveRDS(task_classif, "tests/testthat/_object_snapshots/task_classif.rds")

task_regr = tsk("mtcars") # mlr3 1.4.0
saveRDS(task_regr, "tests/testthat/_object_snapshots/task_regr.rds")

learner_classif = lrn("classif.rpart") # mlr3 1.0.0
learner_classif$train(task_classif)
saveRDS(learner_classif, "tests/testthat/_object_snapshots/learner_classif.rds")

learner_regr = lrn("regr.rpart") # mlr3 1.0.0
learner_regr$train(task_regr)
saveRDS(learner_regr, "tests/testthat/_object_snapshots/learner_regr.rds")

resampling = rsmp("cv", folds = 3) # mlr3 1.0.0
resampling$instantiate(task_classif)
saveRDS(resampling, "tests/testthat/_object_snapshots/resampling.rds")

rr = resample(task_classif, learner_classif, resampling) # mlr3 1.4.0
saveRDS(rr, "tests/testthat/_object_snapshots/rr.rds")

design = benchmark_grid(
  tasks = list(task_classif),
  learners = list(learner_classif),
  resamplings = list(resampling)
)

bmr = benchmark(design) # mlr3 1.4.0
saveRDS(bmr, "tests/testthat/_object_snapshots/bmr.rds")

measure = msr("classif.ce") # mlr3 1.0.0
saveRDS(measure, "tests/testthat/_object_snapshots/measure.rds")

