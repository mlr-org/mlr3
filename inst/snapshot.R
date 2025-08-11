# we ran this once with mlr3 1.0.0
task_classif = tsk("pima")

saveRDS(task_classif, "inst/snapshots/task_classif.rds")

task_regr = tsk("mtcars")
saveRDS(task_regr, "inst/snapshots/task_regr.rds")

learner_classif = lrn("classif.rpart")
learner_classif$train(task_classif)

saveRDS(learner_classif, "inst/snapshots/learner_classif.rds")

learner_regr = lrn("regr.rpart")
learner_regr$train(task_regr)

saveRDS(learner_regr, "inst/snapshots/learner_regr.rds")

resampling = rsmp("cv", folds = 3)
resampling$instantiate(task_classif)
saveRDS(resampling, "inst/snapshots/resampling.rds")

rr = resample(task_classif, learner_classif, resampling)
saveRDS(rr, "inst/snapshots/rr.rds")

design = benchmark_grid(
  tasks = list(task_classif),
  learners = list(learner_classif),
  resamplings = list(resampling)
)

bmr = benchmark(design)
saveRDS(bmr, "inst/snapshots/bmr.rds")

measure = msr("classif.ce")
saveRDS(measure, "inst/snapshots/measure.rds")

