library(bench)
# library(mlr3)
devtools::load_all()


tasks = mlr_tasks$mget(c("iris", "sonar"))
learners = mlr_learners$mget(c("classif.featureless"))
resamplings = mlr_resamplings$mget("subsampling")
measures = mlr_measures$mget(c("acc", "time_train"))

profvis::profvis(
  benchmark(tasks, learners, resamplings, measures, mlr_control(verbose = FALSE, use_future = FALSE))
)
