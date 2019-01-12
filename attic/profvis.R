library(bench)
# library(mlr3)
devtools::load_all()
library(logger)


tasks = mlr_tasks$mget(c("iris", "sonar"))
learners = mlr_learners$mget(c("classif.featureless"))
resamplings = mlr_resamplings$mget("subsampling")
measures = mlr_measures$mget(c("acc", "time_train"))

log_threshold(ERROR, namespace = "mlr3")
profvis::profvis(
  benchmark(expand_grid(tasks, learners, resamplings))
)
