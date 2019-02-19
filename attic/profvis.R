library(bench)
# library(mlr3)
devtools::load_all()
library(logger)


tasks = mlr_tasks$mget(c("iris", "sonar"))
learners = mlr_learners$mget(c("classif.featureless"))
resamplings = mlr_resamplings$mget("subsampling")
measures = mlr_measures$mget(c("classif.acc", "time_train"))

log_threshold(ERROR, namespace = "mlr3")
profvis::profvis(
  benchmark(expand_grid(tasks, learners, resamplings))
)


data("flights", package = "nycflights13")
data = as.data.table(flights)
data$time_hour = NULL
b = as_data_backend(data[!is.na(arr_delay)])
task = TaskRegr$new("flights", b, target = "arr_delay")

profvis::profvis(
  resample(task, mlr_learners$get("regr.featureless"), mlr_resamplings$get("cv", param_vals = list(folds = 3)))
)
