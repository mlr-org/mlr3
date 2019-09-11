library(bench)
# library(mlr3)
pkgload::load_all()
lgr::get_logger("mlr3")$set_threshold("warn")


tasks = mlr_tasks$mget(c("iris", "sonar"))
learners = mlr_learners$mget(c("classif.featureless"))
resamplings = mlr_resamplings$mget("subsampling")
measures = mlr_measures$mget(c("classif.acc", "time_train"))

profvis::profvis(
  benchmark(benchmark_grid(tasks, learners, resamplings))
)


data("flights", package = "nycflights13")
data = as.data.table(flights)
data$time_hour = NULL
b = as_data_backend(data[!is.na(arr_delay)])
task = TaskRegr$new("flights", b, target = "arr_delay")

profvis::profvis(
  resample(task, lrn("regr.featureless"), rsmp("cv", folds = 3))
)
