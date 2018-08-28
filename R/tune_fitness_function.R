tune_fitness_function = function(x, ...) {
  assert_list(x, names = "named")
  lrn$par_vals = x
  res = resample(task = tsk, learner = lrn, resampling = rsm, measures = msr)
  return(res)
}

tune_fitness_function_aggregated = function(x, ...) {
  tune_fitness_function(x, ...)
  y = res$score
  opt_path$add(x = x, y = y)
}

if (FALSE) {
  lrn = mlr_learners$get("classif.rpart")
  tsk = mlr_tasks$get("iris")
  rsm = mlr_resamplings$get("cv")
  msr = mlr_measures$get("mmce")
  par_set = ParamSet$new(params = list(
    ParamInt$new(id = "minsplit", lower = 1L, upper = 10L),
    ParamReal$new(id = "cp", lower = 0, upper = 1)
  ))
  opt_path = OptPath$new(par_set = par_set, y_names = "y", minimize = TRUE, check_feasible = TRUE)
  x = list(minsplit = 10, cp = 0.2)
}
