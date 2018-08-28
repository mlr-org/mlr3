TunerBase = R6Class("TunerBase",
  public = list(
    # member variables
    packages = NULL,
    properties = NULL,
    id = NULL,
    tuner_settings = NULL,
    terminations = NULL,

    # constructor
    initialize = function(packages, properties, id, tuner_settings, terminations) {
      self$packages =assert_character(packages, any.missing = FALSE, unique = TRUE)
      self$properties = assert_character(properties, any.missing = FALSE, unique = TRUE)
      self$id = assert_character(id)
      self$tuner_settings = assert_list(tuner_settings, names = "named")
      self$terminations = assert_list(terminations)
    },

    # public methods
    tune = function(task, learner, resampling, measure, par_set) {
      stop("Not implemented!")
    },

    fitness_function = function(x, task, learner, resampling, measure, opt_path,...) {
      assert_list(x, names = "named")
      lrn$par_vals = x
      res = resample(task = task, learner = learner, resampling = resampling, measure = measure)
      y = res$performance[[2]]
      y = mean(y)
      opt_path$add(x = x, y = y)
      return(y)
    }
  )
)

TunerRandomSearch = R6Class("TunerRandomSearch",
  inherit = TunerBase,
  public = list(
    # constructor
    initialize = function(id = "rs", terminations) {
      super$initialize(packages = character(), properties = character(), id = id, tuner_settings = list(), terminations = terminations)
    },

    # public methods
    tune = function(task, learner, resampling, measure, par_set) {
      opt_path = OptPath$new(par_set = par_set, y_names = "y", minimize = TRUE, check_feasible = TRUE)
      for (i in 1:10) {
        x = par_set$sample(1)
        x = par_set$transform(x)
        x = as.list(x)
        self$fitness_function(x, task, learner, resampling, measure, opt_path)
      }
      return(opt_path)
    }
  )
)

if (FALSE) {
  devtools::load_all()
  library(paradox)
  library(R6)
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

  tune_rs = TunerRandomSearch$new(terminations = list())
  op = tune_rs$tune(task = tsk, learner = lrn, resampling = rsm, measure = msr, par_set = par_set)
  as.data.frame(op)
}
