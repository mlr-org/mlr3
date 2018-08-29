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

    fitness_function = function(x, task, learner, resampling, measure, experiments_store, add_info = list(), ...) {
      assert_list(x, names = "named")
      vec_res = fitness_function_vectorized(list(x), task = task, learner = learner, resampling = resampling, measure = measure, experiments_store = experiments_store, add_info = list(), ...)
      vec_res[[1]]
    },

    fitness_function_vectorized = function(xs, task, learner, resampling, measure, experiments_store, add_info(), ...) {
      assert_list(xs, types = "list")
      learners = .mapply(function(x) {
        this_learner = learner$clone()
        this_learner$par_vals = x
        return(this_learner)
      }, xs)
      bench_res = benchmark(tasks = list(task), learners = learners, resamplings = list(resampling), measures = list(measure))
      experiments_store = rbind(experiments_store, bench_res$data)
      bench_res$data$perform
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

  rr_res = resample(learner = lrn, task = tsk, resampling = rsm, measures = msr)

  design_to_learners = function(learner, design) {
    xsl = design_to_list(design)
    .mapply(
      function(pvs, id) {
        lrn2 = lrn$clone()
        lrn2$par_vals = pvs
        lrn2$id = paste0(lrn2$id, id)
        return(lrn2)
      },
      list(pvs = xsl, id = seq_along(xsl)), list()
    )
  }
  lrns = design_to_learners(lrn, par_set$sample(10))
  res1 = benchmark(learners = lrns, tasks = list(tsk), resamplings = list(rsm), measures = list(msr))
  lrns = design_to_learners(lrn, par_set$sample(10))
  res2 = benchmark(learners = lrns, tasks = list(tsk), resamplings = list(rsm), measures = list(msr))
  res12 = BenchmarkResult$new(rbind(res1$data, res2$data))


  tune_rs = TunerRandomSearch$new(terminations = list())
  op = tune_rs$tune(task = tsk, learner = lrn, resampling = rsm, measure = msr, par_set = par_set)
  as.data.frame(op)
}
