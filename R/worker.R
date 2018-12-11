train_worker = function(e, ctrl) {
  data = e$data
  learner = data$learner
  require_namespaces(learner$packages, sprintf("The following packages are required for learner %s: %%s", learner$id))

  task = data$task$clone(deep = TRUE)$filter(e$train_set)
  pars = c(list(task = task), learner$param_vals)

  log_level(INFO, "Training learner '%s' on task '%s' ...", learner$id, task$id, namespace = "mlr3")

  enc = encapsulate(ctrl$encapsulate_train)
  res = set_names(enc(learner$train, pars),
    c("model", "train_log", "train_time"))

  if (!is.null(learner$fallback)) {
    fb = assert_learner(learner$fallback)
    log_info("Training fallback learner '%s' on task '%s' ...", fb$id, task$id, namespace = "mlr3")
    require_namespaces(fb$packages, sprintf("The following packages are required for fallback learner %s: %%s", learner$id))
    fb_model = try(fb$train(task))
    if (inherits(fb_model, "try-error"))
      stopf("Fallback learner '%s' failed during train", fb$id)
    res$fallback = fb_model
  }

  res
}

predict_worker = function(e, ctrl) {
  data = e$data
  learner = data$learner
  model = data$model

  if (data$train_log$has_condition("error")) {
    if (is.null(learner$fallback))
      stop(sprintf("Unable to predict learner '%s' without model", learner$id))
    learner = learner$fallback
    model = data$fallback
  }
  require_namespaces(learner$packages, sprintf("The following packages are required for learner %s: %%s", learner$id))

  task = data$task$clone(deep = TRUE)$filter(e$test_set)
  pars = c(list(model = model, task = task), learner$param_vals)

  if (ctrl$verbose)
    log_info("Predicting model of learner '%s' on task '%s' ...", learner$id, task$id, namespace = "mlr3")

  enc = encapsulate(ctrl$encapsulate_predict)
  res = set_names(enc(learner$predict, pars),
    c("prediction", "predict_log", "predict_time"))
  assert_class(res$prediction, "Prediction")

  res
}

score_worker = function(e, ctrl) {
  data = e$data
  measures = data$measures
  require_namespaces(unlist(map(measures, "packages")), "The following packages are required for the measures: %s")

  if (ctrl$verbose)
    log_info("Scoring predictions of learner '%s' on task '%s' ...", data$learner$id, data$task$id, namespace = "mlr3")
  calc_all_measures = function() {
    set_names(lapply(measures, function(m) m$calculate(e)), ids(measures))
  }
  enc = encapsulate(ctrl$encapsulate_score)
  res = enc(calc_all_measures, list())
  return(list(performance = res$result, score_time = res$elapsed))
}

experiment_worker = function(iteration, task, learner, resampling, measures, ctrl) {
  e = Experiment$new(task, learner, resampling = resampling, iteration = iteration, measures = measures)

  if (ctrl$verbose) {
    log_info("Running learner '%s' on task '%s (iteration %i/%i)' ...", learner$id, task$id, iteration, resampling$iters, namespace = "mlr3")
    ctrl$verbose = FALSE
  }

  tmp = train_worker(e, ctrl)
  e$data = insert_named(e$data, tmp)

  tmp = predict_worker(e, ctrl)
  e$data = insert_named(e$data, tmp)

  tmp = score_worker(e, ctrl)
  e$data = insert_named(e$data, tmp)

  if (!ctrl$store_prediction)
    e$data["prediction"] = list(NULL)

  if (!ctrl$store_model)
    e$data["model"] = list(NULL)

  remove_named(e$data, c("task", "learner", "resampling", "measures"))
}
