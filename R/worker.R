train_worker = function(e, ctrl) {
  data = e$data
  learner = data$learner

  task = data$task$clone(deep = TRUE)$filter(e$train_set)
  log_debug("train_worker: Learner '%s', task '%s' [%ix%i]", learner$id, task$id, task$nrow, task$ncol, namespace = "mlr3")

  enc = encapsulate(ctrl$encapsulate_train)
  res = set_names(enc(learner$train, list(task = task), learner$packages),
    c("model", "train_log", "train_time"))

  if (!is.null(learner$fallback)) {
    fb = assert_learner(learner$fallback)
    log_debug("train_worker: Training fallback learner '%s' on task '%s'", fb$id, task$id, namespace = "mlr3")
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

  task = data$task$clone(deep = TRUE)$filter(e$test_set)
  log_debug("predict_worker: Learner '%s' on task '%s' [%ix%i]", learner$id, task$id, task$nrow, task$ncol, namespace = "mlr3")

  enc = encapsulate(ctrl$encapsulate_predict)
  res = set_names(enc(learner$predict, list(model = model, task = task), learner$packages),
    c("prediction", "predict_log", "predict_time"))
  assert_class(res$prediction, "Prediction")

  res
}

score_worker = function(e, ctrl) {
  data = e$data
  measures = data$measures
  pkgs = unique(unlist(map(measures, "packages")))

  log_debug("score_worker: Learner '%s' on task '%s' [%ix%i]", data$learner$id, data$task$id, data$task$nrow, data$task$ncol, namespace = "mlr3")
  calc_all_measures = function() {
    set_names(lapply(measures, function(m) m$calculate(e)), ids(measures))
  }
  enc = encapsulate(ctrl$encapsulate_score)
  res = enc(calc_all_measures, list(), pkgs)
  return(list(performance = res$result, score_time = res$elapsed))
}

experiment_worker = function(iteration, task, learner, resampling, measures, ctrl, remote = FALSE) {
  if (remote) {
    # restore the state of the master session
    # currently, this only affects logging as we do not use any global options
    logger::log_threshold(ctrl$log_threshold, namespace = "mlr3")
  }

  e = Experiment$new(task, learner, resampling = resampling, iteration = iteration, measures = measures)

  log_info("Running learner '%s' on task '%s (iteration %i/%i)' ...", learner$id, task$id, iteration, resampling$iters, namespace = "mlr3")

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
