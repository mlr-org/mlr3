devtools::load_all(".")
devtools::load_all("/home/marc/Repository/mlr3learners")

## learner$continue

task = tsk("iris")
learner = lrn("classif.xgboost", nrounds = 10)

learner$train(task)

learner$param_set$values$nrounds = 20

learner$continue(task)

learner$param_set$values
learner$model

## workhorse_continue

resampling = rsmp("cv", folds = 3)
resampling$instantiate(task)

rr = resample(task, learner, resampling, store_models = TRUE)
rr$learners[[1]]$param_set$values
rr$learners[[1]]$model$niter

learners = map(rr$learners, function(x) {x$param_set$values$nrounds = 20; x})
learners[[1]]$param_set$values

wlearner = workhorse_continue(1, task, learners[[1]], resampling, store_models = TRUE)
wlearner$learner_state$model

## resample_continue

learner$param_set$values$nrounds = 30

rr2 = resample_continue(task, learner, resampling, rr, store_models = TRUE)

rr2$learners[[1]]$param_set$values
rr2$learners[[1]]$model
rr2$learners[[3]]$param_set$values
rr2$learners[[3]]$model

## benchmark_continue

learner1 = learner$clone()
learner2 = learner$clone()

grid1 = benchmark_grid(task, c(learner1, learner2), resampling)
bmr = benchmark(grid1, store_models = TRUE)
bmr$resample_result(1)$learners

learner1$param_set$values$nrounds = 40
learner2$param_set$values$nrounds = 40

grid2 = data.table(task = list(task),
                   learner = list(learner1, learner2),
                   resampling = list(resampling),
                   resample_results = list(bmr$resample_result(1), bmr$resample_result(2)))

bmr2 = benchmark_continue(grid2, store = TRUE)
bmr2$resample_result(1)$learners
bmr2$resample_result(1)$learners[[1]]$model



