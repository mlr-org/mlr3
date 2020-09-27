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

learners = map(rr$learners, function(x) {x$param_set$values$nrounds = 30; x})
learners[[1]]$model
learners[[1]]$param_set$values

rr2 = resample_continue(task, learners, resampling, store_models = TRUE)

rr2$learners[[1]]$param_set$values
rr2$learners[[1]]$model

## benchmark_continue

grid1 = benchmark_grid(task, c(learner, learner), resampling)
bmr = benchmark(grid1, store_models = TRUE)

rr1_bmr = bmr$resample_result(1)
rr1_bmr_learners = rr1_bmr$learners
rr1_bmr_learners  = map(rr1_bmr_learners , function(x) {x$param_set$values$nrounds = 30; x})

rr1_bmr_learners[[1]]$id = paste0(rr1_bmr_learners[[1]]$id, 1)
rr1_bmr_learners[[2]]$id = paste0(rr1_bmr_learners[[2]]$id, 2)
rr1_bmr_learners[[3]]$id = paste0(rr1_bmr_learners[[3]]$id, 3)

rr2_bmr = bmr$resample_result(2)
rr2_bmr_learners = rr2_bmr$learners
rr2_bmr_learners  = map(rr2_bmr_learners , function(x) {x$param_set$values$nrounds = 30; x})

rr2_bmr_learners[[1]]$id = paste0(rr2_bmr_learners[[1]]$id, 4)
rr2_bmr_learners[[2]]$id = paste0(rr2_bmr_learners[[2]]$id, 5)
rr2_bmr_learners[[3]]$id = paste0(rr2_bmr_learners[[3]]$id, 6)

grid2 = data.table(task = list(task),
                   learners = list(rr1_bmr_learners, rr2_bmr_learners),
                   resampling = list(resampling))

bmr2 = benchmark_continue(grid2, store = TRUE)

bmr2$resample_result(1)$learners[[1]]
bmr2$resample_result(1)$learners[[1]]$model



