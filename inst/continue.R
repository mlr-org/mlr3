devtools::load_all(".")
devtools::load_all("/home/marc/Repository/mlr3learners")

task = tsk("iris")
learner = lrn("classif.xgboost", nrounds = 10) 
learner$train(task)
learner$model

learner$param_set$values$nrounds = 20
learner$continue(task)

learner$model


task = tsk("iris")
learner1 = lrn("classif.xgboost", nrounds = 10, eta = 0.1) 
learner2 = lrn("classif.xgboost", nrounds = 10, eta = 0.5) 
learners = list(learner1, learner2)
resampling = rsmp("cv", folds = 3)
design = benchmark_grid(task, learners, resampling)
bmr = benchmark(design, store_models = TRUE)

learners[[1]]$param_set$values$nrounds = 20
learners[[2]]$param_set$values$nrounds = 20
bmr2 = benchmark_continue(learners, bmr, store_models = TRUE)

task = tsk("iris")
learner = lrn("classif.xgboost", nrounds = 10) 
resampling = rsmp("cv", folds = 3)
rr = resample(task, learner, resampling, store_models = TRUE)

learner$param_set$values$nrounds = 20
rr2 = resample_continue(learner, rr, store_models = TRUE)