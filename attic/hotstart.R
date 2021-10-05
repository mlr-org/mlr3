library(tictoc)

task = tsk("pima")

# start learner
learner_1 = lrn("classif.debug", iter = 450)
learner_1$train(task)

learner_2 = lrn("classif.debug", iter = 400)
learner_2$train(task)

learner_3 = lrn("classif.rpart")
learner_3$train(task)

learner_4 = lrn("classif.debug", iter = 550)
learner_4$train(task)

hot = HotstartStack$new(list(learner_1, learner_2, learner_3, learner_4))

# target learner
learner = lrn("classif.debug", iter = 500)
hot$adaption_cost(learner = learner, task = task)
hot$adaption_learner(learner, task)


learner$hotstart_stack = hot
learner$train(task)

learner_2$state
learner$state


# test performance

learner = lrn("classif.debug", iter = 1)
learner$train(task)
learners_debug_pima = replicate(1000, learner$clone())


learner = lrn("classif.rpart")
learner$train(task)
learners_rpart_pima = replicate(1000, learner$clone())

hot = HotstartStack$new(c(learners_debug_pima, learners_rpart_pima))

learner = lrn("classif.debug", iter = 500)

tic()
tmp = hot$adaption_cost(learner = learner, task = task)
toc()

tic()
tmp = hot$adaption_learner(learner = learner, task = task)
toc()

# resample
learner_1 = lrn("classif.debug", iter = 1)
resampling = rsmp("cv", folds = 3)
resampling$instantiate(task)

rr = resample(task, learner_1, resampling, store_models = TRUE, store_backends = TRUE)

hot = HotstartStack$new(rr$learners)
learner_2 = lrn("classif.debug", iter = 2)
learner_2$hotstart_stack = hot

rr_2 = resample(task, learner_2, resampling)

# benchmark
task = tsk("pima")
learner_1 = lrn("classif.debug", iter = 1)
resampling = rsmp("cv", folds = 3)
resampling$instantiate(task)

grid = benchmark_grid(task, learner_1, resampling)
bmr = benchmark(grid, store_models = TRUE)

learners = map(seq_len(bmr$n_resample_results), function(i) {
  bmr$resample_result(i)$learners
})

hot = HotstartStack$new(learners[[1]])
learner_2 = lrn("classif.debug", iter = 2)
learner_2$hotstart_stack = hot

learner_2$hotstart_stack$stack$hotstart_learner


grid = benchmark_grid(task, learner_2, resampling)
bmr = benchmark(grid, allow_train_adapt = TRUE)
