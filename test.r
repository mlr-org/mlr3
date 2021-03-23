devtools::load_all("../mlr3learners")

self = lrn("regr.lm")
self$train(tsk("mtcars"))
p = self$predict(tsk("mtcars"))

self$loglik()
m = msr("aic")
p$score(m, learner = self)
m = msr("bic")
p$score(m, learner = self)
