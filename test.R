library(devtools)
library(microbenchmark)
load_all()


set.seed(123)

mylrn = lrn("classif.rpart")
mylrn$train(tsk("iris"))

p = mylrn$predict_newdata_fast(iris)
mb = microbenchmark(times = 5, 
  pd1 = {xx = mylrn$predict_newdata(iris)},
  pd2 = {xx = mylrn$predict_newdata_fast(iris)}
)
print(mb)


# library(profvis)

# pv = profvis::profvis({
#     for (i in 1:100) {
#         xx = mylrn$predict_newdata_fast(iris)
#     }
# })
# htmlwidgets::saveWidget(pv, "profile.html")
# #browseURL("profile.html")







