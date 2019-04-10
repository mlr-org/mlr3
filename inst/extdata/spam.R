data = mlr3misc::load_dataset("spam", "kernlab")
saveRDS(data, file = file.path(root, "inst", "extdata", "spam.rds"), version = 2L)
