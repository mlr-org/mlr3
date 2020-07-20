root = rprojroot::find_package_root_file()
data = mlr3misc::load_dataset("german", "rchallenge")
saveRDS(data, file = file.path(root, "inst", "extdata", "german_credit.rds"), version = 2L)
