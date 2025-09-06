abstract_resamplings = "Resampling"
nspath = dirname(system.file("NAMESPACE", package = "mlr3"))
exports = parseNamespaceFile(basename(nspath), dirname(nspath))$exports
compclass_names = setdiff(grep(exports, pattern = "^Resampling", value = TRUE), abstract_resamplings)
compclasses = lapply(compclass_names, get, envir = asNamespace("mlr3"))
test_that_mlr3component_dict(compclasses, dict_constargs = list(), dict_package = "mlr3")
