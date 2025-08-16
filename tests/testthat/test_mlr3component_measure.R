
abstract_measures = c("Measure", "MeasureClassif", "MeasureRegr", "MeasureSimilarity",
  "MeasureClassifSimple", "MeasureRegrSimple", "MeasureSimilaritySimple", "MeasureBinarySimple")
nspath = dirname(system.file("NAMESPACE", package = "mlr3"))
exports = parseNamespaceFile(basename(nspath), dirname(nspath))$exports
compclass_names = setdiff(grep(exports, pattern = "^Measure", value = TRUE), abstract_measures)
compclasses = lapply(compclass_names, get, envir = asNamespace("mlr3"))
constargs = list(
  MeasureElapsedTime = list(stages = "train")
)
test_that_mlr3component_dict(compclasses, dict_constargs = constargs, dict_package = "mlr3")
