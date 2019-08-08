# mlr3 0.1.2

* Added functions to ease the creation of objects stored in dictionaries:
  `tsk()`, `gen()`, `lrn()`, `rsp()`, `mea()`.

* `BenchmarkResult` now ensures that the stored `ResampleResult`s are in a
  persistent order. Thus, `ResampleResult`s are now addressed by their position
  instead of their hash.

* New method `BenchmarkResult$hashes()`.

* New method `Task$rename()`.

* New S3 generic `as_benchmark_result()`.

* Removed `ResampleResult$combine()`.

# mlr3 0.1.1

* Initial upload to CRAN.
