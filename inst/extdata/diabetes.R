# Generation script for the synthetic `diabetes` task fixture.
# The data set mimics the structure of the former `pima` task (identical
# columns and a binary `diabetes` target with a positive class `"pos"`) and
# contains deterministic missing values for preprocessing tests.
# It is fully synthetic and carries no real patient data.

make_diabetes_fixture = function(n = 128L, seed = 20260724L) {
  stopifnot(
    length(n) == 1L,
    is.finite(n),
    n == as.integer(n),
    n >= 16L
  )
  n = as.integer(n)

  withr::with_seed(seed, {
    pregnant = rpois(n, lambda = 4)
    age = pmin(80L, 21L + rpois(n, lambda = 13))

    glucose = round(pmax(40, rnorm(n, 118 + pregnant, 25)))
    pressure = round(pmax(35, rnorm(n, 71, 11)))
    triceps = round(pmax(5, rnorm(n, 26, 8)))
    insulin = round(pmax(10, rlnorm(n, log(85), 0.65)))
    mass = round(pmax(16, rnorm(n, 32, 6)), 1)
    pedigree = round(rgamma(n, shape = 2, rate = 4), 3)

    score = 0.035 * glucose +
      0.06 * mass +
      0.018 * age +
      0.35 * pedigree +
      rnorm(n)

    diabetes = rep("neg", n)
    diabetes[
      order(score, decreasing = TRUE)[seq_len(max(1L, round(0.35 * n)))]
    ] = "pos"

    x = data.frame(
      pregnant = pregnant,
      glucose = glucose,
      pressure = pressure,
      triceps = triceps,
      insulin = insulin,
      mass = mass,
      pedigree = pedigree,
      age = age,
      diabetes = factor(diabetes, levels = c("neg", "pos"))
    )

    # Deterministic missingness for preprocessing tests.
    missing_columns = c(
      "glucose", "pressure", "triceps", "insulin", "mass"
    )
    for (i in seq_along(missing_columns)) {
      rows = seq.int(i, n, by = 23L + i)
      x[rows, missing_columns[[i]]] = NA
    }

    x
  })
}

root = rprojroot::find_package_root_file()
diabetes = make_diabetes_fixture()
saveRDS(diabetes, file = file.path(root, "inst", "extdata", "diabetes.rds"), version = 2L)
