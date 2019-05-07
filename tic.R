do_package_checks(error_on = "error")

if (ci_has_env("BUILD_PKGDOWN")) {
  do_pkgdown(orphan = TRUE)
}
