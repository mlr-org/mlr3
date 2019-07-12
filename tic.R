do_package_checks(error_on = "error")

if (ci_has_env("BUILD_PKGDOWN")) {
  do_pkgdown(orphan = TRUE)
}

get_stage("after_success") %>%
  add_code_step(system("bash ./inst/trigger-mlr3book.sh"))
