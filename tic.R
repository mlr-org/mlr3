if (ci_has_env("TravisCI")) {

  do_package_checks(args = "--as-cran", error_on = "error")

}

if (ci_has_env("AppVeyorCI")) {

  do_package_checks(
    args = c("--as-cran", "--no-manual", "--no-vignettes", "--no-build-vignettes"),
    build_args = "--no-build-vignettes", error_on = "error")
}

if (ci_has_env("id_rsa")) {

  get_stage("before_deploy") %>%
    add_step(step_setup_ssh())

  get_stage("deploy") %>%
    add_step(step_build_pkgdown()) %>%
    add_step(step_push_deploy(commit_paths = "docs"))
}

# only deploy man files on Travis on non-cron builds
# only run codecov on Travis
if (ci_has_env("TravisCI") && !ci_is_env("TRAVIS_EVENT_TYPE", "cron")) {

  if (ci()$get_branch() == "master") {
    get_stage("deploy") %>%
      add_code_step(devtools::document()) %>%
      add_step(step_push_deploy(commit_paths = c("man/", "DESCRIPTION", "NAMESPACE")))
  }

  get_stage("after_deploy") %>%
    add_code_step(covr::codecov(quiet = FALSE))
}
