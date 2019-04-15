if (inherits(ci(), "TravisCI")) {

  do_package_checks(args = "--as-cran", error_on = "error")

}

if (inherits(ci(), "AppVeyorCI")) {

  do_package_checks(
    args = c("--as-cran", "--no-manual", "--no-vignettes", "--no-build-vignettes"),
    build_args = c("--no-build-vignettes"),
    error_on = "error"
  )

}

if (ci_can_push()) {

  do_pkgdown(commit_paths = "docs")
}

# only deploy man files in master branch
if (ci_get_branch() == "master" && ci_is_env("TRAVIS_EVENT_TYPE", "cron")) {

  get_stage("deploy") %>%
    add_code_step(devtools::document()) %>%
    add_step(step_push_deploy(commit_paths = c("man/", "DESCRIPTION", "NAMESPACE")))
}
