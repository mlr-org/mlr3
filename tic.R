add_package_checks(args = "--as-cran", warnings_are_errors = FALSE,
                   notes_are_errors = FALSE,
                   build_args = c("--no-build-vignettes"))

if (Sys.getenv("id_rsa") != "") {
  # pkgdown documentation can be built optionally. Other example criteria:
  # - `inherits(ci(), "TravisCI")`: Only for Travis CI
  # - `ci()$is_tag()`: Only for tags, not for branches
  # - `Sys.getenv("BUILD_PKGDOWN") != ""`: If the env var "BUILD_PKGDOWN" is set
  # - `Sys.getenv("TRAVIS_EVENT_TYPE") == "cron"`: Only for Travis cron jobs
  get_stage("before_deploy") %>%
    add_step(step_setup_ssh())

  get_stage("deploy") %>%
    add_step(step_build_pkgdown()) %>%
    add_step(step_push_deploy(path = "docs", branch = "gh-pages"))

  if (!Sys.getenv("$TRAVIS_EVENT_TYPE") == "cron") {

    get_stage("deploy") %>%
      add_code_step(devtools::document()) %>%
      add_step(step_push_deploy(commit_paths = "man/"))
  }
}
