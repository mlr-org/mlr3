# add_package_checks()

get_stage("script") %>%
  add_step(step_rcmdcheck(args = "--as-cran", error_on = "error"))
  # add_code_step(devtools::build(manual = TRUE)) %>%
  # add_code_step(devtools::install(quick = TRUE)) %>%
  # add_code_step(rcmdcheck::rcmdcheck(path = "../mlr3_0.1.0-9000.tar.gz", args = "--as-cran",
  #                                    error_on = "error"))


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
}

if (inherits(ci(), "TravisCI") && !Sys.getenv("$TRAVIS_EVENT_TYPE") == "cron") {

  get_stage("deploy") %>%
    add_code_step(devtools::document()) %>%
    add_step(step_push_deploy(commit_paths = "man/"))
}
