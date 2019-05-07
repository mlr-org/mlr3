do_package_checks()

do_pkgdown(commit_paths = "docs/", deploy = TRUE)

# only deploy man files in master branch
if (ci_get_branch() == "master" && ci_is_env("TRAVIS_EVENT_TYPE", "cron")) {

  get_stage("deploy") %>%
    add_code_step(devtools::document()) %>%
    add_step(step_push_deploy(commit_paths = c("man/", "DESCRIPTION", "NAMESPACE")))
}
