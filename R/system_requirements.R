DEFAULT_RSPM_REPO_ID <-  "2" # cran
DEFAULT_RSPM <-  "https://packagemanager.rstudio.com"

#' Query the system requirements for a package (and its dependencies)
#'
#' Returns a character vector of system commands to run, if you want to run '
#' @param package The package name to query. If `NULL` the package name is
#'   looked up from a DESCRIPTION file in the current working directory.
#' @param os,os_release The operating system and operating system release version, see
#'   <https://github.com/rstudio/r-system-requirements#operating-systems> for the
#'   list of supported operating systems.
#' @return A character vector of commands to run which will install the system requirements for the package.
#' @export
system_requirements <- function(package = NULL, os, os_release) {
  os_versions <- supported_os_versions()

  os <- match.arg(os, names(os_versions))

  os_release <- match.arg(os_release, os_versions[[os]])

  rspm <- Sys.getenv("RSPM", DEFAULT_RSPM)
  rspm_repo_id <- Sys.getenv("RSPM_REPO_ID", DEFAULT_RSPM_REPO_ID)
  rspm_repo_url <- sprintf("%s/__api__/repos/%s", rspm, rspm_repo_id)

  tmp <- tempfile()
  download(tmp, sprintf("%s/packages?name=%s", rspm_repo_url, package))

  pkg_version_id <- json$parse_file(tmp)[[1]][["id"]]

  tmp2 <- tempfile()
  download(tmp2, sprintf("%s/packages/%s/sysreqs?id=%s&distribution=%s&release=%s", rspm_repo_url, package, pkg_version_id, os, os_release))

  res <- json$parse_file(tmp2)

  pre_install <- unique(unlist(c(res[["pre_install"]], lapply(res[["dependencies"]], `[[`, "pre_install"))))

  install_scripts <- unique(unlist(c(res[["install_scripts"]]), lapply(res[["dependencies"]], `[[`, "install_scripts")))

  c(pre_install, install_scripts)
}


# Adapted from https://github.com/rstudio/r-system-requirements/blob/master/systems.json
# OSs commented out are not currently supported by the API
supported_os_versions <- function() {
  list(
    #"debian" = c("8", "9"),
    "ubuntu" = c("14.04", "16.04", "18.04"),
    "centos" = c("6", "7", "8"),
    "redhat" = c("6", "7", "8"),
    "opensuse" = c("42.3", "15.0"),
    "sle" = c("12.3", "15.0"),
    #"windows" = c("")
  )
}
