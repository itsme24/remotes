DEFAULT_RSPM_REPO_ID <-  "12" # cran
DEFAULT_RSPM <-  "https://solo.rstudiopm.com"

#' Query the system requirements for a dev package (and its dependencies)
#'
#' Returns a character vector of system commands to run, if you want to run '
#' @param path The path to the dev package's root directory.
#' @param os,os_release The operating system and operating system release version, see
#'   <https://github.com/rstudio/r-system-requirements#operating-systems> for the
#'   list of supported operating systems.
#' @param curl The location of the curl binary on your system.
#' @return A character vector of commands to run which will install the system requirements for the package.
#' @export
system_requirements <- function(os, os_release, path = ".", curl = Sys.which("curl")) {
  os_versions <- supported_os_versions()

  os <- match.arg(os, names(os_versions))

  os_release <- match.arg(os_release, os_versions[[os]])

  rspm <- Sys.getenv("RSPM", DEFAULT_RSPM)
  rspm_repo_id <- Sys.getenv("RSPM_REPO_ID", DEFAULT_RSPM_REPO_ID)
  rspm_repo_url <- sprintf("%s/__api__/repos/%s", rspm, rspm_repo_id)

  desc_file <- normalizePath(file.path(path, "DESCRIPTION"))

  res <- system2(curl, args = c("--silent", "--data-binary", paste0("@", desc_file), sprintf("%s/sysreqs?distribution=%s&release=%s&suggests=true", rspm_repo_url, os, os_release)), stdout = TRUE)

  res <- json$parse(res)

  pre_install <- unique(unlist(c(res[["pre_install"]], lapply(res[["dependencies"]], `[[`, "pre_install"))))

  install_scripts <- unique(unlist(c(res[["install_scripts"]], lapply(res[["dependencies"]], `[[`, "install_scripts"))))

  as.character(c(pre_install, install_scripts))
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
    "sle" = c("12.3", "15.0")
    #"windows" = c("")
  )
}
