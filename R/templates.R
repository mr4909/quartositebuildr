#' @keywords internal
#'
#' @title `system.file()` wrapper
#'
#' @description Use to access files in `inst/`.
#'
#' @param path A \code{character} path of target file relative to
#' \code{inst} (in development mode).
#'
#'
cfa_sys_file <- function(path) {
  # Works if package is installed
  installed_path <- system.file(path, package = "quartositebuildr")

  # Fallback for dev mode (load_all) — assumes you're working from package root
  dev_path <- file.path("inst", path)

  if (nzchar(installed_path)) {
    return(installed_path)
  } else if (file.exists(dev_path)) {
    return(dev_path)
  } else {
    return("")  # Triggers your custom error if the file can't be found
  }
}

#' @title Short cut to move all files in system.file directory to newly created
#' directory
#'
#' @param fromDir A \code{character} path where files are currently located
#' @param toDir A \code{character} path where you want the files to be
#'
cfa_move_files <- function(fromDir, toDir){

  fromDirList <- list.files(
    file.path(fromDir),
    full.names = TRUE
  )
  Map(
    f = function(x) file.copy(from = x, to = toDir, recursive = TRUE),
    fromDirList
  )
}
