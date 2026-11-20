#' @title Function to check Drive paths and create them if they do not exist
#' 
#' @description
#' This function checks the Google Drive of the config files listed Google email
#' address to assure that the necessary folder architecture is present and creates
#' the folder architecture if it does not exist.
#' 
#' @param folder string; sub folder to be checked/created within the project folder
#' including an additional sub folder within it called "stable"
#'
#' @param google_email A string containing the gmail address to use for
#' Google Drive authentication.
#' 
#' @param project_folder A string containing the parent folder within Google Drive
#' in which to create the sub folders. This is pulled from the config file.
#'
#' @param local_drive Logical; if TRUE, create folders on the local filesystem
#' mirror instead of Google Drive.
#' 
#' @returns this function runs without an explicit return object
#' 
#' @examples
#' check_drive_download_paths(folder = p0_drive_folders,
#'                            google_email = p0_workflow_config$google_email,
#'                            project_folder = p0_workflow_config$drive_project_folder)
#' 
check_drive_download_paths <- function(folder, google_email, project_folder,
                                       local_drive = FALSE) {
  if (isTRUE(local_drive)) {
    target_dir <- file.path(sub("/+$", "", project_folder), folder)
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(target_dir, "stable"), recursive = TRUE, showWarnings = FALSE)
    return(invisible(target_dir))
  }
  tryCatch({
    drive_auth(google_email)
    drive_ls(path = paste0(project_folder, folder))
  }, error = function(e) {
    # if the outpath doesn't exist, create it along with a "stable" subfolder
    drive_mkdir(name = folder,
                path = project_folder)
    drive_mkdir(name = "stable",
                path = paste0(project_folder,
                              folder))
  })
}