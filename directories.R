username = Sys.info()["login"]

# Set project-specific directories based on the username
project_directories <- list(
  root = list(
    wdir = "/Users/floriankuschel/PoD_Review",
    data_dir = file.path("/Users/floriankuschel/PoD_Review", 'data')
  ))

# Use a default directory if the username is not in the list
selected_project <- project_directories[[username]]

if (is.null(selected_project)) {
  wdir <- default_wdir
  data_dir <- default_data_dir
} else {
  wdir <- selected_project$wdir
  data_dir <- selected_project$data_dir
}

setwd(wdir)
