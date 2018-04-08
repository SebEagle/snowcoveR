# data_tarlist function
#
# This function named 'data_tarlist' is created to return a list with all the files a tar.gz-file contains
#

#'data_tarlist
#'@author Sebastian Buchelt (M.Sc. EAGLE - University of Wuerzburg)
#'@param input_dir Folder directory to the tar.gz file
#'@param inputfile name of tar.gz-file
#'@description is returning a list of all the files a tar.gz-file contains
#'
#'

data_tarlist <- function(input_dir, input_file) {
  zip_directory <- paste(input_dir, input_file, sep = "")
  untar(zip_directory, list = TRUE)
}
