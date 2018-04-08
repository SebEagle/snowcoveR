# data_ziplist function
#
# This function named 'data_ziplist' is created to return a list with all the files a data zip-file contains
#

#'data_ziplist
#'@author Sebastian Buchelt (M.Sc. EAGLE - University of Wuerzburg)
#'@param input_dir Folder directory to zip file
#'@param inputfile name of zip-file
#'@description is returning a list of all the files a zip-file contains
#'
#'

data_ziplist <- function(input_dir, input_file) {
  zip_directory <- paste(input_dir, input_file, sep = "")
  unzip(zip_directory, list = TRUE)
}
