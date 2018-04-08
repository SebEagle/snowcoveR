# data_untar function
#
# This function named 'unzip_data' is created to unzip satelite data zip-files
#

#'data_untar
#'@author Sebastian Buchelt (M.Sc. EAGLE - University of Wuerzburg)
#'@param input_dir Folder directory to tar.gz file
#'@param inputfile name of tar.gz-file
#'@param subfile get only the files out of the tar.gz-file, that are needed
#'@param output_dir directory, where untarred files should be stored
#'@description is untarring files
#'
#'

data_untar <- function(input_dir, input_file, output_dir, subfile) {
  zip_directory <- paste(input_dir, input_file, sep = "")
  untar(zip_directory, exdir=output_dir, files = subfile)
}
