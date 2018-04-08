# data_unzip function
#
# This function named 'unzip_data' is created to unzip satelite data zip-files
#

#'data_unzip
#'@author Sebastian Buchelt (M.Sc. EAGLE - University of Wuerzburg)
#'@param input_dir Folder directory to zip file
#'@param inputfile name of zip-file
#'@param subfile get only the files out of the zip-file, that are needed
#'@param output_dir directory, where unzipped files should be stored
#'@description is unzipping files
#'
#'

data_unzip <- function(input_dir, input_file, output_dir, subfile) {
  zip_directory <- paste(input_dir, input_file, sep = "")
  unzip(zip_directory, exdir=output_dir, files = subfile)
}
