# get_Sentinel2_data_from_zip function
#
# This function named 'get_Sentinel2_data_from_zip' extracts all needed S2 files and
# stores them in their folder structure
# A txt file is provided that contains the paths to the extracted files
#

#'get_Sentinel2_data_from_zip
#'@author Sebastian Buchelt (M.Sc. EAGLE - University of Wuerzburg)
#'@param data_dir Folder directory to zip files
#'@param dir_aim Folder, where extracted files / folder structure should be stored
#'@param used_band string vector with bands, that should be extractet (e.g "B02", "B11", "AOT", "SCL")
#'@param used_res string vector with corresponding resolution ("10", "10",...), must be of same size as used_band,
#'
#'Check for available Resolution beforehand!!!
#'
#'@param add if variable is set TRUE, new layer directories will be added to already existing directory file
#'(mandatory that directory file already exists)
#'@description This function named 'get_Sentinel2_data_from_zip' extracts all needed S2 files and stores them in their folder structure.
#'@description A txt file is provided that contains the paths to the extracted files
#'
#'
#'

get_Sentinel2_data_from_zip <- function(data_dir, dir_aim, used_band, used_res, add=TRUE){

  ##### remains
  # data_dir <- '/home/sebi/Studium/Master/Daten/Sentinel/'
  # dir_aim <- "/home/sebi/Studium/Master/Semester_1/R_project_snowcover/DATA/Sentinel"
  # used_band <- c("B02", "B03", "B04", "B05")
  # used_res <- c("10", "10", "10", "20")
  # add=TRUE


  ##### combine bands and resolution to one String, will be used to search for the wanted file within the zip-folder structure
  used_prod <- paste(used_band, used_res, sep = "_")

  ##### create a list for the metadata directory textfile
  prod_files <- used_prod
  ##### get number of bands that should be extracted
  prod_elements <- length(used_prod)

  ##### create a list of all the zip files, that the directory folder contains
  file_list <- list.files(path = data_dir)
  zip_files <- str_subset(file_list, ".zip")   # get only the zip files (needed, if folder contains other files aswell)
  ##### get the number of zip files in this directory
  zip_elements <- length(zip_files)


  ############### start procedure ################################
  for (list_counter in 1:zip_elements) {
    file1 <- zip_files[list_counter]                # get one zip-file from the list
    datalist <- data_ziplist(data_dir, file1)       # create list of all files contained in the zip file
    df <- data.frame(datalist)                      # convert tar-filelist to dataframe

    for (prod_counter in 1:prod_elements) {
      zip_subfile <- str_subset(df[,1], used_prod[prod_counter])    # find band_resolution images in zip-filelist
      data_unzip(data_dir,file1,dir_aim,zip_subfile)                # unzip the needed files to subfolder output directory
      prod_files[prod_counter] <- zip_subfile                       # safe unzipped file to metadata directory list
    }

    file1 <- str_replace(file1, ".zip", ".SAFE/")                   # get the subfolder name where the metadata directory textfile should be stored
    filename <- paste(dir_aim,file1, "directories.txt", sep = "")           # create metadata directory filename to store it in subfolder

    ##### if keyword add=TRUE, new file directories will be added to metadata file
    if(add==TRUE){
      old_df <- read.table(filename)                          # read old metadata file
      prod_files_old <- sapply(old_df, as.character)          # create a character list
      n_old <- length(prod_files_old)                         # get length of old list

      ##### check, if added file directory already exists in metadata file: if not, it will be added to metadata file
      for (files in 1:prod_elements) {                        # check for each added element
        new_f <- prod_files[files]
        exist <- FALSE
        for (old_files in 1:n_old) {                          # if there is already one in the existing old file
          old_f <- prod_files_old[old_files]
          if(new_f==old_f){
            exist <- TRUE
          }
        }
        if(exist==FALSE){
          prod_files_old <- c(prod_files_old[1:n_old],new_f)  # and add it to the file directory list
          n_old <- length(prod_files_old)                     # get new length of prod_files_old, to make adding of several new files possible
        }
      }
      prod_files <- prod_files_old    # store new metadata directories list in the file, that will be saved
    }
    ##### end if keyword add=TRUE ###########

    ##### save metadata file 'directories.txt'
    prod_list <- data.frame(prod_files)                             # convert metadata directory list to dataframe
    write.table(prod_list,filename,sep="", row.names = TRUE, col.names = TRUE)      # save metadata directory textfile
  }
  ###############  end procedure  ################################
}
