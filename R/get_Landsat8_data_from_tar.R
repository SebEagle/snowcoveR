# get_Landsat8_data_from_tar function
#
# This function named 'get_Landsat8_data_from_tar' extracts all needed S2 files and
# stores them in their folder structure
# A txt file is provided that contains the paths to the extracted files
#

#'get_Landsat8_data_from_tar
#'@author Sebastian Buchelt (M.Sc. EAGLE - University of Wuerzburg)
#'@param data_dir Folder directory to tar files
#'@param dir_aim Folder, where extracted files / folder structure should be stored
#'@param used_band string vector with bands, that should be extractet (e.g "band1", "band2",..., "band7")
#'@param add if variable is set TRUE, new layer directories will be added to already existing directory file
#'(mandatory that directory file already exists)
#'@description This function extracts all needed Landsat8 files and stores them in subfolder of the aimfolder structure.
#'@description A txt file is provided that contains the paths to the extracted files
#'
#'
#'

get_Landsat8_data_from_tar <- function(data_dir, dir_aim, used_band, add=TRUE){

  ##### remains
  #dir1 <- '/home/sebi/Schreibtisch/Studium/Master/Daten/Landsat/'
  #dir_aim <- "/home/sebi/Studium/Master/Semester_1/R_project_snowcover/DATA/Landsat/"
  #used_band <- c("band2", "band3", "band4", "band5")
  #used_res <- c("10", "10", "10", "20")


  ##### create a list for the metadata directory textfile
  prod_files <- used_band
  ##### get number of bands that should be extracted
  prod_elements <- length(used_band)

  ##### create a list of all the tar.gz files, that the directory folder contains
  file_list <- list.files(path = data_dir)
  tar_files <- str_subset(file_list, "tar.gz") # get only the zip files (needed, if folder contains other files aswell)
  ##### get the number of tar.gz files in this directory
  tar_elements <- length(tar_files)


  ############### start procedure ################################
  for (list_counter in 1:tar_elements) {
    file1 <- tar_files[list_counter]                        # get one tar.gz-file from the list
    datalist <- data_tarlist(data_dir, file1)               # create list of all files contained in the tar.gz file
    df <- data.frame(datalist)                              # convert tar-filelist to dataframe
    subfolder_ls <- str_replace(file1, ".tar.gz", ".SAFE/") # create a subfolder for output directory to store the data
    dir_store <- paste(dir_aim, subfolder_ls, sep = "")     # subfolder output directory

    for (prod_counter in 1:prod_elements) {
      tar_subfile <- str_subset(df[,1], used_band[prod_counter])                # find band images in tar-filelist
      data_untar(data_dir,file1,dir_store,tar_subfile)                          # untar the needed files to subfolder output directory
      prod_files[prod_counter] <- paste(subfolder_ls, tar_subfile, sep = "")    # safe untarred file to metadata directory list
    }

    filename <- paste(dir_aim,subfolder_ls, "/directories.txt", sep = "")  # create metadata directory filename to store it in subfolder aswell

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
