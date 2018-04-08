# get_testsite_Data function
#
# This function named 'get_testsite_Data' loads entire satellite scene tif files of selected bands and
# crops them to a predefined extent, the result is stored in a new subfolder 'testsite' in each acquisition folder
#
#

#'get_testsite_Data
#'@author Sebastian Buchelt (M.Sc. EAGLE - University of Wuerzburg)
#'@param store_directory Directory where all satellite scenes are stored in subfolders
#'@param s02,ls8 defines, whether data from Sentinel or Landsat subfolder is used
#'@param ref_CRS used coordinate reference system; in case data is stored in different coordinate systems,
#' it will be reprojected to this coordinate system
#'@param ext extent of testsite the scenes will be cropped to
#'@param res if data needs to be reprojected, this will be the resolution of the resulting raster file
#'@param needed_bands define bands, which should be cropped (these are the input file names - satellite specific)
#'(e.g "band1", "band5", "B04", "B08")
#'@param name_bands define name of output tif-files (e.g "green", "NIR", "SWIR")
#'@description This function named 'get_testsite_Data' loads entire satellite scene tif files of selected bands and
#'crops them to a predefined extent, the result is stored in a new subfolder 'testsite' in each acquisition folder
#'
#'
#'
#'

get_testsite_Data <- function(store_directory, s02, ls8, ref_CRS, ext, res, needed_bands, name_bands){

  # create example raster in case that reprojection is needed
  example_raster <- raster(crs=ref_CRS, ext=ext,resolution=res)

  # determine satellite subfolder
  if (s02){
    satellite = "Sentinel/"
  } else{
    if (ls8){
      satellite = "Landsat/"
    }
  }

  # get number and names of satellite scenes
  data_store <- paste(store_directory, satellite, sep = "")
  file_list <- list.files(path = data_store)
  file_numb <- length(file_list)

  # get number of bands which should be cropped
  n_bands <- length(needed_bands)

  ############### start procedure ################################
  for(file_elements in 1:file_numb){
    filename <- paste(data_store,file_list[file_elements], "/",sep="")      # get name of satellite scene folder

    # create new subfolder 'testsite', where testsite data is stored
    file_dir <- paste(filename, "testsite/", sep = "")
    dir.create(file_dir, showWarnings = FALSE)

    # read metadata with stored tif-file directories
    metadata_name <- paste(filename, "directories.txt", sep = "")           # get metadata filename
    metadata <- read.table(metadata_name)                                   # read table

    data_numb <- nrow(metadata)               # get length of filelist
    all_directories <- character(data_numb)   # create a list of characters for all existing directories
    band_directories <- character(n_bands)    # create a list for directories of needed bands, that will be cropped

    # load existing directory strings into created character list
    for (data_names in 1:data_numb){
      all_directories[data_names] <- paste(data_store,metadata[data_names,1], sep = "")
    }

    # get needed directories from all directories
    for (data_names in 1:n_bands){
      band_directories[data_names]<- str_subset(all_directories, needed_bands[data_names])
    }


    ##### execute raster loading, cropping and saving result
    for (bands in 1:n_bands) {
      raster_data <- raster(band_directories[bands])                      # load raster data
      projection_raster <- projection(raster_data, asText=FALSE)          # get projection of raster data

      if(str_detect(projection_raster, "zone=33")){                       # if raster has different coordinate system,
        raster_crop <- projectRaster(raster_data,example_raster,res = 30) # reproject it to reference coordinate system
                                                                          # and the needed extent of the testsite
      }else{
        raster_crop <- crop(raster_data, ext)                             # if coordinate system is the same,
      }                                                                   # just crop it to testsite extent

      # store cropped data in new subfolder 'testsite'
      writeRaster(raster_crop,paste(file_dir,name_bands[bands],".tif", sep = ""), format="GTiff", overwrite=TRUE)
    }
  }
  ################# end procedure ################################
}
