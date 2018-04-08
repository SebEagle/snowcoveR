# calculate_snowextent function
#
# This function named 'calculate_snowextent' loads testsite SWIR & green tif files and calculates NDSI values.
# Additionally a simple snow/no-snow classification and a snow/cloud/snowfree-surface classification is produced.
# Therefore thresholds are used:
# Snow: minimum NDSI, minimum reflectance in green band
# Cloud: minimum reflectance in green band and SWIR band
#

#'calculate_snowextent
#'@author Sebastian Buchelt (M.Sc. EAGLE - University of Wuerzburg)
#'@param store_directory Directory where all satellite scenes are stored in subfolders
#'@param s02,ls8 defines, whether data from Sentinel or Landsat subfolder is used
#'@param thresholds_snow_cloud set thresholds for snow extent & cloud cover for the resulting classification.
#'\preformatted{
#' The parameter contains a list of 4 thresholds
#' first:   NDSI minimum value for snowcover
#' second:  minimum green reflectance for snowcover
#' third:   minimum green reflectance for cloudcover
#' fourth:  minimum SWIR reflectance for cloudcover}
#'@description This function named 'calculate_snowextent' loads testsite SWIR & green tif files and calculates NDSI values.
#'Additionally a simple snow/no-snow classification and a snow/cloud/snowfree-surface classification is produced.
#'\preformatted{
#'Therefore thresholds are used:
#'Snow: minimum NDSI, minimum reflectance in green band
#'Cloud: minimum reflectance in green band and SWIR band + existence of snow}
#'
#'

calculate_snowextent <- function(store_directory, s02, ls8, thresholds_snow_cloud){

  # get different thresholds from threshold input list
  thr_NDSI_snow <- thresholds_snow_cloud[1]     # NDSI threshold for snow
  thr_green_snow <- thresholds_snow_cloud[2]    # green reflectance threshold for snow
  thr_green_cloud <- thresholds_snow_cloud[3]   # green reflectance threshold for cloud
  thr_SWIR_cloud <- thresholds_snow_cloud[4]    # SWIR reflectance threshold for cloud

  # determine satellite subfolder(s); Landsat and Sentinel scenes can be calculated at the same time
  if (s02){
    if (ls8){
      satellite = c("Sentinel/", "Landsat/")
    }else{
      satellite = "Sentinel/"
    }
  }else{
    if (ls8){
      satellite = "Landsat/"
    }
  }
  sat_numb = length(satellite) #

  # set subfolder and tif filenames of green and SWIR reflectance
  subfolder = "testsite/"
  needed_bands <- c("green.tif", "SWIR.tif")

  ########## execute procedure for each satellite subfolder
  for (num in 1:sat_numb) {

    ##### get number and names of satellite scenes
    data_store <- paste(store_directory, satellite[num], sep = "")
    file_list <- list.files(path = data_store)
    file_numb <- length(file_list)

    ############### start procedure ################################
    for(file_elements in 1:file_numb){

      # get file directory, where green & SWIR reflectance files are stored
      file_dir <- paste(data_store,file_list[file_elements], "/",subfolder,sep="")
      # get filenames of reflectance files
      filenames <- paste(file_dir,needed_bands,sep="")

      # load raster data
      green <- raster(filenames[1])
      SWIR <- raster(filenames[2])

      ##### check resolution cell of SWIR & green raster: if different aggregate higher resolution to lower
      res_green <- res(green)               # get green resolution
      res_SWIR <- res(SWIR)                 # get SWIR resolution
      if(res_green[1]!=res_SWIR[1]){
        ratio <- res_SWIR/res_green
        if(ratio[1]>1){
          greentoaggregate <- green
          green <- aggregate(greentoaggregate, fact=ratio, fun=mean)    # aggregate green to same resolution as SWIR
        }
      }

      ##### calculate NDSI and store it as a tif file in the 'testsite' subfolder of each Satellite scene folder
      NDSI <- NDSI_calc(green, SWIR)
      writeRaster(NDSI,paste(file_dir,"NDSI.tif", sep = ""), format="GTiff", overwrite=TRUE)

      ##### calculate snowcover as snow/no-snow classification map using NDSI & green reflectance thresholds
      snowtest1 <- raster(NDSI)
      snowtest2 <- raster(NDSI)
      snowtest1[NDSI>thr_NDSI_snow] <- 1      # NDSI minimum threshold
      snowtest1[NDSI<=thr_NDSI_snow] <- 0     # NDSI minimum threshold

      snowtest2[green<thr_green_snow] <- 0    # green reflectance minimum threshold
      snowtest2[green>=thr_green_snow] <- 1   # green reflectance minimum threshold

      snowextent <- snowtest1*snowtest2       # snowextent, where both conditions are fulfilled

      # save snow/no-snow classification as tif-file
      writeRaster(snowextent,paste(file_dir,"snow.tif", sep = ""), format="GTiff", overwrite=TRUE)

      ##### calculate cloudcover using green & SWIR reflectance thresholds, for snow/cloud/free-surface classification map
      cloud1 <- raster(NDSI)
      cloud2 <- raster(NDSI)
      cloud1[green>thr_green_cloud] <- 1      # green reflectance minimum threshold
      cloud1[green<=thr_green_cloud] <- 0     # green reflectance minimum threshold
      cloud2[SWIR>thr_SWIR_cloud] <- 1        # SWIR reflectance minimum threshold
      cloud2[SWIR<=thr_SWIR_cloud] <- 0       # SWIR reflectance minimum threshold
      cloud2[snowextent==1] <- 0              # exclude areas from class cloud, which are already classified as snow
      cloud <- cloud1*cloud2                  # cloud extent, where all 3 conditions are fulfilled

      # create raster for classification: 0 = free surface, 1 = snow, 2 = cloud
      snowextent[cloud==1] <- 2

      # save snow/cloud/free-surface-classification as tif-file
      writeRaster(snowextent,paste(file_dir,"snow_cloud.tif", sep = ""), format="GTiff", overwrite=TRUE)
    }
    ################# end procedure ################################
  }
  ########## end execution for satellite folder(s)
}
