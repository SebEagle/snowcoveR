# recalc_snowextent_scene function
#
# This function named 'recalc_snowextent_scene' recalculates snow extent to exclude low probable snow pixels and
# include high probable snow under cloudcover. For that the previously calculated dem height zone probabilities are used (function 'calc_scene_snowstats')
#
#

#'recalc_snowextent_scene
#'@author Sebastian Buchelt (M.Sc. EAGLE - University of Wuerzburg)
#'@param store_directory Directory where all satellite scenes are stored in subfolders
#'@param dem_subdir subdirectory, where DEM files are stored
#'@param height_zone_size defines the size (height difference) of the altitudinal zones for which the snowcover percentages are calculated;
#' must be the same size as used in function 'calc_scene_snowstats'
#'@param threshold1_snow set thresholds for heigth zone snow probability, where pixel will be excluded from the class snow
#'@param threshold2_cloud set thresholds for heigth zone snow probability, above which cloud pixels will be added to the class snow
#'@description This function named 'recalc_snowextent_scene' recalculates snow extent to exclude low probable snow pixels and
#'include high probable snow under cloudcover. For that the previously calculated dem height zone probabilities are used
#'(function 'calc_scene_snowstats')
#'
#'

recalc_snowextent_scene <- function(store_directory, dem_subdir, height_zone_size, threshold1_snow, threshold2_cloud){

  # set subfolder directories
  satellite <-  c("Sentinel/", "Landsat/")    # Satellite subfolders
  subfolder <-  "/testsite/"                  # subfolder within satellite subfolders, where testsite scene are stored
  dem_dir <- paste(dem_subdir,"testsite/", sep = "")            # subfolder, where testsite DEM-files are stored
  dem_store <- paste(store_directory,dem_dir, sep = "")

  numb_satellite <- length(satellite) # get number of satellites used for following for-loop

  #--------------------------------------------------------------------------------------------------
  ########## execute procedure for each satellite subfolder
  for (num in 1:numb_satellite){

    ##### get number and names of satellite scenes
    data_store <- paste(store_directory, satellite[num], sep = "")
    file_list <- list.files(path = data_store)
    file_numb <- length(file_list)

    ##### get date of acquisition for image filename from satellite scene foldername, and DEM file with the fitting resolution
    if(satellite[num]=="Sentinel/"){
      dates <- str_sub(file_list, start = 12, end =19)
      dem <- raster(paste(dem_store, "dem20.tif", sep = ""))
    }else{
      dates <- str_sub(file_list, start = 11, end =18)
      dem <- raster(paste(dem_store, "dem30.tif", sep = ""))
    }

    ##### adjust dem values so that they fit to the row number of the heigth zone statistics
    dem <- as.integer(dem/height_zone_size)
    dem <- dem-cellStats(dem, stat='min', na.rm=TRUE)+1

    ###################################################################################################
    ############### start procedure ################################
    for (n_elements in 1:file_numb){

      ##### start procedure, if Satellite scene is within the thresholds tested in calculate_stats.R and therefore used.txt contains TRUE
      txt_dir <- paste(data_store,file_list[n_elements],"/used.txt", sep = "")      # get txt-file, where is written, whether new_snow should be calculated or not
      data_used <- read.table(txt_dir)                                              # read it
      if(data_used$V1){                                                             # check, if new_snow should be calculated
        #print(file_list[n_elements])

        #--------------------------------------------------------------------------------------------------
        ##### read DEM heigth zone statistics from txt-file
        dem_stats_dir <- paste(data_store,file_list[n_elements],"/dem_stat.txt", sep = "")
        dem_stats <- read.table(dem_stats_dir, header = TRUE, sep = ";")

        snow_prob <- sapply(dem_stats$snowcover, as.numeric)        # create vector with heigth zone percentage of snow
        surface_prob <- sapply(dem_stats$freesurface, as.numeric)   # create vector with heigth zone percentage of freesurface

        #--------------------------------------------------------------------------------------------------
        ########## create a raster with snowcover probability for each pixel, based on the height zone dependend percentage of snow and freesurface areas
        snow_prob_raster <- setValues(raster(dem),0)
        for (height_zone in cellStats(dem, stat='min', na.rm=TRUE):cellStats(dem, stat='max', na.rm=TRUE)) {
          # snow cover probability = area snow[height zone of pixel]/(area snow + area freesurface)[height zone of pixel] in percent
          snow_prob_raster[dem==height_zone] <- (snow_prob[height_zone]/(snow_prob[height_zone]+surface_prob[height_zone]))*100
        }

        #--------------------------------------------------------------------------------------------------
        # get file directory, where needed tif file snow_cloud is stored
        file_dir <- paste(data_store,file_list[n_elements],subfolder,"snow_cloud.tif", sep = "")
        snow_cloud_cover <- raster(file_dir)    # read raster

        #--------------------------------------------------------------------------------------------------
        ########## create new raster with new calculated snow cover
        new_snow <- setValues(raster(snow_cloud_cover),0)
        # reproject raster if they have different extent
        if(extent(snow_prob_raster)!=extent(snow_cloud_cover)){
          snow_prob_raster <- projectRaster(snow_prob_raster, snow_cloud_cover, method = "ngb")
        }

        # thresholds for new snowcover
        new_snow[(snow_cloud_cover==1)&(snow_prob_raster>threshold1_snow)] <- 1      # turn snow into no snow, if height zone probability for snow is below threshold1
        new_snow[(snow_cloud_cover==2)&(snow_prob_raster>threshold2_cloud)] <- 1      # turn cloud into snow, if heigth zone probability for snow is above threshold2

        #--------------------------------------------------------------------------------------------------
        ##### save new_snow raster
        save_dir <- paste(data_store,file_list[n_elements],subfolder,"new_snow.tif", sep = "")
        writeRaster(new_snow,save_dir, format="GTiff", overwrite=TRUE)

      }else{
        #print("no")
      }

    }
  }
}
