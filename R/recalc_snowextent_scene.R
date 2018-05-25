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

  file_name <- paste(store_directory, "statistics_recalced.txt", sep = "")
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
      sat = "Sentinel"
      dem <- raster(paste(dem_store, "dem20.tif", sep = ""))
    }else{
      dates <- str_sub(file_list, start = 11, end =18)
      sat = "Landsat"
      dem <- raster(paste(dem_store, "dem30.tif", sep = ""))
    }

    #--------------------------------------------------------------------------------------------------
    ##### create dataframe for overall statistics
    sat_col <- rep(sat, file_numb)                                          # create a vector with Satellite name for overall statistics
    snowcover <- vector(mode="numeric", length=file_numb)                   # vector for snow cover
    # create dataframe from vectors above for overall statistics (contains Satellite, date of acquisition, cover percentages)
    stats <- data.frame(sat_col, dates, snowcover)

    ##### calculate dem height pixel distribution for 100m heigth zones
    dem_vector <- as.vector(dem)/height_zone_size                                            # get pixels heigth zone
    dem_int <- as.integer(dem_vector)                                           # discrete values
    min_dem <- min(dem_int)                                                     # get min heigth zone
    max_dem <- max(dem_int)                                                     # get max heigth zone

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



        #--------------------------------------------------------------------------------------------------
        ##### calculate overall snow/cloud cover statistics
        data_vector <- as.vector(new_snow)
        pixel_number <- sum(data_vector >= 0, na.rm = TRUE)
        stats$snowcover[n_elements] <- sum(data_vector == 1, na.rm = TRUE)/pixel_number*100

        #--------------------------------------------------------------------------------------------------
        ############### calculate dem heigth zone statistics ###############

        ##### calculate statistics for each height zone, snowcover/cloudcover/freesurface percentage, by beforehand counted number of pixels per heigth zone
        for (height in min_dem:max_dem) {
          pos_list <- height-min_dem+1
          sub_vector <- data_vector[dem_int==height]
          dem_stats$snowcover[pos_list] <- sum(sub_vector == 1, na.rm = TRUE)/dem_stats$number_of_pixels[pos_list]*100
        }

        dem_stats <- dem_stats[,-c(3,5)]

        ##### store dem height zone statistics in txt-file in Satellite scene subfolder
        dem_stats_dir <- paste(data_store,file_list[n_elements],"/dem_stat_recalc.txt", sep = "")
        write.table(dem_stats,dem_stats_dir,sep=";", row.names = FALSE, col.names = TRUE)

        ############### end calculation of dem heigth zone statistics ###############
        #--------------------------------------------------------------------------------------------------

      }else{
        #print("no")
      }

    }
    ##### merge overall statistics from both satellites
    if(num==1){
      all_stats <- stats
    }else{
      all_stats <- bind_rows(stats,all_stats)
    }
  }
  all_stats <- all_stats[which(all_stats$snowcover>0),]
  ##### save overall statistics as txt-file in project folder (store_directory)
  write.table(all_stats,file_name,sep=";", row.names = FALSE, col.names = TRUE)
}
