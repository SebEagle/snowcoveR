# calc_scene_snowstats function
#
# This function named 'calc_scene_snowstats' loads the snow/cloud/freesurface classification tif file and calculates cover percentages.
# This is done for the whole testsite, to decide, whether the scene will be used for the timeseries or not. (cloud cover less than threshold, cloud cover less than freesurface)
# This is stored as TRUE or FALSE in a text-file in the satellite scene subfolder.
# Additionally snow/cloud/freesurface cover percentages are also calculated for each height zone derrived from DEM file and stored as table in satellite scene subfolder.
#
#

#'calc_scene_snowstats
#'@author Sebastian Buchelt (M.Sc. EAGLE - University of Wuerzburg)
#'@param store_directory Directory where all satellite scenes are stored in subfolders
#'@param dem_subdir subdirectory, where DEM files are stored
#'@param height_zone_size defines the size (height difference) of the altitudinal zones for which the snowcover percentages are calculated
#'@param thresholds_cloud thresholds for cloud cover to decide, whether the scene will be used in time series or not
#'
#'@description This function named 'calc_scene_snowstats' loads the snow/cloud/freesurface classification tif file and calculates cover percentages.
#'\preformatted{
#'This is done for the whole testsite, to decide, whether the scene will be used
#'for the timeseries or not. Used Thresholds for decision:
#'      - cloud cover less than input threshold
#'      - cloud cover less than freesurface
#'  => Decision is stored as TRUE or FALSE in a text-file in the satellite scene subfolder.}
#'
#'Additionally snow/cloud/freesurface cover percentages are also calculated for each height zone derrived from DEM file and stored as table in satellite scene subfolder.
#'
#'

calc_scene_snowstats <- function(store_directory, dem_subdir, height_zone_size, threshold_cloud, last_snow=FALSE){


  file_name <- paste(store_directory, "statistics.txt", sep = "")
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
    cloudcover <- vector(mode="numeric", length=file_numb)                  # vector for cloud cover
    snowcover <- vector(mode="numeric", length=file_numb)                   # vector for snow cover
    freesurface <- vector(mode="numeric", length=file_numb)                 # vector for free surface
    used <- vector(mode="logical", length=file_numb)                 # vector for free surface
    # create dataframe from vectors above for overall statistics (contains Satellite, date of acquisition, cover percentages)
    stats <- data.frame(sat_col, dates, cloudcover, snowcover, freesurface, used)

    #--------------------------------------------------------------------------------------------------
    ##### calculate dem height pixel distribution for 100m heigth zones
    dem_vector <- as.vector(dem)/height_zone_size                                            # get pixels heigth zone
    dem_int <- as.integer(dem_vector)                                           # discrete values
    min_dem <- min(dem_int)                                                     # get min heigth zone
    max_dem <- max(dem_int)                                                     # get max heigth zone
    dem_class <- seq(from = min_dem*height_zone_size, to = max_dem*height_zone_size, by =height_zone_size)             # create vector with heigth zone values (eg. 500, 600, 700, ....)
    length_dem <- length(dem_class)                                             # get number of height zones
    number_of_pixels <- vector(mode="numeric",length = length_dem)              # create vector for total number of pixels in each heigth zone, needed for cover percentages
    for (height in min_dem:max_dem) {
      number_of_pixels[height-min_dem+1] <- sum(dem_int==height, na.rm = TRUE)  # count pixels in height zones and store in just created vector
    }


    ###################################################################################################
    ############### start procedure ################################
    for (n_elements in 1:file_numb){

      # get file directory, where needed tif files are stored
      file_dir <- paste(data_store,file_list[n_elements],subfolder,"snow_cloud.tif", sep = "")

      snow_cloud_cover <- raster(file_dir)    # load raster

      #--------------------------------------------------------------------------------------------------
      ##### calculate overall snow/cloud cover statistics
      data_vector <- as.vector(snow_cloud_cover)
      pixel_number <- sum(data_vector >= 0, na.rm = TRUE)
      stats$freesurface[n_elements] <- sum(data_vector == 0, na.rm = TRUE)/pixel_number*100
      stats$snowcover[n_elements] <- sum(data_vector == 1, na.rm = TRUE)/pixel_number*100
      stats$cloudcover[n_elements] <- sum(data_vector == 2, na.rm = TRUE)/pixel_number*100

      #--------------------------------------------------------------------------------------------------
      ##### if cloudcover is higher than freesurface or cloudcover is above a threshold, write FALSE in a text file
      ##### if FALSE is written, this dataset won't be used for further calculations and the time series
      txt_dir <- paste(data_store,file_list[n_elements],"/used.txt", sep = "")
      if(stats$cloudcover[n_elements]>threshold_cloud||stats$cloudcover[n_elements]>stats$freesurface[n_elements]){
        data_used <- FALSE
      }else{
        data_used <- TRUE
      }
      stats$used[n_elements] <- data_used
      # write txt-file with TRUE or FALSE in Satellite scene subfolder
      write(data_used, file = txt_dir)


      #--------------------------------------------------------------------------------------------------
      ############### calculate dem heigth zone statistics ###############
      ##### create dataframe for dem heigth zone statistics
      cloudcover <- vector(mode="numeric", length=length_dem)
      snowcover <- vector(mode="numeric", length=length_dem)
      freesurface <- vector(mode="numeric", length=length_dem)
      dem_stats <- data.frame(dem_class,number_of_pixels, cloudcover, snowcover, freesurface)

      ##### calculate statistics for each height zone, snowcover/cloudcover/freesurface percentage, by beforehand counted number of pixels per heigth zone
      for (height in min_dem:max_dem) {
        pos_list <- height-min_dem+1
        sub_vector <- data_vector[dem_int==height]
        dem_stats$freesurface[pos_list] <- sum(sub_vector == 0, na.rm = TRUE)/number_of_pixels[pos_list]*100
        dem_stats$snowcover[pos_list] <- sum(sub_vector == 1, na.rm = TRUE)/number_of_pixels[pos_list]*100
        dem_stats$cloudcover[pos_list] <- sum(sub_vector == 2, na.rm = TRUE)/number_of_pixels[pos_list]*100
      }

      ##### store dem height zone statistics in txt-file in Satellite scene subfolder
      dem_stats_dir <- paste(data_store,file_list[n_elements],"/dem_stat.txt", sep = "")
      write.table(dem_stats,dem_stats_dir,sep=";", row.names = FALSE, col.names = TRUE)

      ############### end calculation of dem heigth zone statistics ###############
      #--------------------------------------------------------------------------------------------------
    }
    ################# end procedure ################################

    ##### merge overall statistics from both satellites
    if(num==1){
      all_stats <- stats
    }else{
      all_stats <- bind_rows(stats,all_stats)
    }
  }

  ##### save overall statistics as txt-file in project folder (store_directory)
  write.table(all_stats,file_name,sep=";", row.names = FALSE, col.names = TRUE)
  #--------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------
}
