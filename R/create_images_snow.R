# create_images_snow function
#
# This function named 'create_images_snow' loads three tif files for RGB image or NDSI tif file or classification tif files to plot them.
# The plots will be stored as png files in the subfolder images separated in several subfolders.
#

#'create_images_snow
#'@author Sebastian Buchelt (M.Sc. EAGLE - University of Wuerzburg)
#'@param store_directory Directory where all satellite scenes are stored in subfolders
#'@param images vector with strings that defines which images are calculated
#'\preformatted{ Possible input strings:
#'     - 'snow':   plots snow/no-snow classification extent map
#'     - 'snow_cloud': plots snow/cloud/freesurface classification map
#'     - 'new_snow': plots recalculated snow extend from new_snow.tif file
#'              (produced only for timeseries used data; look at function 'calc_scene_snowstats')
#'     - 'RGB': plot RGB false or true color from the three bands in variable 'RGBimage_bands'
#'     - 'NDSI': plots a greyscale image of NDSI}
#'@param RGBimage_bands vector containing 3 strings with bands, which should be useed for RGB imaging
#'@param new_scenes set true if you want only RGB-images to be plotted, where 'new_snow' extent was calculated. A new subfolder will be created.
#'@description This function named 'create_images_snow' loads three tif files for RGB image or NDSI tif file or
#'classification tif files to plot them.
# The plots will be stored as png files in the subfolder images separated in several subfolders.
#'
#'

create_images_snow <- function(store_directory, images, RGBimage_bands=FALSE, new_scenes=FALSE){


  # set subfolder directories
  satellite <-  c("Sentinel/", "Landsat/")    # Satellite subfolders
  satellite_shortcut <- c("_s", "_l")         # needed to store images from different satellites at same dates
  subfolder <-  "/testsite/"                  # subfolder within satellite subfolders, where testsite scene are stored
  subfolder_save <- "images/"                 # subfolder, where resulting images should be stored
  dir.create(paste(store_directory,subfolder_save,sep = ""), showWarnings = FALSE)
  for (img in 1:length(images)) {
    dir.create(paste(store_directory,subfolder_save,images[img],"/",sep = ""), showWarnings = FALSE)
  }
  if(new_scenes){dir.create(paste(store_directory,subfolder_save,"RGB_new/",sep = ""), showWarnings = FALSE)}

  #--------------------------------------------------------------------------------------------------


  # get number of image types, that should be created for following for-loop
  numb_imgtype <- length(images)
  numb_satellite <- length(satellite) # get number of satellites used for following for-loop

  #--------------------------------------------------------------------------------------------------
  ########## execute procedure for each satellite subfolder
  for (num in 1:numb_satellite){

    ##### get number and names of satellite scenes
    data_store <- paste(store_directory, satellite[num], sep = "")
    file_list <- list.files(path = data_store)
    file_numb <- length(file_list)

    ##### get date of acquisition for image filename from satellite scene foldername
    if(satellite[num]=="Sentinel/"){
      dates <- str_sub(file_list, start = 12, end =19)
    }else{
      dates <- str_sub(file_list, start = 11, end =18)
    }

    ###################################################################################################
    ############### start procedure ################################
    for (n_elements in 1:file_numb){

      # get file directory, where needed tif files are stored
      file_dir <- paste(data_store,file_list[n_elements],subfolder, sep = "")

      ############### execute procedure for all images needed
      for (imgtype in 1:numb_imgtype) {

        # get, which imagetype should be created
        img <- images[imgtype]

        # define name and directory of imagefile to be saved
        if(new_scenes & img=="RGB"){
          save_dir <- paste(store_directory,subfolder_save,img,"_new/",img,"_",dates[n_elements],satellite_shortcut[num],".png",sep = "")     # if new_scenes=TRUE store RGB in new subfolder
        }else{save_dir <- paste(store_directory,subfolder_save,img,"/",img,"_",dates[n_elements],satellite_shortcut[num],".png",sep = "")}
        # -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
        ########## procedure for false-color RGB  (SWIR-NIR-green)
        if(img=="RGB"){

          # get filenames of needed tif files
          file_name <- paste(file_dir, RGBimage_bands , sep = "")

          # load rasters
          red_color <- raster(file_name[1])          # load red_color raster
          green_color <- raster(file_name[2])           # load green_color raster
          blue_color <- raster(file_name[3])         # load blue_color raster

          # get resolutions of loaded rasters
          res_blue_color <- res(blue_color)
          res_red_color <- res(red_color)
          res_green_color <- res(green_color)

          ##### check resolutions of rasters and aggregate to lower resolution of red_color, if needed
          if(res_blue_color[1]!=res_red_color[1]){
            ratio <- res_red_color/res_blue_color
            if(ratio[1]>1){
              blue_colortoaggregate <- blue_color
              blue_color <- aggregate(blue_colortoaggregate, fact=ratio, fun=mean)
              res_blue_color <- res(blue_color)
            }else{
              red_colortoaggregate <- red_color
              red_color <- aggregate(red_colortoaggregate, fact=1/ratio, fun=mean)
              res_red_color <- res(red_color)
            }
          }
          if(res_green_color[1]!=res_red_color[1]){
            ratio <- res_red_color/res_green_color
            if(ratio[1]>1){
              green_colortoaggregate <- green_color
              green_color <- aggregate(green_colortoaggregate, fact=ratio, fun=mean)
            }else{
              red_colortoaggregate <- red_color
              red_color <- aggregate(red_colortoaggregate, fact=1/ratio, fun=mean)
            }
          }

          ##### create and save false-color RGB
          scene_RGB <- stack(red_color,green_color,blue_color)                    # create raster stack

          ##### if new_scenes=TRUE, plot only images where new_snow extent was calculated, else plot and save all
          if(new_scenes){
            txt_dir <- paste(data_store,file_list[n_elements],"/used.txt", sep = "")      # get txt-file, where is written, whether new_snow was calculated or not
            data_used <- read.table(txt_dir)                                              # read it
            if(data_used$V1){                                                             # check, if new_snow exists
              png(filename = save_dir,width = 1000, height = 1540)  # define png-file of image
              plotRGB(scene_RGB, stretch="lin")                     # plot RGB
              dev.off()                                             # save RGB
            }
          }else{
            png(filename = save_dir,width = 1000, height = 1540)  # define png-file of image
            plotRGB(scene_RGB, stretch="lin")                     # plot RGB
            dev.off()                                             # save RGB
          }

          ########## end RGB procedure
          # -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

          ########## other images procedures
        }else{
          if(img=="new_snow"){
            txt_dir <- paste(data_store,file_list[n_elements],"/used.txt", sep = "")      # get txt-file, where is written, whether new_snow was calculated or not
            data_used <- read.table(txt_dir)                                              # read it
            if(data_used$V1){                                                             # check, if new_snow exists
              # get filenames of needed tif files
              file_name <- paste(file_dir,img,".tif", sep = "")
              rasterdata <- raster(file_name)                       # load raster data
              png(filename = save_dir,width = 1000, height = 1540)                    # define png-file of image
              plot(rasterdata)                                                        # plot classification
              dev.off()                                                               # save image
            }
          }else{

            # get filenames of needed tif files
            file_name <- paste(file_dir,img,".tif", sep = "")
            rasterdata <- raster(file_name)                       # load raster data

            ##### if imagetype = NDSI, create a grey scale image of NDSI value
            if(img=="NDSI"){
              ggR(rasterdata, stretch = "lin")                                        # plot greyscale image
              ggsave(filename = save_dir,width = 133, height = 200, units = "mm", device = "png")    # save image

              ##### otherwise plot classification result (either snow/no-snow or snow/cloud/free-surface classification)
            }else{
              png(filename = save_dir,width = 1000, height = 1540)                    # define png-file of image
              plot(rasterdata)                                                        # plot classification
              dev.off()                                                               # save image
            }
          }
        }
        ########## end other images procedures
      }
      ############### end execution for all images needed
    }
    ################# end procedure ################################
    ###################################################################################################
  }
  ########## end execution for satellite folder(s)
  #--------------------------------------------------------------------------------------------------
}
