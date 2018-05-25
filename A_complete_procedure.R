
#################################################################################################################
################################## load basic data ##############################################################
#################################################################################################################

##### get basics
# define directories (needs to be done!!!)
data_directory <- "/home/..."              # directory of zip & tar files
main_directory <- "/home/..."              # directory, where tif-files should be stored
store_directory <- paste(main_directory,"DATA1/", sep = "")     # optional, creates a subfolder, if you want to process data in partitions
all_used_subdirs <- c("DATA/", "DATA2/", "DATA3/")              # list of used subfolders to create a single gif file from partition processing results

wd <- "/home/..."
setwd(wd)

library(stringr)
library(inekpackage)
load_libs()

##### run on test basis or for all files
test <- FALSE


if(test){
  addon <- "test/"
}else{
  addon <- ""
}

# set directories
data_directory <- "/home/sebi/Studium/Master/Daten/Daten_April/"                                  # directory of zip & tar files
main_directory <- "/home/sebi/Studium/Master/Semester_1/R_project_snowcover/"            # optional
store_directory <- paste(main_directory,"DATA3/", sep = "")    # directory, where tif-files should be stored
all_used_subdirs <- c("DATA/", "DATA2/", "DATA3/")

dir.create(store_directory, showWarnings = FALSE)

#################################################################################################################
###################### load Sentinel and Landsat data from zip/tar.gz files #####################################
#################################################################################################################


########## load Sentinel data

# set Sentinel directories
s2_directory <- paste(data_directory, "Sentinel/", addon, sep = "")
store_s2 <- paste(store_directory, "Sentinel/", sep = "")
dir.create(store_s2, showWarnings = FALSE)
#setwd(store_s2)

# define needed bands and their resolution (check for available resolution beforehand!!!)
needed_bands <- c("B08", "B03","B11")
needed_res <- c("10", "10", "20")

# get the data from zip-file
get_Sentinel2_data_from_zip(s2_directory, store_s2, needed_bands, needed_res, add = FALSE)
# set working directory back to standard
#setwd(wd)

#--------------------------------------------------------------------------------------------------
########## load Landsat data

# set Landsat directories
l8_directory <- paste(data_directory, "Landsat/", addon, sep = "")
store_l8 <- paste(store_directory, "Landsat/", sep = "")
dir.create(store_l8, showWarnings = FALSE)
#setwd(store_l8)

# define needed bands
needed_bands <- c("band5", "band6", "band3")

# get the data from tar.gz-file
get_Landsat8_data_from_tar(l8_directory, store_l8, needed_bands, add = FALSE)
# set working directory back to standard
setwd(wd)



#################################################################################################################
################################ crop loaded data to testsite extent ############################################
#################################################################################################################

#--------------------------------------------------------------------------------------------------
########## define needed variables

# set directory, where tif-files are stored
setwd(store_directory)

# define a standard coordinate system, to which data in other coordinate systems will be projected to
proj_CRS <- CRS("+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# define coordinate extent of testarea for this standard coordinate system
ext <- extent(670500,709500,5190500,5260100)

# define, whether cropping should be done for Sentinel or Landsat scene
Sentinel = TRUE
Landsat = FALSE

############################################ crop Sentinel ######################################################
#--------------------------------------------------------------------------------------------------
########## set, which Satellite specific bands are used
# and add the needed resolution for possible raster coordinate system projections
if(Sentinel){
  used_bands <- c("B08", "B03", "B11")            # Sentinel band names (order must fit to 'name_output')
  projection_resolution=20
}else{
  if(Landsat){
    used_bands <- c("band5","band3", "band6")     # Landsat band names (order must fit to 'name_output')
    projection_resolution=30
  }
}

# define the names of the resulting testarea tif files (order must fit to bands used from satellite)
name_output <- c("NIR", "green", "SWIR")

#--------------------------------------------------------------------------------------------------
########## execute function 'get_testsite_Data' to create the needed tif-files with testsite extent
get_testsite_Data(store_directory, Sentinel, Landsat,ref_CRS=proj_CRS, ext = ext, res = projection_resolution,
                  needed_bands = used_bands, name_bands = name_output)

############################################ crop Landsat #######################################################

# define, whether cropping should be done for Sentinel or Landsat scene
Sentinel = FALSE
Landsat = TRUE

#--------------------------------------------------------------------------------------------------
########## set, which Satellite specific bands are used
# and add the needed resolution for possible raster coordinate system projections
if(Sentinel){
  used_bands <- c("B08", "B03", "B11")            # Sentinel band names (order must fit to 'name_output')
  projection_resolution=20
}else{
  if(Landsat){
    used_bands <- c("band5","band3", "band6")     # Landsat band names (order must fit to 'name_output')
    projection_resolution=30
  }
}

#--------------------------------------------------------------------------------------------------
########## execute function 'get_testsite_Data' to create the needed tif-files with testsite extent
get_testsite_Data(store_directory, Sentinel, Landsat,ref_CRS=proj_CRS, ext = ext, res = projection_resolution,
                  needed_bands = used_bands, name_bands = name_output)



#################################################################################################################
################## calclate snowextent for all scenes (thresholds can be defined here) ##########################
#################################################################################################################

# define, whether calculation should be done for Sentinel or Landsat scene or both
Sentinel = TRUE
Landsat = TRUE

#--------------------------------------------------------------------------------------------------
########## set thresholds:
# first:   NDSI minimum value for snowcover
# second:  minimum green reflectance for snowcover
# third:   minimum green reflectance for cloudcover
# fourth:  minimum SWIR reflectance for cloudcover}
thresholds = c(0.4,1500,2500,1700) # min NDSI; min green band for snow; min green band for cloud; min SWIR for cloud

#--------------------------------------------------------------------------------------------------
########## execute function 'calculate_snowextent' to create NDSI, snow/no-snow & snow/cloud/surface classification tif files
calculate_snowextent(store_directory,Sentinel,Landsat,thresholds)



#################################################################################################################
############################### merge and project needed DEMs to testsite extent ################################
#################################################################################################################


# get file directory, where needed dem files are stored
dem_sub_dir <- "DEM/ASTER/"
filenames <- c("ASTGTM2_N46E011_dem.tif","ASTGTM2_N47E011_dem.tif")
files <- paste(store_directory,dem_sub_dir,filenames, sep = "")

safe_dir <- paste(store_directory,dem_sub_dir,"testsite/", sep = "")  # subdirectory, where testsite dem is stored
dir.create(safe_dir, showWarnings = FALSE)



#--------------------------------------------------------------------------------------------------
############## start procedure #####################
dem1 <- raster(files[1])    # load raster of first dem
dem2 <- raster(files[2])    # load raster of second dem

dem_total <- merge(dem1, dem2)    # merge dems

########## create testsite dems, one with Landsat resolution 30m, one with Sentinel resolution 20m
# create testsite raster the dem should projected to
example_raster_20 <- raster(crs=proj_CRS, ext=ext,resolution=20)   # create testsite raster the dem should projected to
example_raster_30 <- raster(crs=proj_CRS, ext=ext,resolution=30)
# project raster
dem30 <- projectRaster(dem_total,example_raster_30,res = 30)
dem20 <- projectRaster(dem_total,example_raster_20,res = 20)

# save testsite raster as tif-file
writeRaster(dem30,paste(safe_dir,"dem30.tif", sep = ""), format="GTiff", overwrite=TRUE)
writeRaster(dem20,paste(safe_dir,"dem20.tif", sep = ""), format="GTiff", overwrite=TRUE)


################ end procedure #####################
#--------------------------------------------------------------------------------------------------


#################################################################################################################
########## plot images of NDSI, false color RGB, snow/no-snow & snow/cloud/freesurface classification ###########
#################################################################################################################


########## define which images should be produced
# snow:   snow/no-snow classification
# snow_cloud:  snow/cloud/snowfree-surface classification
# NDSI:   NDSI greyscale map
# RGB:  RGB false color image of testarea using band combination: SWIR, NIR, green}
# new_snow: snow/no-snow classification after calculating snow cover under clouds
images <- c("RGB","snow", "snow_cloud", "NDSI")

RGB_bands <- c("SWIR.tif", "NIR.tif", "green.tif")

#new_scenes <- FALSE # set true if you want only RGB-images to be plotted, where 'new_snow' extent was calculated

create_images_snow(store_directory, images, RGBimage_bands=RGB_bands)

#################################################################################################################
############ calculate overall snow/cloud-cover statistics & DEM height zone dependend statistics ###############
#################################################################################################################

heigth_zone <- 100      # defines the size (height difference) of the altitudinal zones for which the snowcover percentages are calculated
threshold_cloud <- 30   # thresholds for cloud cover to decide, whether the scene will be used in time series or not
library(dplyr)
calc_scene_snowstats(store_directory, dem_sub_dir, heigth_zone, threshold_cloud)

overall_stats <- read.csv(paste(store_directory, "statistics.txt", sep=""), header=TRUE, sep = ";")
ordered <- overall_stats[ order(overall_stats$dates, overall_stats$sat_col), ]
cloud_thres <- 30
snow_thres <- 8
snow_start <- 0
for (stat_pos in 1:length(ordered$dates)) {
  if(ordered$used[stat_pos]){
    snow_start <- ordered$snowcover[stat_pos]
  }else{
    if(ordered$cloudcover[stat_pos]<cloud_thres){
      #print(snow_start)
      #print(ordered$snowcover[stat_pos])
      if((ordered$snowcover[stat_pos]<(snow_start+snow_thres)) && (ordered$snowcover[stat_pos]>(snow_start-snow_thres))){
        #print(stat_pos)
        ordered$used[stat_pos] <- TRUE
        snow_start <- ordered$snowcover[stat_pos]
        file_dir_used <- paste(store_directory, ordered$sat_col[stat_pos],"/", sep = "")
        folder_list <- list.files(path = file_dir_used)
        Sat_folder <- str_subset(folder_list, as.character(ordered$dates[stat_pos]))   # get only the zip files (needed, if folder contains other files aswell)
        print(Sat_folder)
        txt_dir <- paste(file_dir_used,Sat_folder,"/used.txt", sep = "")
        write(TRUE, file = txt_dir)
      }
    }
  }

}

#################################################################################################################
## recalculate snow extent to exclude low probable snow pixels and include high probable snow under cloudcover ## (dem height probabilities are used)
#################################################################################################################

threshold_snow1 <- 10       # snow probability, where snow pixels will be excluded from the class snow (done with height zone probabilities)
threshold_cloud2 <- 60      # snow probability, where cloud pixels will be added to the class snow (done with height zone probabilities)

recalc_snowextent_scene(store_directory, dem_sub_dir, heigth_zone, threshold_snow1, threshold_cloud2)

#################################################################################################################
################# plot images of recalculated new snowcover and the matching false color RGBs ###################
#################################################################################################################

########## define which images should be produced
# snow:   snow/no-snow classification
# snow_cloud:  snow/cloud/snowfree-surface classification
# NDSI:   NDSI greyscale map
# RGB:  RGB false color image of testarea using band combination: SWIR, NIR, green}
# new_snow: snow/no-snow classification after calculating snow cover under clouds
images <- c("RGB","new_snow")

new_scenes <- TRUE # set true if you want only RGB-images to be plotted, where 'new_snow' extent was calculated

create_images_snow(store_directory, images, RGBimage_bands=RGB_bands, new_scenes)

#################################################################################################################
########################## create 3D plot images and save them as png files #####################################
#################################################################################################################


dem_sub_dir <- "DEM/ASTER/"
background <- paste(store_directory,"true_RGB_s2.png", sep = "")    # define background for 3D plots
location_file <- paste(store_directory, "locations.csv", sep = "")
view_directions <- c("north", "south", "top")
results_3d_snow(store_directory,subfolder_img="TRUE_RGB/", dem_sub_dir, background, locations=location_file, views=view_directions, color_snow = "deepskyblue", color_locs="red")

background <- paste(store_directory,"DEM_s.png", sep = "")    # define background for 3D plots
results_3d_snow(store_directory,subfolder_img="DEM/", dem_sub_dir, background, locations=location_file, views=view_directions, color_snow = "deepskyblue", color_locs="red")

background <- paste(store_directory,"scene_RGB_s.png", sep = "")    # define background for 3D plots
results_3d_snow(store_directory,subfolder_img="", dem_sub_dir, background, locations=location_file, views=view_directions, color_snow = "red", color_locs="cyan")

#################################################################################################################
################# create gifs from 3D images and for the matching false color RGBs ##############################
#################################################################################################################

library(animation)

#--------------------------------------------------------------------------------------------------
########## define needed variables (directories)
store_directories <- paste(main_directory, all_used_subdirs, sep = "")
plot_directories <- paste(store_directories, "images/", sep = "")
image_subdir <- c(paste("3D_plots/", c("top/", "north/", "south/", "DEM/top/", "DEM/north/", "DEM/south/", "TRUE_RGB/top/", "TRUE_RGB/north/", "TRUE_RGB/south/"), sep =""), "RGB_new/")            # subfolders of png-files

save_dir <- "gifs/"                                                                             # subfolder for resulting gif-file
dir.create(paste(main_directory,save_dir,sep = ""), showWarnings = FALSE)

# define names of resulting gifs
name_gif <- c("3D_top", "3D_north", "3D_south", "DEM_top", "DEM_north", "DEM_south", "RGB_top", "RGB_north", "RGB_south","sat_RGB")

name_gif <- paste(main_directory, save_dir, name_gif, sep = "")

# set wd to output folder
setwd(paste(main_directory, save_dir,sep = ""))

#--------------------------------------------------------------------------------------------------
########## execute procedure for each image type subfolder  ######################################
for (image_type in 1:length(image_subdir)) {
  img_files <- c()
  for (nfolders in 1:length(plot_directories)) {
    img_list <- list.files(path = paste(plot_directories[nfolders], image_subdir[image_type], sep = ""))     # get list with all image filenames
    img_files <- c(img_files, paste(plot_directories[nfolders], image_subdir[image_type],img_list, sep = ""))              # create complete directory to make it readable  for im.convert
  }
  ani.options('interval=1')
  im.convert(img_files, output = paste(name_gif[image_type], ".gif", sep = ""))                                     # create gif
  im.convert(img_files, output = paste(name_gif[image_type], "_small.gif", sep = ""), extra.opts = "-resize 40%")   # create small size gifs
}
#--------------------------------------------------------------------------------------------------

# reset working directory
setwd(wd)

#################################################################################################################
########################################## end overall procedure ################################################
#################################################################################################################
