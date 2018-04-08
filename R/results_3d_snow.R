# results_3d_snow function
#
# This function named 'results_3d_snow' recalculated snow extend tif files and plots them as red overlay in 3D on a RGB-image useing the DEM raster.
# Several point of views can be produced, but might be needed to be adjusted for different testsites.
# If a locations csv-file is added remarkable points in the scene can be plotted and named in the image for better recognition of scene.
#

#'results_3d_snow
#'@author Sebastian Buchelt (M.Sc. EAGLE - University of Wuerzburg)
#'@param store_directory Directory where all satellite scenes are stored in subfolders
#'@param dem_subdir subdirectory, where DEM files are stored
#'@param texture_file defines png-file, which should be used as background for 3d plot (must be exactly fitting to testsite scene & mirrored horizontally; e.g. with gimp)
#'@param views decide which views should be plotted; "north", "south" and "top" are possible
#'@param locations if a csv-file with locations within testsite exits, add it here so that their locations and names will be added to the image
#'\preformatted{ Example:
#'name;E;N;height;UTM
#'Innsbruck;681613;5236564;600;32
#'Sterzing;685358;5196180;950;32
#'Brenner;690605;5208832;1380;32}
#'@description his function named 'results_3d_snow' recalculated snow extend tif files and plots them as red overlay in 3D on a RGB-image useing the DEM raster.
#'Several point of views can be produced, but is needed to be adjusted for different testsites.
#'If a locations csv-file is added remarkable points in the scene can be plotted and named in the image for better recognition of scene.
#'
#'

results_3d_snow <- function(store_directory, subfolder_img=FALSE, dem_subdir, texture_file, locations=FALSE, views, color_snow, color_locs){

  ##### set subfolder directories
  satellite <-  c("Landsat/", "Sentinel/")    # Satellite subfolders
  satellite_shortcut <- c("_l", "_s")         # needed to store images from different satellites at same dates
  subfolder <-  "/testsite/"                  # subfolder within satellite subfolders, where testsite scene are stored
  subfolder_save <- "images/3D_plots/"        # subfolder, where resulting 3D plot images are stored
  dir.create(paste(store_directory,subfolder_save,sep = ""), showWarnings = FALSE)
  if(subfolder_img==FALSE){}else{subfolder_save <- paste(subfolder_save, subfolder_img, sep = "")}

  dem_dir <- paste(dem_subdir,"testsite/", sep = "")      # subfolder, where testsite DEM-files are stored
  dem_store <- paste(store_directory,dem_dir, sep = "")   # directory for dem file

  ##### create needed subdirectories
  dir.create(paste(store_directory,subfolder_save,sep = ""), showWarnings = FALSE)
  subf_small <- paste(views,"/", sep = "")
  north <- FALSE
  south <- FALSE
  top <- FALSE
  for (img in 1:length(views)) {
    dir.create(paste(store_directory,subfolder_save, subf_small[img],sep = ""), showWarnings = FALSE)
    if(str_detect(views[img], "north")){north <- TRUE}
    if(str_detect(views[img], "south")){south <- TRUE}
    if(str_detect(views[img], "top")){top <- TRUE}
  }

  #--------------------------------------------------------------------------------------------------
  ##### load 3D viewing matrix from predefined txt-files
  par_south <-read.table(paste(store_directory, "view_mat_south", sep = ""))
  par_south <- as.matrix(par_south)
  par_north <-read.table(paste(store_directory, "view_mat_north", sep = ""))
  par_north <- as.matrix(par_north)
  par_top <-read.table(paste(store_directory, "view_mat_top", sep = ""))
  par_top <- as.matrix(par_top)
  if(locations==FALSE){}else{
    locs <- read.csv(locations, header = TRUE, sep = ";")
  }
  numb_satellite <- length(satellite) # get number of satellites used for following for-loop

  open3d(windowRect=c(0,82,1920,1085)) # open window to plot 3D images
  #--------------------------------------------------------------------------------------------------
  ########## execute procedure for each satellite subfolder
  for (num in 1:numb_satellite){

    ##### get number and names of satellite scenes
    data_store <- paste(store_directory, satellite[num], sep = "")
    file_list <- list.files(path = data_store)
    file_numb <- length(file_list)

    #--------------------------------------------------------------------------------------------------
    ##### get date of acquisition for image filename from satellite scene foldername and load dem file with satellite specific resolution
    if(satellite[num]=="Sentinel/"){
      dates <- str_sub(file_list, start = 12, end =19)
      dem <- raster(paste(dem_store, "dem20.tif", sep = ""))
    }else{
      dates <- str_sub(file_list, start = 11, end =18)
      dem <- raster(paste(dem_store, "dem30.tif", sep = ""))
    }

    # convert date files for plotting
    mydates <- as.Date(dates, format = "%Y%m%d")
    mydates <- format(mydates, format="%d.%m.%Y")


    #--------------------------------------------------------------------------------------------------
    ##### calculate new dem with linear and exponential gradient for plotting to get background area more visible
    height <- raster::as.matrix(dem)
    dem2 <- dem
    for (row_x in 1:nrow(dem)) {
      resolution <- res(dem)
      dem2[row_x,] <- 1.0035^(row_x/nrow(dem)*2300)+(row_x/nrow(dem)*2300)      # calculate gradient for data visualisation
      #print(row_x)
    }
    dem_new <- dem + dem2         # add gradient to existing dem


    ###################################################################################################
    ############### start procedure ################################
    for (n_elements in 1:file_numb){

      # get file directory, where needed tif files are stored
      file_dir <- paste(data_store,file_list[n_elements],subfolder, sep = "")

      ##### start procedure, if Satellite scene is within the thresholds tested in calculate_stats.R and therefore used.txt contains TRUE
      txt_dir <- paste(data_store,file_list[n_elements],"/used.txt", sep = "")      # get txt-file, where is written, whether new_snow was calculated or not
      data_used <- read.table(txt_dir)                                              # read it
      if(data_used$V1==FALSE){                                                      # check, if new_snow should be calculated
      }else{

        ########## start procedure
        data <- raster(paste(file_dir, "new_snow.tif", sep = ""))     # load raster

        ##### calculate overall snowcover statistics
        data_vector <- as.vector(data)                        # convert data to vector (needed to calculate stats)
        pixel_number <- sum(data_vector >= 0, na.rm = TRUE)   # count total number of pixels
        snowcover <- as.character(round(sum(data_vector == 1, na.rm = TRUE)/pixel_number*100, digits = 1))  # calculate snowcover percentage

        #--------------------------------------------------------------------------------------------------
        ########## prepare data for 3D plot

        ##### convert all data to matrices
        infos <- raster::as.matrix(data)
        height <- raster::as.matrix(dem_new)
        height_old <- raster::as.matrix(dem)

        ##### transpose data (3D plot should be geocorrect depiction)
        infos <- t(infos)
        height <- t(height)
        height_old <- t(height_old)

        ##### mirror matrix (again: 3D plot should be geocorrect depiction)
        infos_s <- infos[c(nrow(infos):1),,drop = FALSE]
        height_s <- height[c(nrow(height):1),,drop = FALSE]
        height_old_s <- height_old[c(nrow(height_old):1),,drop = FALSE]

        ##### create matrix with color information for 3d plot
        colors_info_s <- as.character(infos_s)      # create new matrix
        colors_info_s[infos_s==0] <- "white"        # no snow will be not colored
        colors_info_s[infos_s==1] <- color_snow          # snow will be colored red


        #--------------------------------------------------------------------------------------------------
        if(locations==FALSE){}else{
          ########## get locations of point data
          col_pos <- colFromX(dem, locs[,2])    # get row, col positions of point data
          row_pos <- rowFromY(dem, locs[,3])    # get row, col positions of point data

          # adjust to transpose of data
          col_pos_t <- row_pos
          row_pos_t <- col_pos

          # adjust to mirroring data
          row_pos_t_s <- ncol(dem)-row_pos_t

          # adjst height to calculated gradient
          height_adjust <- locs$height + 1.0035^(row_pos/nrow(dem)*2300)+(row_pos/nrow(dem)*2300)

          ##### store calculated data of points in scene specific dataframe
          locs_scene <- cbind(locs, col_pos, row_pos, col_pos_t, row_pos_t, row_pos_t_s, height_adjust)
        }

        #--------------------------------------------------------------------------------------------------
        ############### start ploting 3D image ###############

        ########## plot image
        persp3d(x=1:ncol(dem), y=1:nrow(dem), height_s, texture= texture_file, textype="rgb",texmipmap=TRUE,    # plot background RGB, at gradient DEM
                aspect=c(ncol(dem),y=nrow(dem),z=400/resolution[1]*30),dev = 4, lit=FALSE,                      # adjust x,y,z scale
                color=colors_info_s, add = FALSE, axes=FALSE, xlab="", ylab="", zlab="")                        # overplot snowcover with color & switch of axes

        # insert locations with points and naming text
        if(locations==FALSE){}else{
          for (text_numb in 1:length(locs_scene$name)) {
            text3d(x=locs_scene$row_pos_t_s[text_numb], y = locs_scene$col_pos_t[text_numb], z = locs_scene$height_adjust[text_numb]+700, locs_scene$name[text_numb],  font=1, cex = 1.8, color=c(color_locs))
            points3d(x=locs_scene$row_pos_t_s[text_numb], y = locs_scene$col_pos_t[text_numb], z = locs_scene$height_adjust[text_numb]+50,  color = color_locs, size = 10.0)
          }
        }

        ########## plot image from the south
        if(south){
          # insert text with date and snow cover percentage
          text3d(x=as.integer(ncol(dem)/2), y = 0000, z = 3700, paste(mydates[n_elements],"    snow cover: ",snowcover, "%", sep = "" ) , font=5, cex = 3.0, color=c("black"))

          # plot 3D image from the south
          rgl.viewpoint( fov = 50, zoom = 0.33, userMatrix = par_south)

          # save view as png-file
          rgl.snapshot(filename = paste(store_directory, subfolder_save, "south/south_", dates[n_elements],
                                        satellite_shortcut[num], ".png", sep = ""), fmt = "png", top=FALSE)
        }
        #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        ########## plot 3D image from the north
        if(south){
          rgl.viewpoint( fov = 50, zoom = 0.33, userMatrix = par_north)
          # insert additional text with date and snow cover percentage for look from north
          text3d(x=as.integer(ncol(dem)/2), y = nrow(dem), z = 8800, paste(mydates[n_elements],"    snow cover: ",snowcover, "%", sep = "" ) , font=5, cex = 3.0, color=c("black"))
          # save view as png-file
          rgl.snapshot(filename = paste(store_directory, subfolder_save, "north/north_", dates[n_elements],
                                        satellite_shortcut[num], ".png", sep = ""), fmt = "png", top=FALSE)
        }

        #- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        ########## plot 3D image from the top
        if(top){
          persp3d(x=1:ncol(dem), y=1:nrow(dem), height_old_s, texture= texture_file, textype="rgb",texmipmap=TRUE,      # plot background RGB, at true DEM
                  aspect=c(ncol(dem),y=nrow(dem),z=200/resolution[1]*30),dev = 4, lit=FALSE,                            # adjust x,y,z scale
                  color=colors_info_s, add = FALSE, axes=FALSE, xlab="", ylab="", zlab="")                              # overplot snowcover with color & switch of axes

          # plot 3D image from the top
          rgl.viewpoint( fov = 50, zoom = 0.45, userMatrix = par_top)

          # insert locations with points and naming text
          if(locations==FALSE){}else{
            for (text_numb in 1:length(locs_scene$name)) {
              text3d(x=locs_scene$row_pos_t_s[text_numb], y = locs_scene$col_pos_t[text_numb], z = locs_scene$height[text_numb]+700, locs_scene$name[text_numb],  font=1, cex = 1.8, color=c(color_locs))
              points3d(x=locs_scene$row_pos_t_s[text_numb], y = locs_scene$col_pos_t[text_numb], z = locs_scene$height[text_numb]+50,  color = color_locs, size = 10.0)
            }
          }

          # save view as png-file
          rgl.snapshot(filename = paste(store_directory, subfolder_save, "top/top_", dates[n_elements],
                                        satellite_shortcut[num], ".png", sep = ""), fmt = "png", top=FALSE)
        }
      }
    }
  }
}
