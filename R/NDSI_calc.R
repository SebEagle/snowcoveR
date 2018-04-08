NDSI_calc<- function(green, SWIR){
  NDSI <- ((green-SWIR)/(green+SWIR))
  return(NDSI)
}
