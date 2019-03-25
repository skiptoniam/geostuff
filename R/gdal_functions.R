#' @title gdal functions in R
#' @rdname gdal_functions
#' @name gdal_crop
#' @param inpath file path of the input raster
#' @param outpath file path of the output raster
#' @param extent extent for raster to be cropped to
#' @param resolution resolution of output raster (usually based on the input raster)
#' @export

gdal_crop <- function(inpath, outpath, extent, resolution){
  gdalwarp <- Sys.which('gdalwarp')
  if(gdalwarp=='') stop('gdalwarp not found on system.')
  if(!file.exists(outpath)) {
    print(extent)
    if(length(resolution)!=2)stop('need x and y resolution')
    print(resolution)
    message('Cropping ', basename(outpath))
    system(
      sprintf('gdalwarp -co "COMPRESS=LZW" -of gtiff -te %f %f %f %f -tr %f %f %s %s',
              extent[1],extent[3],extent[2],extent[4], resolution[1], resolution[2], inpath, outpath))
  }
}
