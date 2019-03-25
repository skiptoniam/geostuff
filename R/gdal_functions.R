#' @title Crop function using gdal
#' @rdname gdal_crop
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

#' @title Mask function using gdal
#' @rdname gdal_mask
#' @name gdal_mask
#' @param inpath file path of the input raster
#' @param mask path to a mask file which is NA for cells to mask and 1 for cells to keep.
#' @param outpath file path of the output raster
#' @export

gdal_mask <- function(inpath, mask, outpath) {
  gdal_calc <- Sys.which('gdal_calc.py')
  feature_dir <- base::tempfile("");
  base::dir.create(feature_dir);
  rand_fname <- base::tempfile("tmp", feature_dir);
  tmp_rast <- base::paste0(rand_fname, ".tif");

  if(gdal_calc=='') stop('gdal_calc.py not found on system.')
  if(!file.exists(outpath)) {
    message('Masking ', basename(outpath))
    call1 <- sprintf('python %s -A %s --outfile=%s --calc="A*(A>-9999)" --NoDataValue=-9999',  gdal_calc, inpath, tmp_rast)
    call2 <- sprintf('python %s --co="COMPRESS=LZW" -A %s -B %s --outfile=%s --calc="(A*(A>0))*(B*(B>0))" --NoDataValue=0',  gdal_calc, tmp_rast, mask, outpath)
    system(call1)
    system(call2)
    system(paste0('rm ',tmp_rast))
  }
}
