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
  if(gdal_calc=='') stop('gdal_calc.py not found on system.')
  if(!file.exists(outpath)) {
    feature_dir <- base::tempfile("");
    base::dir.create(feature_dir);
    rand_fname <- base::tempfile("tmp", feature_dir);
    tmp_rast <- base::paste0(rand_fname, ".tif");
    message('Masking ', basename(outpath))
    call1 <- sprintf('python %s -A %s --outfile=%s --calc="A*(A>-9999)" --NoDataValue=-9999',  gdal_calc, inpath, tmp_rast)
    call2 <- sprintf('python %s --co="COMPRESS=LZW" -A %s -B %s --outfile=%s --calc="(A*(A>0))*(B*(B>0))" --NoDataValue=0',  gdal_calc, tmp_rast, mask, outpath)
    system(call1)
    system(call2)
    system(paste0('rm ',tmp_rast))
  }
}

#' @title Rotate raster from -180/180 to 0/360 degrees
#' @rdname gdal_180_to_360
#' @name gdal_180_to_360
#' @param inpath
#' @param outpath
###@param extent extent of raster, as vector in the following format xmin, xmax, ymin, ymax.

gdal_180_to_360 <- function(inpath,outpath){

  gdal_trans <- Sys.which('gdal_translate')
  gdal_merge <- Sys.which('gdal_merge.py')
  if(gdal_trans=='') stop('gdal_translate not found on system.')
  if(gdal_merge=='') stop('gdal_merge.py not found on system.')

  if(!file.exists(outpath)){
     r <- raster::raster(inpath)
     ext <- raster::extent(r)
     if (!all(c(ext[1],ext[2],ext[3],ext[4])%in%c(-180,180,-90,90)))stop('check the extent of the original rasters, should be xmin = -180, xmax = 180, ymin = -90, max = 90')
     feature_dir <- base::tempfile("");
     base::dir.create(feature_dir);
     rand_fname <- base::tempfile("tmp", feature_dir);
     tmp_r <- base::paste0(rand_fname, "R.tif");
     tmp_l <- base::paste0(rand_fname, "L.tif");
     call1 <- sprintf('gdal_translate -projwin -180 90 0 -90 -a_ullr 180 90 360 -90 %s %s', inpath, tmp_r)
     call2 <- sprintf('gdal_translate -a_srs EPSG:4326 -projwin 0 90 180 -90 -a_ullr -0 90 180 -90 %s %s', inpath, tmp_l)
     call3 <- sprintf('python %s -of GTiff -o %s %s %s', gdal_merge, outpath, tmp_l, tmp_r)#  $FILE3 $FILE2 $FILE1
     call4 <- paste0('rm ',tmp_r,' ',tmp_l)
     system(call1)
     system(call2)
     system(call3)
     system(call4)
  }
}

