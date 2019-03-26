#' @title Crop function using gdal
#' @rdname gdal_crop
#' @name gdal_crop
#' @param inpath file path of the input raster
#' @param outpath file path of the output raster
#' @param extent extent for raster to be cropped to
#' @param resolution resolution of output raster (usually based on the input raster)
#' @param return_raster if TRUE raster will be returned from function call.
#' @export
#'
#' @importFrom raster raster

gdal_crop <- function(inpath, outpath, extent, resolution,return_raster = FALSE){
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
  if (isTRUE(return_raster)) {
    outraster <- raster::raster(outpath)
    return(outraster)
  }
}

#' @title Mask function using gdal
#' @rdname gdal_mask
#' @name gdal_mask
#' @param inpath file path of the input raster
#' @param mask path to a mask file which is NA for cells to mask and 1 for cells to keep.
#' @param outpath file path of the output raster
#' @param return_raster if TRUE raster will be returned from function call.
#' @export
#'
#' @importFrom raster raster


gdal_mask <- function(inpath, mask, outpath,return_raster = FALSE) {

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
  if (isTRUE(return_raster)) {
    outraster <- raster::raster(outpath)
    return(outraster)
  }
}

#' @title Rotate raster from -180/180 to 0/360 degrees
#' @rdname gdal_180_to_360
#' @name gdal_180_to_360
#' @param inpath file path of input file to change.
#' @param outpath file path of output file to generate.
#' @param return_raster if TRUE raster will be returned from function call.
#' @export

#' @importFrom raster raster

gdal_180_to_360 <- function(inpath,outpath,return_raster = FALSE){

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
  if (isTRUE(return_raster)) {
    outraster <- raster::raster(outpath)
    return(outraster)
  }
}

#' @title Resample raster to different resolution
#' @rdname gdal_resample
#' @name gdal_resample
#' @param inpath file path of input file to change.
#' @param outpath file path of output file to generate.
#' @param resolution desired new resolution for x and y.
#' @param method resample method, default is nearest neighbour which is fast but dodgy.
#' @param bigtif if TRUE deal with a big geotiff slightly differently.
#' @param return_raster if TRUE raster will be returned from function call.
#' @export

#' @importFrom raster raster

gdal_resample <- function (inpath, outpath, resolution, method = 'near',
                           bigtif = FALSE, return_raster = FALSE){

  if(lenght(resolution)!=2)stop("Target resolution need for x and y.")

  if (!method %in% c("near", "bilinear", "cubic", "cubicspline",
                     "lanczos", "average", "mode", "max", "min", "med", "q1",
                     "q3")) {
    stop("Resampling method not available.")
  }

  resample_command <- paste0("gdalwarp -multi -of vrt -tr ",
                             " ", resolution, " ", resolution, " -r ",
                             method, " ", inpath, " ", gsub(tools::file_ext(outpath),
                                                            "vrt", outpath))
  if (isTRUE(bigtif)) {
    VRT2TIF <- paste0("gdal_translate -co compress=LZW -co BIGTIFF=YES",
                      " ", gsub(tools::file_ext(outpath), "vrt", outpath),
                      " ", gsub(tools::file_ext(outpath), "tif", outpath))
  }
  else {
    VRT2TIF <- paste0("gdal_translate -co compress=LZW",
                      " ", gsub(tools::file_ext(outpath), "vrt", outpath),
                      " ", gsub(tools::file_ext(outpath), "tif", outpath))
  }
  system(resample_command)
  system(VRT2TIF)
  unlink(gsub(tools::file_ext(outpath), "vrt", outpath))
  if (isTRUE(return_raster)) {
    outraster <- raster::raster(outpath)
    return(outraster)
  }
}

#' @title Calculate distance raster using gdal
#' @rdname gdal_distance
#' @name gdal_distance
#' @param inpath file path of input file to change.
#' @param outpath file path of output file to generate.
#' @param target numeric vaules to calculate distance from default is zero.
#' @param maxdist The maximum distance to be generated. maxdist is in pixels.
#' @param return_raster if TRUE raster will be returned from function call.
#' @export

gdal_distance <- function(inpath, outpath, target=NULL, maxdist=NULL, return_raster=FALSE){
          gdal_prox <- Sys.which('gdal_proximity.py')
          if(gdal_prox=='') stop('gdal_proximity.py not found on system.')

          if(!file.exists(outpath)){
            call1 <- sprintf('python %s %s %s -of GTiff  ', gdal_prox, inpath, outpath)
            system(call1)
          }
          if (isTRUE(return_raster)) {
            outraster <- raster::raster(outpath)
            return(outraster)
          }
}

