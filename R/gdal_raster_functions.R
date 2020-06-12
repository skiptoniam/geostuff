#' @title Crop function using gdal
#' @rdname gdalCrop
#' @name gdalCrop
#' @param inpath file path of the inpath raster
#' @param outpath file path of the output raster
#' @param extent extent for raster to be cropped to
#' @param resolution resolution of output raster (usually based on the inpath raster)
#' @param return_raster if TRUE raster will be returned from function call.
#' @export
#'
#' @importFrom raster raster

gdalCrop <- function(inpath, outpath, extent=NULL, resolution=NULL, return = TRUE){

  tmpTif1 <- tempfile(fileext=".tif")
  tmpTif2 <- tempfile(fileext=".tif")

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

  if (isTRUE(return)) {
    outraster <- raster::raster(outpath)
    return(outraster)
  }
}

#' @title Faster rasterisation of shapefile to raster using gdal
#' @rdname gdalRasterise
#' @name gdalRasterise
#' @param shp local shapefile or file path of the inpath raster
#' @param rast local shapefile or file path of the output raster
#' @param variable what variable to convert to a raster
#' @export

#' @importFrom raster raster xres yres projection xmin ymin xmax ymax values extent nrow ncol
#' @importFrom rgdal writeOGR

gdalRasterise <- function(shp, rast, variable=NULL, bigtif=FALSE) {

  tmpTif <- tempfile(fileext=".tif")

  if(is.character(shp)){
   tmpShp <- shp
  } else {
   tmpShp <- tempfile(fileext=".shp")
   rgdal::writeOGR(shp,dirname(tmpShp),gsub("[.]shp","", basename(tmpShp)),driver="ESRI Shapefile",overwrite=TRUE)
  }

  # use gdalPolygonize
  if( is.null(variable) ) {
    system(
      sprintf( "gdal_rasterize --config GDAL_CACHEMAX 2000 -burn 1 -at -a_nodata -9999 -a_srs '%s' -tr %f %f -te %f %f %f %f '%s' '%s'",
               projection(rast), xres(rast), yres(rast), xmin(rast), ymin(rast), xmax(rast), ymax(rast),
               tmpShp, tmpTif)
    )
  } else {
    system(
      sprintf( "gdal_rasterize --config GDAL_CACHEMAX 2000 -a '%s' -at -a_nodata -9999 -a_srs '%s' -tr %f %f -te %f %f %f %f '%s' '%s'",
               variable, projection(rast), xres(rast), yres(rast), xmin(rast), ymin(rast), xmax(rast), ymax(rast),
               tmpShp, tmpTif)
    )
  }

  if(bigtif)return(print(tmpTif))

  # create a new raster object
  r.out <- raster(extent(rast), ncols=ncol(rast), nrows=nrow(rast), crs=projection(rast))
  values(r.out) <- values(raster(tmpTif))

  # free up the data
  unlink(tmpTif)
  if(!is.character(shp)) unlink(tmpShp)

  return(r.out)
}

#' @title Faster reprojectiom of rasters using gdal
#' @rdname gdalProject
#' @name gdalProject
#' @param inraster local or file path of the inpath raster
#' @param outdir local shapefile or file path of the output raster
#' @param xres resolution of cells
#' @export

gdalProject <- function(inraster, outdir=NULL, xres, yres=xres, s_srs, t_srs,
                        resampling, extent, of='GTiff', extension='tif',
                        ot='Float32',returnRaster = TRUE) {

  if(is.raster(inraster)){
    tmpTifIn <- tempfile(fileext=".tif")
    raster::writeRaster(x = inraster, filename = tmpTifIn, format = "GTiff", overwrite=TRUE)
  } else {
    tmpTifIn <- inraster
  }

  outfile <- tmpTifIn
  extension(outfile) <- sub('^\\.+', '.', paste0('.', extension))
  if(is.null(outdir)) outfile <- file.path(normalizePath(tempdir()), basename(outfile))
  else outfile <- file.path(normalizePath(outdir), basename(outfile))

  system(sprintf('gdalwarp -ot %s -s_srs "%s" -t_srs "%s" -r %s -multi %s -tr %s -dstnodata -9999 -of %s "%s" "%s"',
                 ot, s_srs, t_srs, resampling,
                 if(!missing(extent)) paste(c('-te', extent), collapse=' ') else '',
                 paste(xres, yres), of, tmpTifIn, outfile))
  # system(sprintf('gdalinfo -stats %s', outfile), show.output.on.console=FALSE)

  unlink(tmpTifIn)

  if (isTRUE(returnRaster)) {
    outraster <- raster::raster(outfile)
    return(outraster)
  }

}

#' @title Clip raster with a shape file
#' @rdname gdalClipWithShape
#' @name gdalClipWithShape
#' @param inrast file path or raster object
#' @param inshp file path or shape object
#' @param outrast NULL or file path. Default NULL and produces a tmp raster file.
#' @param field name of specific field in shapefile to clip raster with. Default is NULL
#' @param cropToShape logical Should the raster be cropped to the extent of the shapefile. Default is FALSE
#' @param returnRaster logical Should the raster be returned to R environment? Default is TRUE
#' @export
#' @importFrom raster raster

gdalClipWithShape <- function(inrast, inshp, outrast=NULL, field=NULL,
                              cropToShape=FALSE, returnRaster=TRUE){

  if(!is.character(inshp)){
    tmpShpIn <- tempfile(fileext="in.shp")
    rgdal::writeOGR(inshp,dirname(tmpShpIn),gsub("[.]shp","", basename(tmpShpIn)),driver="ESRI Shapefile",overwrite=TRUE)
  } else {
    tmpShpIn <- inshp
  }
  if(!is.character(inrast)){
    tmpRastIn <- tempfile(fileext="in.tif")
  } else {
    tmpRastIn <- inrast
  }
  if(!is.character(outrast)|is.null(outrast)){
    tmpRastOut <- tempfile(fileext="out.tif")
  } else {
    tmpRastOut <- outrast
  }

  if(cropToShape){
    if(is.null(field)) x <- sprintf("gdalwarp -of GTiff -cutline '%s' -crop_to_cutline '%s' '%s'",
                                    tmpShpIn, tmpRastIn, tmpRastOut)
    else x <- sprintf("gdalwarp -of GTiff -cutline '%s' -cl %s -crop_to_cutline '%s' '%s'",
                      tmpShpIn, field, tmpRastIn, tmpRastOut)
  } else {
    if(is.null(field)) x <- sprintf("gdalwarp -of GTiff -cutline '%s' '%s' '%s'",
                                    tmpShpIn, tmpRastIn, tmpRastOut)
    else x <- sprintf("gdalwarp -of GTiff -cutline '%s' -cl %s '%s' '%s'",
                      tmpShpIn, field, tmpRastIn, tmpRastOut)
  }

  system(x)
  retrast <- NULL
  if(returnRaster) retrast <- raster::raster(tmpRastOut)

  # free up the data
  if(!is.character(inrast)) unlink(tmpRastIn)
  if(!is.character(inshp)) unlink(tmpShpIn)
  # if(!is.character(outrast)|is.null(outrast)) unlink(tmpRastOut)

  return(retrast)

}


#' @title Mask function using gdal
#' @rdname gdalMask
#' @name gdalMask
#' @param inpath file path of the inpath raster
#' @param mask path to a mask file which is NA for cells to mask and 1 for cells to keep.
#' @param outpath file path of the output raster
#' @param return_raster if TRUE raster will be returned from function call.
#' @export
#'
#' @importFrom raster raster


gdalMask <- function(inpath, mask, outpath,return_raster = FALSE, ...) {

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
    unlink(tmp_rast)
  }
  if (isTRUE(return_raster)) {
    outraster <- raster::raster(outpath)
    return(outraster)
  }
}

#' @title Rotate raster from -180/180 to 0/360 degrees
#' @rdname gdal180to360
#' @name gdal180to360
#' @param inpath file path of inpath file to change.
#' @param outpath file path of output file to generate.
#' @param return_raster if TRUE raster will be returned from function call.
#' @export

#' @importFrom raster raster

gdal180to360 <- function(inpath,outpath,return_raster = FALSE){

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
#' @param inraster file path of inpath file to change.
#' @param outpath file path of output file to generate.
#' @param resolution desired new resolution for x and y.
#' @param method resample method, default is nearest neighbour which is fast but dodgy.
#' @param bigtif if TRUE deal with a big geotiff slightly differently.
#' @param return_raster if TRUE raster will be returned from function call.
#' @export

#' @importFrom raster raster

# gdalResample <- function(inpath, outpath, resolution, method = 'near', bigtif = FALSE, return_raster = FALSE){
gdalResample <- function(inraster, outpath = NULL, resampleRaster = NULL,
                         resolution=NULL, method = 'bilinear', bigtif = FALSE,
                         returnRaster = TRUE){

  tmpTifIn <- tempfile(fileext=".tif")
  if(is.null(outpath)){
    tmpTifOut <- tempfile(fileext=".tif")
  } else {
    tmpTifOut <- outpath
  }
  if(!is.null(resampleRaster))  resolution <- raster::res(resampleRaster)
  if(length(resolution)!=2)stop("Target resolution need for x and y.")

  if (!method %in% c("near", "bilinear", "cubic", "cubicspline",
                     "lanczos", "average", "mode", "max", "min", "med", "q1",
                     "q3")) {
    stop("Resampling method not available.")
  }

  raster::writeRaster(x = inraster, filename = tmpTifIn, format = "GTiff", overwrite=TRUE)

  resample_command <- paste0("gdalwarp -multi -of vrt -tr ",
                             " ", resolution[1], " ", resolution[2], " -r ",
                             method," ",tmpTifIn, " ", gsub(tools::file_ext(tmpTifOut), "vrt", tmpTifOut))
  if (isTRUE(bigtif)) {
    VRT2TIF <- paste0("gdal_translate -co compress=LZW -co BIGTIFF=YES",
                      " ", gsub(tools::file_ext(tmpTifOut), "vrt", tmpTifOut),
                      " ", gsub(tools::file_ext(tmpTifOut), "tif", tmpTifOut))
  }
  else {
    VRT2TIF <- paste0("gdal_translate -co compress=LZW",
                      " ", gsub(tools::file_ext(tmpTifOut), "vrt", tmpTifOut),
                      " ", gsub(tools::file_ext(tmpTifOut), "tif", tmpTifOut))
  }
  system(resample_command)
  system(VRT2TIF)
  unlink(gsub(tools::file_ext(tmpTifOut), "vrt", tmpTifOut))
  if (isTRUE(returnRaster)) {
    outraster <- raster::raster(tmpTifOut)
    return(outraster)
  }
}

#' @title Calculate distance raster using gdal
#' @rdname gdalDistance
#' @name gdalDistance
#' @param inpath file path of inpath file to change.
#' @param outpath file path of output file to generate.
#' @param target numeric vaules to calculate distance from default is zero.
#' @param maxdist The maximum distance to be generated. maxdist is in pixels.
#' @param return_raster if TRUE raster will be returned from function call.
#' @export

gdalDistance <- function(inpath, outpath, target=NULL, maxdist=NULL, return_raster=FALSE){
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

gdal_stitch_titles <- function(outpath, tiles_path, return_raster = TRUE, large_tif = TRUE){
  buildVRT <- paste0("gdalbuildvrt", " ", gsub(pkgmaker::file_extension(outpath),
                                               "vrt", outpath), " ", tiles_path)
  if (large_tif == TRUE) {
    VRT2TIF <- paste0("gdal_translate -co compress=LZW -co BIGTIFF=YES",
                      " ", gsub(pkgmaker::file_extension(outpath),
                                "vrt", outpath), " ", gsub(pkgmaker::file_extension(outpath),
                                                               "tif", outpath))
  }
  else {
    VRT2TIF <- paste0("gdal_translate -co compress=LZW",
                      " ", gsub(pkgmaker::file_extension(outpath),
                                "vrt", outpath), " ", gsub(pkgmaker::file_extension(outpath),
                                                               "tif", outpath))
  }
  system(buildVRT)
  system(VRT2TIF)
  unlink(gsub(pkgmaker::file_extension(outpath), "vrt",
              outpath))
  if (return_raster) {
    out <- raster::raster(outpath)
    return(out)
  }
}


#' @export
is.raster <- function(x)
{
  return((class(x)=="RasterLayer" || class(x)=="RasterBrick" || class(x)=="RasterStack"))
}

