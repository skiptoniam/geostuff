#' @title Rotate raster from -180/180 to 0/360 degrees
#' @rdname gdal180to360
#' @name gdal180to360
#' @param inpath file path of inpath file to change.
#' @param outpath file path of output file to generate.
#' @param return.raster if TRUE raster will be returned from function call.
#' @export

#' @importFrom raster raster

gdal180to360 <- function(inpath,outpath,return.raster = FALSE){

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
    call1 <- sprintf('gdal_translate -projwin -180 90 0 -90 -a_ullr 180 90 360 -90 %s %s compress=LZW', inpath, tmp_r)
    call2 <- sprintf('gdal_translate -a_srs EPSG:4326 -projwin 0 90 180 -90 -a_ullr -0 90 180 -90 %s %s compress=LZW', inpath, tmp_l)
    call3 <- sprintf('%s -of GTiff -o %s %s %s compress=LZW', gdal_merge, outpath, tmp_l, tmp_r)#  $FILE3 $FILE2 $FILE1
    call4 <- paste0('rm ',tmp_r,' ',tmp_l)
    system(call1)
    system(call2)
    system(call3)
    system(call4)
  }
  if (isTRUE(return.raster)) {
    outraster <- raster::raster(outpath)
    return(outraster)
  }
}

#' @title Generic function for using gdal_calc
#' @rdname gdalCalc
#' @name gdalCalc
#' @param inpath file path of inpath file to change.
#' @param outpath file path of output file to generate.
#' @param calc_fun A text function that can be used from numpy, some examples include: "sum", "average", "std", "max", "min"
#' @param return.raster if TRUE raster will be returned from function call.
#' @export
#'

gdalCalc <- function(inpath, outpath, calc_fun="sum", numpy=TRUE,
                     na.value=NULL, overwrite=TRUE, return.raster=TRUE,
                     quiet=FALSE){

  gdal_calc <- Sys.which('gdal_calc.py')
  if(gdal_calc=='') stop('gdal_calc.py not found on system. Make sure gdal and python installed and visiable to path.')
  if(overwrite)overwrite <- "--overwrite"
  else overwrite <- ""

  if(quiet)quiet.call <- "--quiet"
  else quiet.call <- ""
  # if(is.null(na.value)) na.call <- ""
  # else na.call <- paste0("--NoDataValue=",na.value)

  if (!is.null(calc_fun)){
    calc_fun_in <- calc_fun
    if(!is.character(calc_fun_in)) stop("Must be a character such as 'sum', see numpy for avaliable functions")
  } else {
    stop("You must provide a calc_fun function\n")
  }

  nbands <- suppressWarnings(sapply(inpath, function(x) nrow(attr(rgdal::GDALinfo(x), 'df'))))
  if(length(inpath) > 26 || nbands > 26) stop('Maximum number of inputs is 26.')
  if(length(nbands) > 1 & any(nbands > 1))
    warning('One or more rasters have multiple bands. First band used.')

  if(length(inpath)==1) {
    inputs <- paste0('-', LETTERS[seq_len(nbands)], ' ', shQuote(inpath), ' --',
                     LETTERS[seq_len(nbands)], '_band ', seq_len(nbands), collapse=' ')
    n <- nbands
  } else {
    inputs <- paste0('-', LETTERS[seq_along(nbands)], ' ', shQuote(inpath), ' --',
                     LETTERS[seq_along(nbands)], '_band 1', collapse=' ')
    n <- length(inpath)
  }

  message('Calculating ',calc_fun,' and writing to ', basename(outpath))
  if(numpy) call1 <- sprintf("%s %s --outfile='%s' --calc='numpy.%s([%s], axis=0)' --co=compress=LZW '%s' %s", gdal_calc, inputs, outpath, calc_fun_in, paste0(LETTERS[seq_len(n)], collapse=','), overwrite,quiet.call)
  else call1 <- sprintf("%s %s --outfile='%s' --calc='%s' --co=compress=LZW '%s' %s", gdal_calc, inputs, outpath, calc_fun_in, overwrite,quiet.call)#  $FILE3 $FILE2 $FILE1
  system(call1)

  if(return.raster){
    r <- raster(outpath)
    return(r)
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
#' @param return.raster logical Should the raster be returned to R environment? Default is TRUE
#' @export
#' @importFrom raster raster

gdalClipWithShape <- function(inrast, inshp, outrast=NULL, field=NULL,
                              cropToShape=FALSE, return.raster=TRUE){

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
    if(is.null(field)) x <- sprintf("gdalwarp -co compress=LZW -of GTiff -cutline '%s' -crop_to_cutline '%s' '%s'",
                                    tmpShpIn, tmpRastIn, tmpRastOut)
    else x <- sprintf("gdalwarp -co compress=LZW -of GTiff -cutline '%s' -cl %s -crop_to_cutline '%s' '%s'",
                      tmpShpIn, field, tmpRastIn, tmpRastOut)
  } else {
    if(is.null(field)) x <- sprintf("gdalwarp -co compress=LZW -of GTiff -cutline '%s' '%s' '%s'",
                                    tmpShpIn, tmpRastIn, tmpRastOut)
    else x <- sprintf("gdalwarp -co compress=LZW -of GTiff -cutline '%s' -cl %s '%s' '%s'",
                      tmpShpIn, field, tmpRastIn, tmpRastOut)
  }

  system(x)
  retrast <- NULL
  if(return.raster) retrast <- raster::raster(tmpRastOut)

  # free up the data
  if(!is.character(inrast)) unlink(tmpRastIn)
  if(!is.character(inshp)) unlink(tmpShpIn)
  # if(!is.character(outrast)|is.null(outrast)) unlink(tmpRastOut)

  return(retrast)

}

#' @title Crop function using gdal
#' @rdname gdalCrop
#' @name gdalCrop
#' @param inpath file path of the inpath raster
#' @param outpath file path of the output raster
#' @param extent extent for raster to be cropped to
#' @param resolution resolution of output raster (usually based on the inpath raster)
#' @param return.raster if TRUE raster will be returned from function call.
#' @export
#'
#' @importFrom raster raster

gdalCrop <- function(inpath, outpath, extent=NULL, resolution=NULL, return.raster = TRUE){

  gdalwarp <- Sys.which('gdalwarp')
  if(gdalwarp=='') stop('gdalwarp not found on system.')
  # if(!file.exists(outpath)) {
    if(is.null(extent))stop('need an extent xmin, xmax, ymin, ymax')
    if(length(resolution)!=2)stop('need x and y resolution')
    message('Cropping ', basename(inpath))
    call1 <- sprintf('gdalwarp -overwrite -co "compress=LZW" -ot Float32 -of gtiff -te %f %f %f %f -tr %f %f %s %s',
            extent[1],extent[3],extent[2],extent[4], resolution[1], resolution[2], inpath, outpath)
    system(call1)
  # }

  if (isTRUE(return.raster)) {
    outraster <- raster::raster(outpath)
    return(outraster)
  }
}

#' @title Calculate distance raster using gdal
#' @rdname gdalDistance
#' @name gdalDistance
#' @param inpath file path of inpath file to change.
#' @param outpath file path of output file to generate.
#' @param target numeric values to calculate distance from default is zero.
#' @param maxdist The maximum distance to be generated. maxdist is in pixels.
#' @param bigtif If the raster is large you might need to use this call.
#' @param return.raster if TRUE raster will be returned from function call.
#' @export

gdalDistance <- function(inpath, outpath, target=0, maxdist=NULL, bigtif=FALSE, return.raster=TRUE){
  gdal_prox <- Sys.which('gdal_proximity.py')
  if(gdal_prox=='') stop('gdal_proximity.py not found on system.')

  if(is.null(target))target <- 0

  if(bigtif){

  if(is.null(maxdist)){
    if(!file.exists(outpath)){
      call1 <- sprintf('%s %s %s -values %d -of GTiff -distunits GEO -nodata -9999 -co compress=LZW -co BIGTIFF=YES', gdal_prox, inpath, outpath, target)
      system(call1)
    }
  } else {
    if(!file.exists(outpath)){
      call1 <- sprintf('%s %s %s -values %d -maxdist %d -distunits GEO -nodata -9999 -of GTiff -co compress=LZW -co BIGTIFF=YES -overwrite', gdal_prox, inpath, outpath, target, maxdist)
      system(call1)
    }
  }

  } else {

  if(is.null(maxdist)){
    if(!file.exists(outpath)){
      call1 <- sprintf('%s %s %s -values %d -of GTiff -distunits GEO -nodata -9999 -co compress=LZW', gdal_prox, inpath, outpath, target)
      system(call1)
    }
  } else {
    if(!file.exists(outpath)){
      call1 <- sprintf('%s %s %s -values %d -maxdist %d -distunits GEO -nodata -9999 -of GTiff -co compress=LZW -overwrite', gdal_prox, inpath, outpath, target, maxdist)
      system(call1)
    }
  }

  }

  if (isTRUE(return.raster)) {
    outraster <- raster::raster(outpath)
    return(outraster)
  }
}

#' @title Mask function using gdal
#' @rdname gdalEdit
#' @name gdalEdit
#' @param inpath file path of the inpath raster
#' @param scale path to a mask file which is NA for cells to mask and 1 for cells to keep.
#' @param offset file path of the output raster
#' @param return.raster if TRUE raster will be returned from function call.
#' @export
#'
#' @importFrom raster raster

gdalEdit <- function(inpath,scale,offset,return.raster=TRUE){

  call1 <- sprintf('gdal_edit.py -scale %s -offset %s "%s"', scale, offset, inpath)
  system(call1)
  if (isTRUE(return.raster)) {
    outraster <- raster::raster(inpath)
    return(outraster)
  }
}

#' @title Extract raster values from points.
#' @rdname gdalExtract
#' @name gdalExtract
#' @param inrasters file path(s) of rasters to extract values
#' @param pts Coordinates of points to extract raster values.
#' @param simplify Logical. If \code{TRUE}, then if all files referred to in
#'   \code{srcfile} are single band rasters, the sampled values will be
#'   simplified to a matrix.
#' @param sp Logical. If \code{TRUE}, a \code{SpatialPointsDataFrame} will be
#'   returned, otherwise a \code{data.frame} will be returned.
#' @export

gdalExtract <- function(inrasters, pts, simplify=TRUE, sp=FALSE) {
  tryCatch(suppressWarnings(system('gdallocationinfo', intern=TRUE)),
           error=function(e) {
             stop('GDAL is not installed, ',
                  'or gdallocationinfo is not available on PATH.')
           })
  stopifnot(all(file.exists(inrasters)))
  if(is(pts, 'SpatialPoints')) {
    p4s <- proj4string(pts)
    pts <- coordinates(pts)
  } else {
    p4s <- NA_character_
  }
  xy <- do.call(paste, as.data.frame(pts))
  vals <- setNames(lapply(inrasters, function(f) {
    meta <- attr(rgdal::GDALinfo(f, returnStats=FALSE, returnRAT=FALSE,
                                 returnColorTable=FALSE), 'df')
    nbands <- nrow(meta)
    nodata <- ifelse(meta$hasNoDataValue, meta$NoDataValue, NA)
    if(any(is.na(nodata))) {
      warning('NoData value not identified for one or more bands of ', f,
              '. Interpret values accordingly. See ?nodata.', call. = FALSE)
    }
    message(sprintf('Querying %sraster data: %s',
                    ifelse(nbands > 1, 'multiband ', ''), f))
    v <- system(sprintf('gdallocationinfo -valonly "%s" -geoloc', f),
                input=xy, intern=TRUE)
    w <- grep('warning', v, value=TRUE, ignore.case=TRUE)
    if (length(w) > 0)
      warning(sprintf('gdallocationinfo returned warning(s) for %s:\n', f),
              paste(w, collapse='\n'), call.=FALSE)
    e <- grep('error', v, value=TRUE, ignore.case=TRUE)
    if (length(e) > 0)
      stop(sprintf('gdallocationinfo returned error(s) for %s:\n', f),
           paste(e, collapse='\n'), call.=FALSE)
    v <- as.numeric(grep('warning|error', v, value=TRUE, invert=TRUE,
                         ignore.case=TRUE))
    v <- ifelse(!is.na(nodata) & v==nodata, NA, v)
    v <- t(matrix(v, nrow=nbands))
  }), inrasters)

  if(isTRUE(simplify) && all(sapply(vals, ncol)==1)) {
    vals <- do.call(cbind, vals)
    colnames(vals) <- inrasters
    vals
  }
  if(is.list(vals) && length(vals)==1) {
    vals <- vals[[1]]
  }
  if(isTRUE(sp)) {
    if(!is.list(vals)) vals <- list(vals)
    spdf <- lapply(vals, function(x) {
      SpatialPointsDataFrame(pts, as.data.frame(vals), proj4string=CRS(p4s))
    })
    if(length(spdf)==1) spdf[[1]] else spdf
  } else {
    vals
  }
}

#' @title Mask function using gdal
#' @rdname gdalMask
#' @name gdalMask
#' @param inpath file path of the inpath raster
#' @param inmask file path which is NA for cells to mask and 1 for cells to keep.
#' @param outpath file path of the output raster
#' @param return.raster if TRUE raster will be returned from function call.
#' @export
#'
#' @importFrom raster raster

gdalMask <- function(inpath, inmask, outpath, return.raster = FALSE) {
  gdal_calc <- Sys.which('gdal_calc.py')
  if(gdal_calc=='') stop('gdal_calc.py not found on system.')
  if(!file.exists(outpath)) {
    message('Masking ', basename(outpath))
    system(
      sprintf('%s --co="COMPRESS=LZW" -A %s -B %s --outpath=%s --calc="A"',
              gdal_calc, inpath, inmask, outpath) )
  }
  if (isTRUE(return.raster)) {
    outraster <- raster::raster(outpath)
    return(outraster)
  }

}


#' @title Split a multiband raster into multiple single files.
#' @rdname gdalMultiband2Singles
#' @name gdalMultiband2Singles
#' @param inpath input multiband raster file path.
#' @param outdir output directory to write multiple single files.
#' @param band which bands to convert, default is NULL (and will do all).
#' @param return_list TRUE, return rasters as a list.
#' @export

gdalMultiband2Singles <- function(inpath, outdir=NULL, bands=NULL, return_list = TRUE){

  gdal_trans <- Sys.which('gdal_translate')
  if(gdal_trans=='') stop('gdal_translate not found on system.')
  if(is.null(outdir)) outdir <- tempdir()

  tmpBrick <- raster::brick(inpath)
  nbands <- raster::nlayers(tmpBrick)

  if(!is.null(bands)) {
    b1 <- range(bands)[1]
    b2 <- range(bands)[2]
  } else {
    b1 <- 1
    b2 <- nbands
  }

  if(nbands>1){
    for (ii in b1:b2){
      tmpRastOut <- paste0(outdir,"/",names(tmpBrick)[ii],"_",ii,".tif")
      call1 <- sprintf("gdal_translate -co compress=LZW '%s' -b %d '%s'", inpath, ii, tmpRastOut)
      system(call1)
    }
  } else{
    message("This is not a multiband object")
    return(NULL)
  }

  if(return_list){
    rast.list <- list()
    for (ii in b1:b2){
      rast.list[[ii]] <- raster(paste0(names(tmpBrick)[ii],"_",ii,".tif"))
    }
    return(rast.list)
  }

}

#' @title Calculate inter quartile range using gdal_calc
#' @rdname gdalIQR
#' @name gdalIQR
#' @param inpath file path of inpath file to change.
#' @param outpath file path of output file to generate.
#' @param overwrite Overwrite the existing raster?
#' @param return.raster if TRUE raster will be returned from function call.
#' @param quiet Do processing quietly?
#' @export

gdalIQR <- function(inpath, outpath, overwrite=TRUE, return.raster=TRUE, quiet=FALSE){

  gdal_calc <- Sys.which('gdal_calc.py')
  if(gdal_calc=='') stop('gdal_calc.py not found on system. Make sure gdal and python installed and visiable to path.')
  if(overwrite)overwrite <- "--overwrite"
  else overwrite <- ""

  if(quiet)quiet.call <- "--quiet"
  else quiet.call <- ""

  nbands <- suppressWarnings(sapply(inpath, function(x) nrow(attr(rgdal::GDALinfo(x), 'df'))))
  if(length(inpath) > 26 || nbands > 26) stop('Maximum number of inputs is 26.')
  if(length(nbands) > 1 & any(nbands > 1))
    warning('One or more rasters have multiple bands. First band used.')

  if(length(inpath)==1) {
    inputs <- paste0('-', LETTERS[seq_len(nbands)], ' ', shQuote(inpath), ' --',
                     LETTERS[seq_len(nbands)], '_band ', seq_len(nbands), collapse=' ')
    n <- nbands
  } else {
    inputs <- paste0('-', LETTERS[seq_along(nbands)], ' ', shQuote(inpath), ' --',
                     LETTERS[seq_along(nbands)], '_band 1', collapse=' ')
    n <- length(inpath)
  }

  call1 <- sprintf("%s %s --outfile='%s' --calc='numpy.quantile([%s], 0.75, axis=0)-numpy.quantile([%s], 0.25, axis=0)' --co=compress=LZW '%s' %s",
                   gdal_calc, inputs, outpath, paste0(LETTERS[seq_len(n)], collapse=','),
                   paste0(LETTERS[seq_len(n)], collapse=','), overwrite, quiet.call)
  system(call1)

  if(return.raster){
    r <- raster(outpath)
    return(r)
  }

}


#' @title Faster reprojectiom of rasters using gdal
#' @rdname gdalReproject
#' @name gdalReproject
#' @param inpath file path of the inpath raster
#' @param outpath file path of the output raster
#' @param xres resolution of output raster x direction.
#' @param yres resolution of output raster y direction - default is the same as xres.
#' @param s_src the EPSG projection of the original raster
#' @param t_src the EPSG projection of the output raster
#' @param resampling The method to resample the raster, default is "bilinear"
#' @param extent The extent of the output raster: xmin, ymin, xmax, ymax
#' @param ot The data output type, default is "Float32".
#' @param of The file output format, default is "GTiFF".
#' @param return.raster Default TRUE, if TRUE return the processed raster to the R environment.
#' @export

gdalReproject <- function(inpath, outpath, xres, yres=xres, s_srs, t_srs, resampling= "bilinear",
                        extent=NULL, ot = "Float32", of='GTiff', return.raster = TRUE) {

  # outpath <- file.path(normalizePath(tempdir()), basename(outpath))
  system(sprintf('gdalwarp -overwrite -ot %s -s_srs "%s" -t_srs "%s" -r %s -te %s %s %s %s -tr %s -dstnodata -9999 -of %s "%s" "%s" -co compress=LZW',
                 ot, s_srs, t_srs, resampling, extent[1],extent[3],extent[2],extent[4], paste(xres, yres), of, inpath, outpath))

  if (isTRUE(return.raster)) {
    outraster <- raster::raster(outpath)
    return(outraster)
  }

}

#' @title Faster rasterisation of shapefile to raster using gdal
#' @rdname gdalRasterise
#' @name gdalRasterise
#' @param shp local shapefile or file path of the inpath raster
#' @param rast local shapefile or file path of the output raster
#' @param res resolution of cells x and y.
#' @param ext extent of raster xmin, xmax, ymin, ymax.
#' @param pixel Number of pixels in x and y direction. This is an alternative to res.
#' @param variable what variable to convert to a raster
#' @param invert If TRUE rasterise the inverse of the shapefile. Default is FALSE.
#' @param return.raster return raster? Default is TRUE and function will return a raster object
#' @export

#' @importFrom raster raster xres yres projection xmin ymin xmax ymax values extent nrow ncol
#' @importFrom rgdal writeOGR

gdalRasterise <- function(shp, rast, res=NULL, ext=NULL, pixels=NULL,
                          variable=NULL, invert = FALSE, return.raster=TRUE) {

  if(!is.null(res)&length(res)==1) res <- rep(res,2)
  if(invert)inv <- "-i"
  else inv <- ""

  if(!is.null(pixels)&length(pixels)!=2)
    stop("If using 'pixels' the input should be the number of pixels in the x and y direction. This argument cannot be used with res.")

  if(is.character(rast)){
    tmpTif <- rast
    if(is.null(res)&is.null(ext)&is.null(pixels)){
      stop("If rast is a file path you must supply 'res' & 'extent' or 'pixels' & 'extent'")
    } else {
      xrs <- res[1]
      yrs <- res[2]
      xmn <- ext[1]
      ymn <- ext[3]
      xmx <- ext[2]
      ymx <- ext[4]
    }
   } else {
     if(!is.raster(rast))stop("Raster must be a raster object or a valid path to a raster file.")
    tmpTif <- tempfile(fileext=".tif")
      if(is.null(res)){
        xrs <- xres(rast)
        yrs <- yres(rast)
       } else {
        xrs <- res[1]
        yrs <- res[2]
      }
    if(is.null(ext)){
      xmn <- xmin(rast)
      ymn <- ymin(rast)
      xmx <- xmax(rast)
      ymx <- ymax(rast)
      } else {
      xmn <- ext[1]
      ymn <- ext[3]
      xmx <- ext[2]
      ymx <- ext[4]
    }
  }


  if(is.character(shp)){
    tmpShp <- shp
  } else {
    tmpShp <- tempfile(fileext=".shp")
    rgdal::writeOGR(shp,dirname(tmpShp),gsub("[.]shp","", basename(tmpShp)),driver="ESRI Shapefile",overwrite=TRUE)
  }

  if( is.null(variable) ) {

    if(is.null(pixels)){
      callnovar <- sprintf( "gdal_rasterize -burn 1 -at -a_nodata -9999 -tr %f %f -te %f %f %f %f '%s' '%s' -co compress=LZW",
                          xrs, yrs, xmn, ymn, xmx, ymx, tmpShp, tmpTif)
    } else {
      callnovar <- sprintf( "gdal_rasterize -burn 1 -at -a_nodata -9999 -ts %f %f -te %f %f %f %f '%s' '%s' -co compress=LZW",
                           pixels[1], pixels[2], xmn, ymn, xmx, ymx, tmpShp, tmpTif)
    }
    system(callnovar)
  } else {
    if(is.null(pixels)){
      callvar <- sprintf( "gdal_rasterize  -a '%s' -at -a_nodata -9999 -tr %f %f -te %f %f %f %f '%s' '%s' -co compress=LZW",
                          variable, xrs, yrs, xmn, ymn, xmx, ymx, tmpShp, tmpTif)
    } else {
      callvar <- sprintf( "gdal_rasterize  -a '%s' -at  -a_nodata -9999 -ts %f %f -te %f %f %f %f '%s' '%s' -co compress=LZW",
                          variable, pixels[1], pixels[2], xmn, ymn, xmx, ymx, tmpShp, tmpTif)
    }
    system(callvar)
  }

  # if(bigtif)return(print(tmpTif))

  # create a new raster object
  if(return.raster)r.out <- raster(tmpTif)
  else r.out <- NULL

  # free up the data
  if(!is.character(rast)) unlink(tmpTif)
  if(!is.character(shp)) unlink(tmpShp)

  return(r.out)
}


#' @title Reclassify raster using gdal_calc
#' @rdname gdalReclassify
#' @name gdalReclassify
#' @param inpath file path of inpath file to change.
#' @param outpath file path of output file to generate.
#' @param reclassify_list A list of values to reclassify, the value will be assigned by list index, e.g the first element will be made 1 and so forth. Values associated with that list will be converted to that number.
#' @param calc_fun A text function that follows the "https://gdal.org/programs/gdal_calc.html" syntax.
#' @param return.raster if TRUE raster will be returned from function call.
#' @export

gdalReclassify <- function(inpath, outpath, reclassify_list=NULL, calc_fun=NULL, overwrite=TRUE, return.raster=TRUE){

  # if(all(is.null(c(calc_fun,reclassify_list)))) stop("You must provide a calc_fun function or reclassify_list")

  gdal_calc <- Sys.which('gdal_calc.py')
  if(gdal_calc=='') stop('gdal_calc.py not found on system. Make sure gdal and python installed and visiable to path.')
  if(overwrite)overwrite <- "--overwrite"
  else overwrite <- ""

  if(!is.null(reclassify_list)&&is.null(calc_fun)){
    message("Using reclassify_list to reclass raster\n")
    reclassify_list <- list(crop = c(10, 11, 12, 20, 30),
                         crop_mosaic = 40,
                         forest = c(50, 60, 61, 62, 70, 80, 90, 100, 160, 170), #>15% cover
                         grass = c(110, 130),
                         wetland = c(180),
                         urban = c(190),
                         shrub = c(120, 121,122),
                         other = c(140, 150, 151, 152, 153, 200,201,202,220),
                         water = 210)

    reclass_idx <- function(idx, x) {
      .x <- x[[idx]]
      paste0("(",idx,"*(",paste0("(A==",.x,")",collapse = "+"),"))")
    }
    calc_list_out <- lapply(seq_along(reclassify_list), reclass_idx, x = reclassify_list)
    calc_fun_in <- paste0(calc_list_out,collapse = "+")
  } else if (!is.null(reclassify_list)&&!is.null(calc_fun)){
    message("Using calc_fun instead of reclassify_list to reclass raster\n")
    calc_fun_in <- calc_fun
  } else if (is.null(reclassify_list)&&!is.null(calc_fun)){
    message("Using calc_fun instead of reclassify_list to reclass raster\n")
    calc_fun_in <- calc_fun
  } else {
    stop("You must provide a calc_fun function or reclassify_list\n")
  }

  call1 <- sprintf("%s -A '%s' --outfile='%s' --calc='%s' --co=compress=LZW '%s'", gdal_calc, inpath, outpath, calc_fun_in, overwrite)#  $FILE3 $FILE2 $FILE1
  system(call1)

  if(return.raster){
    r <- raster(outpath)
    return(r)
  }


}

#' @title Resample raster to different resolution
#' @rdname gdalResample
#' @name gdalResample
#' @param inpath file path of inpath file to change.
#' @param outpath file path of output file to generate.
#' @param resolution desired new resolution for x and y.
#' @param method resample method, default is nearest neighbour which is fast but dodgy. Options are: "near", "bilinear", "cubic", "cubicspline",
# "lanczos", "average", "mode", "max", "min", "med", "q1","q3"
#' @param bigtif if TRUE deal with a big geotiff slightly differently.
#' @param return.raster if TRUE raster will be returned from function call.
#' @export
#' @importFrom raster raster

gdalResample <- function(inpath, outpath = NULL,
                         resolution=NULL,
                         method = 'bilinear', bigtif = FALSE,
                         return.raster = TRUE){

  if(is.null(outpath)){
    tmpTifOut <- tempfile(fileext=".tif")
  } else {
    tmpTifOut <- outpath
  }

  # if(!is.null(resampleRaster))  resolution <- raster::res(resampleRaster)
  if(length(resolution)!=2)stop("Target resolution need for x and y.")

  if (!method %in% c("near", "bilinear", "cubic", "cubicspline",
                     "lanczos", "average", "mode", "max", "min", "med", "q1",
                     "q3")) {
    stop("Resampling method not available.")
  }

  # resample_command <- paste0("gdalwarp -multi -of vrt -tr ",
  #                            " ", resolution[1], " ", resolution[2], " -r ",
  #                            method," ",inpath, " ", gsub(tools::file_ext(tmpTifOut), "vrt", tmpTifOut))
  if (isTRUE(bigtif)) {
    resample_command <- paste0("gdalwarp -multi -of vrt -tr ",
                               " ", resolution[1], " ", resolution[2], " -r ",
                               method," '",inpath, "' '", gsub(tools::file_ext(tmpTifOut), "vrt", tmpTifOut),"'")
    VRT2TIF <- paste0("gdal_translate -co compress=LZW -co BIGTIFF=YES",
                     " ", gsub(tools::file_ext(tmpTifOut), "vrt", tmpTifOut),
                      " ", gsub(tools::file_ext(tmpTifOut), "tif", tmpTifOut))
    system(resample_command)
    system(VRT2TIF)
    unlink(gsub(tools::file_ext(tmpTifOut), "vrt", tmpTifOut))
  } else {
    resample_command <- paste0("gdalwarp overwrite -co compress=LZW -tr ",
                               " ", resolution[1], " ", resolution[2], " -r ",
                               method," '",inpath, "' '",tmpTifOut,"'")
    system(resample_command)
  }
  if (isTRUE(return.raster)) {
    outraster <- raster::raster(tmpTifOut)
    return(outraster)
  }
}

#' @title Stitch together tiles.
#' @rdname gdalStitchTitles
#' @name gdalStitchTitles
#' @param tilespath  file path to the raster tiles. use "/path/*.tif"
#' @param outpath file path of output file to generate.
#' @param bigtif Is the raster very large > 4GB; If so use this call.
#' @param return.raster default TRUE, raster returned to R environment.

#' @export

# tilespath <- "/home/woo457/Dropbox/AFMA_cumulative_impacts/data/covariate_data/tmpfolders/*.tif"
# outpath <- "/home/woo457/Dropbox/AFMA_cumulative_impacts/data/covariate_data/tmpfolders/built_to_split.tif"

gdalStitchTitles <- function(tilespath, outpath, bigtif = TRUE, return.raster = TRUE){

  buildVRT <- paste0("gdalbuildvrt", " ", gsub(pkgmaker::file_extension(outpath),
                                               "vrt", outpath), " ", tilespath)

  if (bigtif == TRUE) {
    VRT2TIF <- paste0("gdal_translate -co compress=LZW -co BIGTIFF=YES",
                      " ", gsub(pkgmaker::file_extension(outpath),
                                "vrt", outpath), " ", gsub(pkgmaker::file_extension(outpath),
                                                           "tif", outpath))
  } else {
    VRT2TIF <- paste0("gdal_translate -co compress=LZW",
                      " ", gsub(pkgmaker::file_extension(outpath),
                                "vrt", outpath), " ", gsub(pkgmaker::file_extension(outpath),
                                                           "tif", outpath))
  }
  system(buildVRT)
  system(VRT2TIF)
  unlink(gsub(pkgmaker::file_extension(outpath), "vrt",
              outpath))
  if (return.raster) {
    out <- raster::raster(outpath)
    return(out)
  }
}


#' @title Calculate distance raster using gdal
#' @rdname gdalTiles
#' @name gdalTiles
#' @param inpath file path of inpath file to change.
#' @param outdir file path of output file to generate.
#' @param nx The number of x direction pixels per tile
#' @param ny The number of x direction pixels per tile
#' @export

gdalTiles <- function(inpath, outdir, nx=256, ny=nx){

  gdal_retile <- Sys.which('gdal_retile.py')
  if(gdal_retile=='') stop('gdal_retile.py not found on system.')

  call1 <- sprintf("%s -ps %d %d -targetDir '%s' '%s' -co compress=LZW", gdal_retile, nx, ny, outdir, inpath)
  system(call1)

}

#' @export
is.raster <- function(x){
  return((class(x)=="RasterLayer" || class(x)=="RasterBrick" || class(x)=="RasterStack"))
}

