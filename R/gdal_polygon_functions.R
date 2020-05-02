#' @title Clip vector file ogr2ogr with a bounding box
#' @rdname ogrBBoxClip
#' @name ogrBBoxClip
#' @param inpath file path of the inpath raster
#' @param outpath file path of the output raster
#' @param extent extent for raster to be cropped to
#' @param returnShape if TRUE raster will be returned from function call.
#' @export
#'
#' @importFrom raster raster

ogrBBoxClip <- function(inshp, outshp=NULL, ext, returnShape=TRUE) {

  stopifnot(class(ext)%in%"Extent")

  if(!is.character(inshp)){
    tmpShpIn <- tempfile(fileext="in.shp")
    rgdal::writeOGR(inshp,dirname(tmpShpIn),gsub("[.]shp","", basename(tmpShpIn)),driver="ESRI Shapefile",overwrite=TRUE)
  } else {
    tmpShpIn <- inshp
  }
  if(!is.character(outshp)|is.null(outshp)){
    tmpShpOut <- tempfile(fileext="out.shp")
  } else {
    tmpShpOut <- outshp
  }

  ##
  system(sprintf("ogr2ogr -f 'ESRI Shapefile' '%s' '%s' -clipsrc %f %f %f %f ",
                 tmpShpOut, tmpShpIn, ext@xmin, ext@ymin, ext@xmax, ext@ymax))

  retshp <- NULL
  if(returnShape) retshp <- rgdal::readOGR(tmpShpOut)

  # free up the data
  if(!is.character(inshp)) unlink(tmpShpIn)
  if(!is.character(outshp)|is.null(outshp)) unlink(tmpShpOut)

  return(retshp)
}

# inshp <-"/home/woo457/Dropbox/NESP SS2/GIS-DATA/Australia/australia-boundary-clip-marine.shp"
# inrast <- "/home/woo457/Dropbox/NESP SS2/Pressures/Pressures2020Processed//Land-use_intensification/Point discharges/NESP_NOD_TSS_exp_smoothed_SpatiallyProcessed.tif"
# outrast <- NULL

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


