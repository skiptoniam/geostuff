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



