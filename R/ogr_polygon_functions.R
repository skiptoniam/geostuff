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


#' @title Merge multple shapefiles
#' @rdname mergeOGR
#' @name mergeOGR
#' @param infile.list A list of path files that points to shape files
#' @param outfile The path to shapefile you wish to write too
#' @export


mergeOGR <- function(infiles.list,outfile){

  if(!is.list(infiles.list))stop("infiles.list should be a list of shapefile paths.")
  if(length(infiles.list)<2)stop("infiles.list should have more that one shapefile to merge")

  system(sprintf("ogr2ogr -f 'ESRI Shapefile' '%s' '%s'",outfile,infiles.list[[1]]))
  for(i in 2:length(infiles.list)){
    system(sprintf("ogr2ogr -f 'ESRI Shapefile' -update -append '%s' '%s'",outfile,infiles.list[[i]]))
  }

}

