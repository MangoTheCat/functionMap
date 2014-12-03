overlayPointsWithPolygons = function(x, y, fn = NULL) {
	if (interactive())
		.Deprecated("over", package = "sp")
	# x = pts, y = rings, return ring with f(grid) items
	y = as(y, "SpatialPolygons")
	id = pointsInSpatialPolygons(x, y)
	if (!is.null(fn))
		## WAS: data.frame(t(data.frame(lapply(split(df, id), fn))))
		## calls fn on complete data.frame; the following calls fn
		## on each column:
		do.call(rbind, lapply(split(x@data, id), function(x) sapply(x, fn)))
	else
		id
}

setMethod("overlay", 
	signature(x = "SpatialPointsDataFrame", y = "SpatialPolygons"), 
		 overlayPointsWithPolygons)

setMethod("overlay",  # added
	signature(x = "SpatialGridDataFrame", y = "SpatialPolygons"), 
		 function(x, y, ...) overlayPointsWithPolygons(as(x, "SpatialPoints"), y, ...))

setMethod("overlay", 
	signature(x = "SpatialPoints", y = "SpatialPolygons"), 
		function(x, y, ...) overlayPointsWithPolygons(x, y))  # no fn argument!

setMethod("overlay",  # added
	signature(x = "SpatialGrid", y = "SpatialPolygons"), 
		function(x, y, ...) overlayPointsWithPolygons(as(x, "SpatialPoints"), y))  # no fn argument!

overlayPolygonsWithPoints = function(x, y, ...) {
	.Deprecated("over", package = "sp")
	# x = rings, y = pts; return pts with ring values (or id) at grid point
	ypts = as(y, "SpatialPoints")
	sr = as(x, "SpatialPolygons")
	id = pointsInSpatialPolygons(ypts, sr)
	if (is(x, "SpatialPolygonsDataFrame")) {
		ret = x@data[id, , drop = FALSE]
		if (is(y, "SpatialPointsDataFrame"))
			row.names(ret) = row.names(as(y, "data.frame"))
		return(ret)
	} else
		return(id) # 
}

setMethod("overlay", signature("SpatialPolygons", "SpatialPoints"), 
	overlayPolygonsWithPoints)

setMethod("overlay", signature("SpatialPolygons", "SpatialGrid"),  # added
	function(x,y,...) overlayPolygonsWithPoints(x, as(y, "SpatialPoints"), ...))

#overlayGridWithPoints = function(x, y, fn = NULL) {
#   cc = coordinates(y)
#   idx = getGridIndex(cc, x@grid, all.inside = FALSE)
#   if (!fullgrid(x))
#       idx = match(idx, x@grid.index)
#   if (is(x, "SpatialGridDataFrame")) {
#       data = as.data.frame(x@data[idx, ])  ## make sure we return a data frame
#       names(data) = names(x@data)
#       SpatialPointsDataFrame(cc, data, proj4string = CRS(proj4string(x)))
#       }
#   else
#       return(idx)
#}

overlayGridWithPoints = function(x, y, fn = NULL) {
	.Deprecated("over", package = "sp")
	cc = coordinates(y)
	idx = getGridIndex(cc, x@grid, all.inside = FALSE)
	if (!fullgrid(x))
		idx = match(idx, x@grid.index)
	if (is(x, "SpatialGridDataFrame")) {
# Rainer Krug 090331 rownames problem in maptools shapefile import
                cc <- SpatialPoints(cc, proj4string = CRS(proj4string(x)))
		SpatialPointsDataFrame(cc, x@data[idx, , drop=FALSE])
	} else
		return(idx)
}
setMethod("overlay", signature("SpatialGridDataFrame", "SpatialPoints"), 
	overlayGridWithPoints)

setMethod("overlay", signature("SpatialGrid", "SpatialPoints"), 
	overlayGridWithPoints)

setMethod("overlay", signature("SpatialPixelsDataFrame", "SpatialPoints"), 
	overlayGridWithPoints)

setMethod("overlay", signature("SpatialPixels", "SpatialPoints"), 
	overlayGridWithPoints)

#overlayPointsWithGrid = function(x, y, fn = NULL) {
#	cc = coordinates(x)
#	idx = getGridIndex(cc, y@grid, all.inside = FALSE)
#	if (!fullgrid(x))
#		idx = match(idx, x@grid.index)
#	if (is(x, "SpatialGridDataFrame"))
#		SpatialPointsDataFrame(cc, x@data[idx, ], proj4string = CRS(proj4string(x)))
#	else
#		return(idx)
#}
#
#setMethod("overlay", signature("SpatialPointsDataFrame", "SpatialGrid"), 
#	overlayPointsWithGrid)
#setMethod("overlay", signature("SpatialPoints", "SpatialGrid"), 
#	overlayPointsWithGrid)
