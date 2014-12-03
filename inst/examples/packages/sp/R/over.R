.overDF = function(r, data, n, returnList, fn, ...) {
	if (returnList == FALSE && is.null(fn))
		ret = data[sapply(r, function(x) x[1]), , drop=FALSE]
	else {
		ret = lapply(1:n, function(x) data[r[[x]],,drop=FALSE]) # list of data.frames
		if (returnList == FALSE) { # apply fn:
			ret = do.call(rbind, # rbind each aggregated record
				lapply(ret, # apply to each data.frame in ret:
					function(x) {
						if (nrow(x) == 0)
							data.frame(lapply(x, function(xx) c(xx, NA)))
						else
							data.frame(lapply(x, fn, ...))
					}
				)
			)
			ret[is.na(ret)] = NA # removes NaN's
			ret = as.data.frame(ret)
		} 
	}
	ret
}
overDF_for_rgeos = .overDF # to be exported, for rgeos only

# we need to invert a list of indexes, i.e.
# list(c(1,4), c(2,4,5))
# needs to become
# list(c(1), c(2), integer(0), c(1,2), c(2))
# the expensive way is to form the full matrix, as in:
#
#.invert = function(lst, nr, nc) {
#	stopifnot(nr == length(lst))
#	m = matrix(FALSE, nr, nc)
#	for (i in 1:nr)
#		m[i,lst[[i]]] = TRUE
#	lapply(1:nc, function(x) which(m[,x]))
#}
# but the following does this more efficient, memory-wise:
.invert = function(x, nr, nc) { 
	stopifnot(nr == length(x)) # obsolete argument!
	ret = cbind(rep(1:nr, times = sapply(x, length)), unlist(x))
	ret = split(ret[,1], ret[,2])
	# initialize return list with empty cells:
	lst = lapply(1:nc, function(x) integer(0))
	idx = as.integer(names(ret))
	lst[idx] = ret
	lst
}

'%over%' = function(x,y) over(x,y)

# when changing this function, we also might want to change
# overGeomGeomDF in rgeos,
# ... and overDFGenericST in spacetime.
overDFGeneric = function(x, y, returnList = FALSE, fn = NULL, ...) {
	stopifnot(identicalCRS(x, y))
	r = over(x, geometry(y), returnList = TRUE)
	ret = .overDF(r, y@data, length(x), returnList, fn, ...)
	if (! returnList)
		row.names(ret) = row.names(x)
	ret
}

setMethod("over",
	signature(x = "SpatialPoints", y = "SpatialPoints"), 
		function(x, y, returnList = FALSE, fn = NULL, ...) {
			stopifnot(identicalCRS(x, y))
			zd = zerodist2(x, y)
			if (returnList) {
				ret = lapply(1:length(x), function(X) integer(0))
				s = split(zd[,2],zd[,1])
				ix = as.integer(names(s))
				ret[ix] = s
			} else {
				ret = rep(as.integer(NA), length(x))
				ret[zd[,1]] = zd[,2]
			}
			ret
		}
)
setMethod("over",
	signature(x = "SpatialPoints", y = "SpatialPolygons"), 
		function(x, y, returnList = FALSE, fn = NULL, ...) {
			stopifnot(identicalCRS(x, y))
			r = pointsInSpatialPolygons(x, y, returnList)
			if (returnList)
				r = .invert(r, length(y), length(x))
			r
		}
)
setMethod("over",
	signature(x = "SpatialPolygons", y = "SpatialPoints"), 
		function(x, y, returnList = FALSE, fn = NULL, ...) {
			stopifnot(identicalCRS(x,y))
			r = pointsInSpatialPolygons(geometry(y), geometry(x), TRUE)
			if (!returnList)
				r = sapply(r, function(x) x[1])
			r
		}
)
setMethod("over",
	signature(x = "SpatialGrid", y = "SpatialPoints"), 
		function(x, y, returnList = FALSE, fn = NULL, ...)
			over(as(x, "SpatialPoints"), y = y, returnList = returnList, fn = fn, ...)
)
setMethod("over",
	signature(x = "SpatialGrid", y = "SpatialPolygons"), 
		function(x, y, returnList = FALSE, fn = NULL, ...)
			over(as(x, "SpatialPoints"), y = y, returnList = returnList, fn = fn, ...)
)
setMethod("over",
	signature(x = "SpatialGrid", y = "SpatialPixels"), 
		function(x, y, returnList = FALSE, fn = NULL, ...)
			over(as(x, "SpatialPoints"), y = y, returnList = returnList, fn = fn, ...)
)
setMethod("over",
	signature(x = "SpatialGrid", y = "SpatialGrid"), 
		function(x, y, returnList = FALSE, fn = NULL, ...)
			over(as(x, "SpatialPoints"), y = y, returnList = returnList, fn = fn, ...)
)
setMethod("over",
	signature(x = "SpatialPolygons", y = "SpatialGrid"), 
		function(x, y, returnList = FALSE, fn = NULL, ...)
			over(x = x, y = as(y, "SpatialPoints"), returnList = returnList, fn = fn, ...)
)
setMethod("over", signature("SpatialPoints", "SpatialGrid"), 
	function(x, y, returnList = FALSE, fn = NULL, ...) {
		stopifnot(identicalCRS(x,y))
		idx = getGridIndex(coordinates(x), y@grid, all.inside = FALSE)
		.index2list(idx, returnList)
	}
)
setMethod("over", signature("SpatialPoints", "SpatialPixels"), 
	function(x, y, returnList = FALSE, fn = NULL, ...) {
		stopifnot(identicalCRS(x,y))
		idx = getGridIndex(coordinates(x), y@grid, all.inside = FALSE)
		idx = match(idx, y@grid.index)
		.index2list(idx, returnList)
	}
)
setMethod("over",
	signature(x = "SpatialPoints", y = "SpatialPointsDataFrame"), 
		overDFGeneric)
setMethod("over",
	signature(x = "SpatialPoints", y = "SpatialPolygonsDataFrame"), 
		overDFGeneric)
setMethod("over",
	signature(x = "SpatialGrid", y = "SpatialPointsDataFrame"), 
		function(x, y, returnList = FALSE, fn = NULL, ...)
			over(as(x, "SpatialPoints"), y = y, returnList = returnList, fn = fn, ...)
)
setMethod("over",
	signature(x = "SpatialGrid", y = "SpatialPolygonsDataFrame"), 
		function(x, y, returnList = FALSE, fn = NULL, ...)
			over(as(x, "SpatialPoints"), y = y, returnList = returnList, fn = fn, ...)
)
setMethod("over",
	signature(x = "SpatialGrid", y = "SpatialPixelsDataFrame"), 
		function(x, y, returnList = FALSE, fn = NULL, ...)
			over(as(x, "SpatialPoints"), y = y, returnList = returnList, fn = fn, ...)
)
setMethod("over",
	signature(x = "SpatialGrid", y = "SpatialGridDataFrame"), 
		function(x, y, returnList = FALSE, fn = NULL, ...)
			over(as(x, "SpatialPoints"), y = y, returnList = returnList, fn = fn, ...)
)
setMethod("over",
	signature(x = "SpatialPolygons", y = "SpatialPointsDataFrame"), 
		overDFGeneric)
setMethod("over",
	signature(x = "SpatialPolygons", y = "SpatialGridDataFrame"), 
		function(x, y, returnList = FALSE, fn = NULL, ...) {
			stopifnot(identicalCRS(x,y))
			over(x, as(y, "SpatialPixelsDataFrame"), returnList = returnList,
				fn = fn, ...)
		}
)
setMethod("over", signature("SpatialPoints", "SpatialGridDataFrame"), 
	function(x, y, returnList = FALSE, fn = NULL, ...) {
		stopifnot(identicalCRS(x,y))
		idx = over(x, geometry(y))
		ret = y@data[idx,,drop=FALSE]
		row.names(ret) = row.names(x)
		.index2list(ret, returnList)
	}
)
setMethod("over", signature("SpatialPoints", "SpatialPixelsDataFrame"), 
	function(x, y, returnList = FALSE, fn = NULL, ...) {
		stopifnot(identicalCRS(x,y))
		idx = over(x, geometry(y))
		ret = y@data[idx,,drop=FALSE]
		row.names(ret) = row.names(x)
		.index2list(ret, returnList)
	}
)

.index2list = function(x, returnList) {
	if (returnList) {
		l = lapply(1:length(x), function(x) { integer(0) })
		notNA = !is.na(x)
		l[notNA] = x[notNA]
		l
	} else
		x
}

aggregate.Spatial = function(x, by, FUN = mean, ...) {
	by0 = by
	if (gridded(by))
		by = as(by, "SpatialPolygons")
	df = over(by, x, fn = FUN, ...)
	addAttrToGeom(by0, df, match.ID = FALSE)
}
