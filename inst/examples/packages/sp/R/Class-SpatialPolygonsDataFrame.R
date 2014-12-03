setClass("SpatialPolygonsDataFrame",
	contains = "SpatialPolygons", 
	#slots = c(data = "data.frame"),
	representation(data = "data.frame"),
	prototype = list(
		bbox = matrix(rep(NA, 6), 3, 2, dimnames = list(NULL, c("min","max"))),
		proj4string = CRS(as.character(NA)),
		polygons = list(), 
		plotOrder = integer(0),
		data = data.frame()),
	validity = function(object) {
		if (!inherits(object@data, "data.frame"))
			stop("data should be of class data.frame")
		if (nrow(object@data) != length(object@polygons))
		  stop("number of rows in data.frame and polygons in SpatialPolygons don't match")
		return(TRUE)
	}
)

