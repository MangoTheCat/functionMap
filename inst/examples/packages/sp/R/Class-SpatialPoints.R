setClass("SpatialPoints",
	contains = "Spatial", 
	#slots = c(coords = "matrix"),
	representation(coords = "matrix"),
	prototype = list(bbox=matrix(NA), 
		proj4string = CRS(as.character(NA)),
		coords = matrix(0)),
	validity = function(object) {
		if (!is.matrix(object@coords))
			stop("coords slot is not a matrix")
		if (length(object@coords) == 0)
			stop("coords cannot have zero length")
		if (nrow(object@coords) < 1)
			stop("no points set: too few rows")
		if (ncol(object@coords) < 2)
			stop("no points set: too few columns")
		if (!is.double(object@coords[,1]))
			stop("coordinates should be double")
		return(TRUE)
	}
)
