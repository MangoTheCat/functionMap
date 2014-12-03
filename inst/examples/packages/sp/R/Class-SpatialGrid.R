setClass("SpatialPixels", 
	contains = "SpatialPoints",
	#slots = c(grid = "GridTopology", grid.index = "integer"),
	representation(grid = "GridTopology", grid.index = "integer"),
	validity = function(object) {
		if (nrow(object@coords) != length(object@grid.index))
			stop("grid.index should have length equal to nrow(coords)")
		if (max(object@grid.index) > .NumberOfCells(object@grid))
			stop("grid.index max value too large")
		if (min(object@grid.index) < 1)
			stop("grid.index min value too small")
		return(TRUE)
	}
)

setClass("SpatialGrid",
	contains = "Spatial", 
	#slots = c(grid = "GridTopology"),
	representation(grid = "GridTopology"),
	validity = function(object) {
		return(TRUE)
	}
)
