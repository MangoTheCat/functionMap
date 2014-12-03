spDistsN1 <- function(pts, pt, longlat=FALSE) {
	if (inherits(pts, "SpatialPoints")) pts <- coordinates(pts)
	if (!is.matrix(pts)) stop("pts must be a matrix")
	if (ncol(pts) != 2) stop("pts must have two columns")
	if (!is.numeric(pts)) stop("pts must be numeric")
	if (inherits(pt, "SpatialPoints")) pt <- coordinates(pt)
	if (!is.numeric(pt)) stop("pt must be numeric")
	if (length(pt) != 2) stop("pt must have length two")
        storage.mode(pts) <- "double"
        storage.mode(pt) <- "double"
	x <- pts[,1]
	y <- pts[,2]
	xx <- pt[1]
	yy <- pt[2]
	n  <- as.integer(length(x))
	dists <- vector(mode="double", length=n)
	lonlat <- as.integer(longlat)
	res <- .C("sp_dists", x, y, xx, yy, n, dists, lonlat, 
		PACKAGE = "sp")[[6]]
	if (any(!is.finite(res))) {
		nAn <- which(!is.finite(res))
		dx <- abs(x[nAn] - xx)
		dy <- abs(y[nAn] - yy)
		if (all((c(dx, dy) < .Machine$double.eps ^ 0.5)))
			res[nAn] <- 0
		else
			stop(paste("non-finite distances in spDistsN1"))
	}
	res
}

spDists <- function(x, y = x, longlat = FALSE) {
	if (is(x, "Spatial")) {
		stopifnot(identical(proj4string(x), proj4string(y)))
		if (missing(longlat))
			longlat = !is.na(is.projected(x)) && !is.projected(x)
		x = coordinates(x)
		y = coordinates(y)
	}
	stopifnot(ncol(x) == ncol(y))
	if (ncol(x) != 2) {
		if (longlat)
			stop("cannot compute spherical distances for longlat data in more than 2 dimensions")
    	d = outer(x[,1], y[,1], "-") ^ 2
        if (ncol(x) > 2)
			for (i in 2:ncol(x))
           		d = d + outer(x[,i], y[,i], "-") ^ 2
    	matrix(sqrt(d), nrow(x), nrow(y))
	} else {
		spDiN1 = function(x, y, ll) spDistsN1(y, x, ll)
		if (nrow(x) < nrow(y))
			matrix(t(apply(x, 1, spDiN1, y = y, ll = longlat)), nrow(x), nrow(y))
		else
			matrix(apply(y, 1, spDiN1, y = x, ll = longlat), nrow(x), nrow(y))
	}
}
