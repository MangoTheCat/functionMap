if (!isGeneric("recenter"))
	setGeneric("recenter", function(obj)
		standardGeneric("recenter"))

recenter.SpatialPolygons <- function(obj) {
	proj <- is.projected(obj)
	if (is.na(proj)) stop("unknown coordinate reference system")
	if (proj) stop("cannot recenter projected coordinate reference system")
	projargs <- CRS(proj4string(obj))
	pls <- slot(obj, "polygons")
	Srl <- lapply(pls, recenter.Polygons)
	res <- SpatialPolygons(Srl, proj4string=projargs)
	res
}

setMethod("recenter", "SpatialPolygons", recenter.SpatialPolygons)

recenter.Polygons <- function(obj) {
	ID <- slot(obj, "ID")
	rings <- slot(obj, "Polygons")
	srl <- lapply(rings, recenter.Polygon)
	res <- Polygons(srl, ID=ID)
	res
}


recenter.Polygon <- function(obj) {
	crds <- slot(obj, "coords")
	hole <- slot(obj, "hole")
	inout <- (crds[,1] < 0)
	if (all(inout)) {
		crds[,1] <- crds[,1]+360
	} else {
		if (any(inout)) {
			crds[,1] <- ifelse(inout, crds[,1]+360, crds[,1])
		}
	}
	res <- Polygon(crds, hole)
	res
}



recenter.SpatialLines <- function(obj) {
	proj <- is.projected(obj)
	if (is.na(proj)) stop("unknown coordinate reference system")
	if (proj) stop("cannot recenter projected coordinate reference system")
	projargs <- CRS(proj4string(obj))
	lns <- slot(obj, "lines")
	Sll <- lapply(lns, recenter.Lines)
	res <- SpatialLines(Sll, projargs)
	res
}

setMethod("recenter", "SpatialLines", recenter.SpatialLines)


recenter.Lines <- function(obj) {
	ID <- slot(obj, "ID")
	lines <- slot(obj, "Lines")
	sll <- lapply(lines, recenter.Line)
	res <- Lines(sll, ID=ID)
	res
}


recenter.Line <- function(obj) {
	crds <- coordinates(obj)
	inout <- (crds[,1] < 0)
	if (all(inout)) {
		crds[,1] <- crds[,1]+360
	} else {
		if (any(inout)) {
			crds[,1] <- ifelse(inout, crds[,1]+360, crds[,1])
		}
	}
	res <- Line(crds)
	res
}


nowrapSpatialLines <- function(obj, offset=0, eps=rep(.Machine$double.eps, 2)) {
	if (!is(obj, "SpatialLines")) stop("obj not a SpatialLines object")
	proj <- is.projected(obj)
	if (is.na(proj)) stop("unknown coordinate reference system")
	if (proj) stop("cannot recenter projected coordinate reference system")
	proj4CRS <- CRS(proj4string(obj))
	bblong <- bbox(obj)[1,]
	inout <- bblong[1] < offset && bblong[2] >= offset
	if (inout) {
		pls <- slot(obj, "lines")
		Srl <- lapply(pls, .nowrapLines, offset=offset, eps=eps)
		res <- SpatialLines(Srl, proj4CRS)
	} else res <- obj
	res
}

.nowrapLines <- function(obj, offset=0, eps=rep(.Machine$double.eps, 2)) {
	bbo <- range(sapply(obj@Lines, function(x) range(x@coords[,1])))
	inout <- bbo[1] < offset && bbo[2] >= offset
	if (inout) {
		lines <- slot(obj, "Lines")
		ID <- slot(obj, "ID")
		sll <- list()
		for (i in 1:length(lines)) {
			sll <- c(sll, .nowrapLine(lines[[i]], 
				offset=offset, eps=eps))
		}
		res <- Lines(sll, ID=ID)
	} else res <- obj
	res
}

.nowrapLine <- function(obj, offset=0, eps=rep(.Machine$double.eps, 2)) {
	bbo <- range(obj@coords[,1])
	inout <- bbo[1] < offset && bbo[2] >= offset
	if (inout) {
		crds <- coordinates(obj)
#		zoffset <- as.logical(sapply(crds[,1], function(x) 
#			all.equal(x, offset, tolerance = .Machine$double.eps)))
		zoffset <- as.logical(sapply(crds[,1], function(x) 
			abs(x - offset) < eps[1]))
		if (any(zoffset)) {
			brks <- which(zoffset)
			n <- length(brks)
			for (i in 1:n) {
				ib <- brks[i]
				if (crds[(ib-1),1] < offset) 
					crds[ib,1] <- crds[ib,1] - eps[1]
				else crds[ib,1] <- crds[ib,1] + eps[1]
			}
		}
		inout <- (crds[,1] <= offset)
		rle_io <- rle(as.integer(inout))
		brks <- cumsum(rle_io$lengths)
		n <- length(brks)
		res <- list()
		if (n == 0) {
			if (all(crds[,1] < offset)) 
				crds[,1] <- crds[,1] - eps[1]
			else crds[,1] <- crds[,1] + eps[2]
			res <- c(res, list(Line(crds)))
		} else {
			for (i in 1:n) {
				if (i == n) outcrds <- crds
				else {
					ib <- brks[i]
					pt1 <- crds[ib,]
					pt2 <- crds[(ib+1),]
					dpts <- pt1 - pt2
					if (pt1[1] <= offset) {
						x1 <- offset - eps[1]
						x2 <- offset + eps[2]
						y1 <- pt1[2] + 
							abs(pt1[1]-eps[1])*
							(dpts[2] / dpts[1])
						y2 <- pt1[2] + 
							abs(pt1[1]+eps[2])*
							(dpts[2] / dpts[1])
					} else {
						x1 <- offset + eps[1]
						x2 <- offset - eps[2]
						y1 <- pt1[2] - 
							abs(pt1[1]-eps[1])*
							(dpts[2] / dpts[1])
						y2 <- pt1[2] - 
							abs(pt1[1]+eps[2])*
							(dpts[2] / dpts[1])
					}
					outcrds <- rbind(crds[1:ib,], c(x1, y1))
					brks <- brks - nrow(outcrds) + 2
					crds <- rbind(c(x2, y2), crds[-(1:ib),])
				}
				res <- c(res, list(Line(outcrds#, proj4CRS
)))
			}
		}
	} else res <- list(obj)
	res
}

