#' Force directed network graph
#'
#' ECharts style Force network graph visulize the social network matrix data.
#'
#' When no \code{propertyDf} is provided, and no attributes can be used (either \code{use.network.attr} is \code{FALSE}
#' or the object lacking such attributes), the category is defined as the outer-degree (number of called functions) of the 
#' node. The value of node(what you see when your mouse is on the node) is the in-degree (called by how many functions).
#'
#' When \code{propertyDf} is provided, the \code{category} slot goes to category of the plot and \code{value} goes to value of the plot.
#'
#' When the object is a network object rather than a simple adjacency matrix, and the vertex has attributes of \code{category} and \code{value},
#' they will be used in the plot if \code{use.network.attr} is \code{TRUE}. If only \code{category} is available, but \code{value} is not set,
#' this function will use the out-degree (number of called functions) as the value (note this is different behavior to the default behavior where
#' out-degree is showed by category and in-degree is displayed as value).  Moreover, if the network object has edge attributes with name \code{weights}, 
#' this weights will also be shown in the plot.
#' 
#'
#' Another parameter which is useful and deserve explaination is \code{gravity}. When the network contain more than one connected components, 
#' and if gravity is too small, they will spread quite far away, those isolated points (by default is not displayed) may even fly away.
#' In this situation, user have to increase the gravity to make them close.
#' To the contrary, if the graph have too many nodes, the nodes may be very close to each other, too crowded to be seen, 
#' then a smaller gravity need to be set to stretch them up.
#'
#'
#' @param networkMatrix  either a network object or a adjacency matrix. If a matrix, any \code{NA} is treated as 0.
#' @param propertyDf   optional, data.frame defining the \code{category}, \code{value} and \code{color} for the vertices.
#' @param display.isolated decide whether display the isolated vertices or not
#' @param use.network.attr if \code{networkMatrix} is a network object and have corresponding attributes 'category', 'value' and 'color', and no \code{propertyDf} is specified, then use them to create a \code{propertyDf} to make the plot
#' @param size size of the output canvas, user may need to adjust this to their screen size
#' @param title title of the plot, if \code{NULL}, use the deparsed object name
#' @param subtitle subtitle, default is empty
#' @param title.x x position of title, by default we put the title to bottom right
#' @param title.y y position of title
#' @param minRadius the minimal radius of the node, if the node is too small to be seen, user can increase this value
#' @param maxRadius the maximal radius of the node, restrict the node not to be too large. Those relative size of node are according to their value.
#' @param scaling the scaling layout coefficient, if the node to too close together, user can increate the scaling to make the graph more sparse
#' @param do.depth.layout whether layout the graph using depth of nodes
#' @param depth.layout when \code{do.depth.layout} is \code{TRUE}, layout horizontal or vertical
#' @param do.jitter whether do jitter on the depth
#' @param legend whether to show legend for the category of vertex
#' @param legend.x x position of legend, default is top left; typically not shown if there is only one category
#' @param legend.y y position of legend
#' @param legend.orient orientation of legend, can be horizontal or vertical
#' @param toolbox show the magic toolbox panel, which allows user to switch between force and chord plot, and allow save the plot
#' @param toolbox.x x position of toolbox, default is 'top right'
#' @param toolbox.y y position of toolbox
#' @param tooltip whether show the floating tip for the node when mouse hovering on the node 
#' @param show.drawing.tool if show a drawing tool on the toolbox
#' @param auto.opt.large automatically tweak parameter for plotting large graph(>500 nodes)
#' @param gravity control how close those vertices are
#' @param roam Echart options, control how mouse can interactively change the plot, can be \code{TRUE}, 'move', 'scale'
#' @param opt other options which can be passed to ECharts.
#' @return recharts object which can be shown as HTML by \code{\link{plot.recharts}}
#' @export
#' @examples \dontrun{
#'      testData <- matrix(1:25, nrow=5)
#'      plot(eForce(testData))
#'
#'  # using a propertyDf
#'      net <- network.from.sascode(system.file('examples','SAScode',package='functionMap'))
#'       propertyDf = data.frame(category = ifelse(net %v% 'toplevel', 'toplevel macros', 'inner macros'), 
#'                       value=ifelse(net %v% 'toplevel', 50, 10), 
#'                       color=ifelse(net %v% 'toplevel', 'yellow','green'))
#'      plot(eForce(net[,], propertyDf, gravity=2))
#' }

eForce <- function(networkMatrix, propertyDf=NULL, size = c(1860, 930), display.isolated = FALSE, use.network.attr = FALSE,
	title = NULL, subtitle = NULL, title.x = "right", title.y = "bottom", minRadius = 15, maxRadius = 25, scaling = 1.1,
    do.depth.layout=FALSE, depth.layout = c('fixX', 'fixY'), do.jitter = FALSE,
	legend = TRUE, legend.x = "left", legend.y= "top", legend.orient=c("vertical", "horizontal"), 
	toolbox = TRUE, toolbox.x = "right", toolbox.y = "top", 
	tooltip = TRUE, show.drawing.tool=FALSE, auto.opt.large=TRUE,
    gravity = 1e-7, roam=TRUE,
    opt = list() ) {
    
    depth.layout <- match.arg(depth.layout)
	## networkMatrix would be a symmetric matrix
	## if the propertyDf is null, for the vertex, the category is outer degree(called how many functions), the value is the in degree of the node(called by)
	
	# option$title format.
	if (is.null(title)){
		title = gsub('(?<!\\\\)"','\\\\\"',paste(deparse(substitute(networkMatrix)), collapse=''), perl=TRUE)
        #title <- .HTML.escape(paste(deparse(substitute(networkMatrix)), collapse=''))
        if (nchar(title)>100) title = paste(substring(title, 1, 97), '...')
	}
	if (is.null(subtitle)){
		subtitle = ""
	}
    if (is.null(opt$series)) {
        opt$series <- list(gravity = gravity , roam = roam)
    } else {
        opt$series$gravity <- gravity
        opt$series$roam <- roam
    }
	opt$title = list(
		text = title,
		subtext = subtitle,
		x = .matchPos.x(title.x),
		y = .matchPos.y(title.y),
        textStyle = list(
            fontSize = 18,
            fontFamily = 'sans-serif',
            fontStyle =  'italic',
            fontWeight = 'lighter'
        )
	)

    if (use.network.attr && is.network(networkMatrix) && is.null(propertyDf)) {
        attr <- network::list.vertex.attributes(networkMatrix)
        if ('category' %in% attr) {
            propertyDf <- data.frame(category=networkMatrix %v% 'category', stringsAsFactors=FALSE)
            if ('value' %in% attr) {
                propertyDf$value = networkMatrix %v% 'value'
            } else {
                if (setequal(propertyDf$category, c('inpackage','outpackage','S3generic'))) {
                    propertyDf$value = c('inpackage'=30,'S3generic'=10,'outpackage'=1)[ propertyDf$category ]
                } else {
                    propertyDf$value = rowSums(networkMatrix[,]) 
                }
            }
            if ('color' %in% attr) {
                propertyDf$color = networkMatrix %v% 'color'
            }
            if (do.depth.layout) {
                if ('depth' %in% attr) {
                    propertyDf$depth <- networkMatrix %v% 'depth'
                } else {
                    propertyDf$depth <- topo.sort(networkMatrix)
                }
            }
        } else {
            cat('WARNING! not category attribute found for networkMatrix, use.network.attr=TRUE ignored!\n')
        }
        eattr <- network::list.edge.attributes(networkMatrix)
        if ('category' %in% eattr) {
            edge.category <- as.matrix.network.edgelist(networkMatrix, attrname='category')
            edge.category <- edge.category[ edge.category[,3]!='normal', , drop=FALSE ]
            if (NROW(edge.category)==0) {
                edge.category <- NULL
            }
        }
        if ('weights' %in% eattr) {
            elist <- as.matrix.network.edgelist(networkMatrix, attrname='weights')
            networkMatrix <- networkMatrix[,]
            for(i in 1:NROW(elist)) {
                networkMatrix[elist[i,1],elist[i,2]] <- elist[i,3]
            }
        }
    }


    if (!display.isolated) {
        out.deg <- rowSums(networkMatrix[,]>0)
        in.deg <- colSums(networkMatrix[,]>0)
        non.isolated <- which( out.deg!=0 | in.deg!=0 )
        if (length(non.isolated)==0) {
            cat('WARNING! All vertices are isolated, graph is totally un-connected! Please run with display.isolated=TRUE\n')
            return
        }
        networkMatrix <- networkMatrix[non.isolated,non.isolated]
        if (!is.null(propertyDf)) {
            propertyDf <- propertyDf[non.isolated,]
        }
    }

	
	
    opt$legend = list(
			show = isTRUE(legend), 
			x = .matchPos.x(legend.x),
			y = .matchPos.y(legend.y),
			orient =  match.arg(legend.orient)
    )
	
	# opt$tooltip format, not open to user now.
	if(tooltip){
		opt$tooltip = list(
			trigger = "item",
			# formatter = "{a} <br/>{b} : {c} ({d}%)"
            # according to Baidu API, this should be set to
            formater = ' {b} : {c} '
            # And Baidu's API is also misleading that, for edges, {c} is value rather than the weight
		)
	}
	
	# toolbox format
	opt$toolbox=list(
		show = isTRUE(toolbox),
		x = .matchPos.x(toolbox.x), 
		y = .matchPos.y(toolbox.y),
		feature = list(
			mark = list(show = isTRUE(show.drawing.tool), 
                        title = list(
                            mark = 'draw a line',
                            markUndo = 'erase last line',
                            markClear = 'clear all lines')
            ),
			restore = list(show= TRUE,
                        title = 'Restore'),
            magicType = list(show= TRUE, 
                             title=list(force='Force', chord='Chord'), 
                             type=c('force','chord')),
			saveAsImage = list(show= TRUE,
                               title = 'Save Image',
                               lang = 'Click to Save')
		)
	)

	### data format and data map.
	if(!is.null(propertyDf) && (nrow(propertyDf) != nrow(networkMatrix))){
		warning("data matrix doesn't have the same length to propertyDf. The propertyDf will be ignored.")
		propertyDf = NULL
	}
	
	networkMatrix <- as.matrix(networkMatrix)
	if (nrow(networkMatrix) != ncol(networkMatrix))  stop("networkMatrix have to be a square matrix")
	
	# matrix name check.
	if (is.null(colnames(networkMatrix))){
		if (is.null(rownames(networkMatrix))){
			if (is.null(propertyDf)){
				# if the rowname, colname and the propertyDf are missing, will use 1:nrow as names.
				rownames(networkMatrix) = 1:nrow(networkMatrix)
				colnames(networkMatrix) = 1:nrow(networkMatrix)
			}else{
				# if the propertyDf is not Null, the matrix name will use the propertyDf names.
				rownames(networkMatrix) = rownames(propertyDf)
				colnames(networkMatrix) = rownames(propertyDf)
			}
		}else{
			colnames(networkMatrix) = rownames(networkMatrix)
		}
	}
	
	if(!is.null(rownames(propertyDf))) rownames(propertyDf) = rownames(networkMatrix)
	
	# transfer the network Matrix to links items.
    # remove possible na
    networkMatrix[is.na(networkMatrix)] <- 0
    links <- which(networkMatrix>0, arr.ind = TRUE)
    linksOutput <- list(NROW(links),mode='list')
    for(i in 1:NROW(links)) {
        linksOutput[[i]] <- list(
            source = links[i,1] - 1,
            target = links[i,2] - 1,
            weight = networkMatrix[ links[i,1], links[i,2] ], 
            value = networkMatrix[ links[i,1], links[i,2] ] )
    }
    if (exists('edge.category') && !is.null(edge.category)) {
        ind <- match(paste(edge.category[,1], edge.category[,2], sep=','), paste(links[,1],links[,2], sep=','))
        for(i in seq_along(ind)) {
            linksOutput[[ ind[i] ]]$name <- edge.category[i,3]
        }
    }
	names(linksOutput) <- NULL
	
	# set the nodes property item.
	
	#set the default color array.
	.gg.color.hue <- function(n) {
		hues = seq(15, 375, length=n+1)
		hcl(h=hues, l=65, c=100)[1:n]
	}

	#If the propertyDf is null, will use category = out degree, value=in degree as default.
	if (is.null(propertyDf)){
        
        v.names <- colnames(networkMatrix)
        v.names <- gsub('(?<!\\\\)"','\\\\\"', v.names, perl=TRUE) # protect any " in the string
#        v.names <- .Fix.Echart.bug(v.names) 
# More elegent way is not use name, but should use label
        #v.names <- .HTML.escape(v.names)
        adjmat <- networkMatrix > 0

        in.degrees <- unname(colSums(adjmat))

        out.degrees <- unname(rowSums(adjmat))
        out.degrees <- factor(out.degrees, labels=paste('out deg', sort(unique(out.degrees))))
        out.degrees.code <- as.integer(out.degrees) - 1

        nodesOutput <- vector(NROW(adjmat), mode='list')
        for(i in 1:NROW(adjmat)) {
            nodesOutput[[ i ]] <- list(
                name = paste(' ', v.names[i], sep=''), # make sure name is not collide with js keyword
                label = v.names[i],          # label is what displayed
                category = out.degrees.code[i],
                value = in.degrees[i])
        }

        opt$legend$data <- as.list(levels(out.degrees))
        categoriesOutput <- sapply(levels(out.degrees), function(x) list(list('name'=x)), USE.NAMES=FALSE)

	}else{
		if(is.null(propertyDf$value)){
			# if the propertyDf has no column named value, the value will set to 0.
			propertyDf$value=0
		}
		if(is.null(propertyDf$color)){
			# if the propertyDf has no column named color, the color will be default to the .gg.color.hue class.
			# Also, the color will be .gg.color.hue(1) if the category column missed at the same time.
			if (is.null(propertyDf$category)){
				propertyDf$category = 0
				propertyDf$color = .gg.color.hue(1)
			}else{
				categoryList = unique(propertyDf$category)
				colArray = .gg.color.hue(length(categoryList))
				for(category in categoryList ){
					propertyDf[which(propertyDf$category == category), "color"] = colArray[which(categoryList == category)]
				}
			}
		}
		
		categoryList = unique(propertyDf$category)
        if (NROW(propertyDf) < NROW(networkMatrix)) {
            categoryList <- c(categoryList, '!undefined!')   
        }

        opt$legend$data = categoryList
        do.depth.layout <- do.depth.layout && !is.null(propertyDf$depth)
        if (do.depth.layout) {
            maxDepth <- max(propertyDf$depth)
            initial.xy <- if (depth.layout=='fixY') {
                depth.sep <- (size[2] - 20) / (maxDepth + 1)
                depth.sep.10 <- depth.sep/4
                function(depth) {
                    x <- round(runif(1, 0, size[1]-20))
                    y <-  depth.sep * depth + 20
                    if (do.jitter) y <- jitter(y, amount=depth.sep.10)
                    c(x,y) } 
                } else {
                depth.sep <- (size[1] - 20) / (maxDepth + 1)
                depth.sep.10 <- depth.sep/4
                function(depth) {
                    x <-  depth.sep * depth + 20
                    y <- round(runif(1, 0, size[2]-20))
                    if (do.jitter) x <- jitter(x, amount=depth.sep.10)
                    c(x,y) } 
                }
        }
			
        nms <- colnames(networkMatrix)
        ind <- match(nms, rownames(propertyDf))
        nodesOutput <- list()
        for(ii in seq_along(nms)) {
            nm <- gsub('(?<!\\\\)"', '\\\\\"', nms[ii], perl=TRUE)
            node <- list(name = paste(' ', nm , sep = ''), label = nm , value = 0, category = length(categoryList) - 1)
            if (do.depth.layout) {
                node$depth <- 1
                node$initial <- initial.xy(1)
                node$fixX <- depth.layout=='fixX'
                node$fixY <- depth.layout=='fixY'
            }
            if (!is.na(ind[ii])) {
                node$category <- match(propertyDf[ind[ii], 'category'], categoryList) - 1
                node$value <- propertyDf[ind[ii], 'value']
                if (do.depth.layout) {
                    node$depth <- propertyDf[ind[ii], 'depth']
                    node$initial <- initial.xy(node$depth)
                }
            }
            nodesOutput[[ii]] <- node
        }
#		
		categoriesOutput <- lapply(categoryList, function(category){
			return(
					list(
						name = category,
						itemStyle = list(
							normal = list(
								color = propertyDf[which(propertyDf$category == category),  "color"][1]
							)
						)
					)
				)
			}
		)
	}

    if(is.null(opt$series$linkSymbol)) {
        opt$series$linkSymbol = "arrow"
    }

	if(is.null(opt$series$type)) {
		opt$series$type = "force"
	}
	
    opt$series$minRadius = minRadius
    opt$series$maxRadius = maxRadius
    opt$series$scaling = scaling

#   No density now
#	if(is.null(opt$series$density)) {
#		opt$series$density = 0.05
#	}
		
#   renamed to gravity
#	if(is.null(opt$series$attractiveness)) {
#		opt$series$attractiveness = 1.2
#	}
		
	if(is.null(opt$series$itemStyle)) {
		opt$series$itemStyle <- list(
			normal = list(
				label = list(
					show = TRUE,
					textStyle = list(color="#333")
				),
				nodeStyle = list(
					brushType = "both",
					borderColor = "rgba(255,215,0,0.4)",
					borderWidth = 1 
				)
			),
			emphasis = list(
				label = list(
					show = TRUE
				),
				nodeStyle = list(
				    #	r = 30
				)
			)
		)
	}
	
    ## better style of magicType toolbox
    ## can alway restore to original style by click restore
    if (!is.null(opt$toolbox$feature$magicType)) {
        if (is.null(opt$toolbox$feature$magicType$option)) opt$toolbox$feature$magicType$option <- list()
        opt$toolbox$feature$magicType$option$chord <- list(minRadius=2, maxRadius=10, ribbonType=FALSE,
                                                           itemStyle = list(normal=list(label=list(show=TRUE,rotate=TRUE)),
                                                                            chordStyle=list(opacity=0.2)))
        opt$toolbox$feature$magicType$option$force <- list(minRadius=2, maxRadius=10, gravity= 1.1,
                                                           itemStyle = list(normal=list(label=list(show=FALSE)),
                                                                            linkStyle=list(opacity=0.5)))
    }

    if (auto.opt.large && NROW(networkMatrix)>=500) {
    # change style when graph is very large
        opt$series$steps <- 20
        opt$series$large <- TRUE
        opt$series$useWorker <- TRUE
        opt$legend$orient <- 'vertical'
        # opt$series$itemStyle$normal$label$show <- FALSE
        # opt$series$itemStyle$linkStyle$opacity <- 0.5
    }
    # directed chord graph
    opt$series$ribbonType <- FALSE
	
	opt$series$categories = categoriesOutput
	opt$series$nodes = nodesOutput
	opt$series$links = linksOutput
	opt$series = list(opt$series) # because json need a array rather than a map here

	jsonStr <- toJSON(opt, pretty=TRUE)
	outList <- .rechartsOutput(jsonStr, charttype="eForce", size=size)
	opt$size = size
	output <- list(outList=outList, opt=opt)
	class(output) <- c("recharts", "eForce", "list")
	
	### output list format
	return(output)
}

# Note: this is not a Echart bug. But which need caution!
# Echart when using the "name", and it equal to 'toString', then problem happends.
# This is not an legitimate name because it's a java script keyword. 
# This name will not be added to hashMap as 'toString' is always a function object for all javastrict object.
# We need to change the name a little, but appearance is still same
.Fix.Echart.bug <- function(v.names){
    if (length(ind<-which(v.names=='toString'))) {
        v.names[ind] <- paste(' ', v.names[ind], sep='')
    }
    v.names
}
