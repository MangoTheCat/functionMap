#' Force network graph
#'
#' ECharts style Force network graph visulize the social network matrix data.
#'
#' @param networkMatrix   required, a symmetric matrix, each cell value indicates 
#' the weight of the two nodes and the 0 or NA cell would not be counted in. 
#' The matrix should have colnames or rownames.
#' @param propertyDf   optional, data.frame which contain the metadata for the nodes. 
#' It could contain category, value and color columns. The colnames and rownames are required.
#' @param opt option of ECharts.
#' @return The HTML code as a character string.
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
#'      plot(eForce(net[,], propertyDf))
#' }

eForce = function(networkMatrix, propertyDf=NULL, size = c(1024, 768),
	title = NULL, subtitle = NULL, title.x = "center", title.y = "top", 
	legend = TRUE, legend.x = "left", legend.y= "top", legend.orient=c("horizontal", "vertical"), 
	toolbox = TRUE, toolbox.x = "right", toolbox.y = "top", readOnly = TRUE, mark=FALSE, showLabel=TRUE, 
	tooltip = TRUE, calculable=FALSE, xlab = NULL, ylab=NULL, opt = list() ) {
	## networkMatrix would be a symmetric matrix
	## if the propertyDf is null, all the category and value are 0 as default.
	
	# option$title format.
	if (is.null(title)){
		title=""
	}
	if (is.null(subtitle)){
		subtitle = ""
	}
	opt$title = list(
		text = title,
		subtext = subtitle,
		x = .matchPos.x(title.x),
		y = .matchPos.y(title.y)
	)
	
	
    opt$legend = list(
			show = isTRUE(legend), 
			x = .matchPos.x(legend.x),
			y = .matchPos.y(legend.y),
			orient =  match.arg(legend.orient)
    )
	
	opt$calculable = isTRUE(calculable) # ifelse(calculable, "true", "false") 

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
			mark = list(show = isTRUE(mark), 
                        title = list(
                            mark = 'draw line',
                            markUndo = 'undraw one line',
                            markClear = 'clear all draw')
            ),
			restore = list(show= TRUE,
                        title = 'Restore'),
            magicType = list(show= TRUE, 
                             title=list(force='Force', chord='Chard'), 
                             type=c('force','chord')),
			saveAsImage = list(show= TRUE,
                               title = 'Save Image',
                               lang = 'Click to Save')
		)
	)

	### data format and data map.
	if(!is.null(propertyDf) && (nrow(propertyDf) != nrow(networkMatrix))){
		warning("dat matrix doesn't have the same length to propertyDf. The propertyDf will be ignored.")
		propertyDf = NULL
	}

	
	networkMatrix <- as.matrix(networkMatrix)
	if (nrow(networkMatrix) != ncol(networkMatrix))  stop("networkMatrix would be a symmetric matrix")
    if (any(t(networkMatrix) != networkMatrix)) {
        warning('networkMatrix is not symmetric, we force it symmetric by t(M) + M')
        networkMatrix = networkMatrix + t(networkMatrix)
    }
	
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
	networkMatrix[!lower.tri(networkMatrix)] <- NA
	networkMatrix[networkMatrix==0] <- NA
	validNode <- as.data.frame(t(which(!is.na(networkMatrix), arr.ind=TRUE)))
	linksOutput <- lapply(validNode, FUN=function(nodeIndex){
		return(
			list(
				source = nodeIndex[1] - 1,
				target = nodeIndex[2] - 1,
				weight = networkMatrix[nodeIndex[1], nodeIndex[2]],
                value  = networkMatrix[nodeIndex[1], nodeIndex[2]] # this is for show tooltip
			)
		)
	})
	
	names(linksOutput) <- NULL
	
	# set the nodes property item.
	
	#set the default color array.
	.gg.color.hue <- function(n) {
		hues = seq(15, 375, length=n+1)
		hcl(h=hues, l=65, c=100)[1:n]
	}

	#If the propertyDf is null, will use category = 0, value=0 as default.
	if (is.null(propertyDf)){
		nodesOutput <- lapply(colnames(networkMatrix), FUN = function(nodeName){
			return(
				list(
					category = 0,
					name = nodeName,
					value = 0
				)
			)
		})
		
		categoriesOutput <- list(list(
			name = "Default",
			itemStyle = list(
				normal = list(
					color = .gg.color.hue(1)
				)
			)
		))
        opt$legend$data = list("Default")
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

        opt$legend$data = categoryList
			
		nodesOutput <- lapply(colnames(networkMatrix), FUN = function(nodeName){
			indexOfDf = which(rownames(propertyDf) == nodeName)[1]
			if(is.na (indexOfDf)){
				return(
					list(
						category = 0,
						name = nodeName,
						value = 0
					)
				)
			}else{
				return(
					list(
						category = which(categoryList == propertyDf[indexOfDf, "category"]) - 1,
						name = nodeName,
						value = propertyDf[indexOfDf, "value"]
					)
				)
			}
		})
		
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

	if(is.null(opt$series$type)) {
		opt$series$type = "force"
	}
	
	if(is.null(opt$series$minRadius)) {
		opt$series$minRadius = 15
	}
		
	if(is.null(opt$series$maxRadius)) {
		opt$series$maxRadius = 25
	}

	if(is.null(opt$series$density)) {
		opt$series$density = 0.05
	}
		
	if(is.null(opt$series$attractiveness)) {
		opt$series$attractiveness = 1.2
	}
		
	if(is.null(opt$series$itemStyle)) {
		itemStyleOutput = list(
			normal = list(
				label = list(
					show = "true",
					textStyle = list(color="#800080")
				),
				nodeStyle = list(
					brushType = "both",
					strokeColor = "rgba(255,215,0,0.4)",
					lineWidth = 8
				)
			),
			emphasis = list(
				label = list(
					show = "true"
				),
				nodeStyle = list(
					r = 30
				)
			)
		)
	}
	
	
	opt$series$itemStyle = itemStyleOutput
	opt$series$categories = categoriesOutput
	opt$series$nodes = nodesOutput
	opt$series$links = linksOutput
	opt$series = list(opt$series)

	jsonStr <- toJSON(opt, pretty=TRUE)
	outList <- .rechartsOutput(jsonStr, charttype="eForce", size=size)
	opt$size = size
	output <- list(outList=outList, opt=opt)
	class(output) <- c("recharts", "eForce", "list")
	
	### output list format
	return(output)
}
