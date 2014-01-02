.onAttach <- function(libname, pkgname ){

	options(recharts.geoData.dir = chartr("\\", "/", system.file("data", package = "functionMap")))
	options(recharts.template.dir = chartr("\\", "/", system.file("template", package = "functionMap")))

	options(recharts.plot.tag=NULL)
	options(recharts.print.tag="html")
	
	recharts.tags <- c("type",  "chartid", "html", "header", "chart", "jsHeader", "jsData", "jsDrawChart", 
			"jsDisplayChart", "jsFooter", "jsChart", "divChart", "caption", "footer")
	options(recharts.tags = recharts.tags)
	
	jsLoaderFlag <<- FALSE

}



