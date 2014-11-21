library(functionMap)

sas.path <- system.file('examples','SAScode',package='functionMap')

net <- network.from.sascode(sas.path)

print(net)

net %v% 'toplevel'

## Using the plot for functionMap

plotFunctionMap(net)

## Using eForce

plot(eForce(net) + option(title=sprintf('Relation for %s', sas.path)))

## more pretty

propertyDf = data.frame(category = ifelse(net %v% 'toplevel', 'toplevel macros', 'inner macros'),
              value=ifelse(net %v% 'toplevel', 50, 10),
              color=ifelse(net %v% 'toplevel', 'yellow','green'))

plot(eForce(net[,], propertyDf, title=sprintf('Relation for %s', sas.path), title.x='right', title.y='bottom'))

## plot toplevel function in a pdf file

toplevel.sas.structure(sas.path, output.file=(fn<-tempfile(fileext='.pdf')))
shell(fn)

## low level function

parseSASfolder(sas.path)

## parseSASscript can return a data.frame, but a list of known names of user's macros need to be supplied, 
## or it searh all the SAS files under the working directory and use the filename as the user macros names.

parseSASscript(file.path(sas.path,'MainAnalysis.SAS'), network.vertex.names(net), output.format='data.frame')

