


data(edhec)
R <- edhec[,1:5]


chart.TimeSeries(R)

# The main title gets messed up when adding panels
plot2_xts(R)
x <- current.chob()
ls.str(x)
ls.str(x$Env)

addDrawdowns()
addDrawdowns()
x <- current.chob()
ls.str(x)
ls.str(x$Env)


chart.TimeSeries(R, auto.grid=FALSE)
plot2_xts(R, auto.grid=FALSE)


charts.TimeSeries(R)
plot2_xts(R, byColumn=TRUE)
title("Edhec Returns")

cl <- chartLayout(matrix(1:5), 1, c(2,2,1,1,1))
plot2_xts(R, byColumn=TRUE, layout=cl)
title("Edhec Returns")

x <- current.chob()
# Get the structure of the environments
ls.str(x)
ls.str(x$Env)


##### scratch area #####
# Should we have a theme object, as in quantmod, that sets all of the basic 
# parameters such as lty, lwd, las, cex, colorset, element.color, etc?

# chart specification (i.e. the xts chob)

# behaviors
# default (similar to chart.TimeSeries)
# small multiples
# panels
# chart specifications
# - specifications for common charts (e.g. charts.PerformanceSummary)

# what is he doing with frame and asp in chart_Series?
# what are the following variables used for
# frame
# asp
# clip

# http://www.lemnica.com/esotericR/Introducing-Closures/

