


data(edhec)
R <- edhec[,1:5]


chart.TimeSeries(R)
plot2_xts(R)

chart.TimeSeries(R, auto.grid=FALSE)
plot2_xts(R, auto.grid=FALSE)

chart.TimeSeries(R, minor.ticks=FALSE)
plot2_xts(R, minor.ticks=FALSE)


plot2_xts(R, byColumn=TRUE)
title("Edhec Returns")

charts.TimeSeries(R)

x <- current.chob()
# Get the structure of the environments
ls.str(x)
ls.str(x$Env)

##### scratch area #####
# Should we have a theme object that sets all of the basic parameters such
# as lty, lwd, las, cex, colorset, element.color, etc?

# chart specification (i.e. the xts chob)

# behaviors
# default (similar to chart.TimeSeries)
# small multiples
# panels
# chart specifications
# - specifications for common charts (e.g. charts.PerformanceSummary)

# what is he doing with frame and asp in chart_Series?
# what are the following variables used ofr
# frame
# asp
# clip

# http://www.lemnica.com/esotericR/Introducing-Closures/

