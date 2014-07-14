library(xtsExtra)
library(PerformanceAnalytics)


data(edhec)
R <- edhec[,1:2]

chart.TimeSeries(R)
plot2_xts(R)

charts.TimeSeries(R) 
# charts.TimeSeries messes up par("mar") so I need to call dev.off()
dev.off()
# the titles are gett
plot2_xts(R, byColumn=TRUE)

chart.Bar(R[,1])
plot2_xts(R[,1], type="h")

charts.Bar(R)
# charts.TimeSeries messes up par("mar") so I need to call dev.off() to reset
dev.off()
plot2_xts(R, byColumn=TRUE, type="h")

# Replicates charts.PerformanceSummary
plot2_xts(R, mainPanel=list(name="CumReturns"))
addReturns()
addDrawdowns()

plot2_xts(R)
addRollingPerformance()
addRollingPerformance(FUN="StdDev.annualized")
addRollingPerformance(FUN="SharpeRatio.annualized")


# The main title gets messed up when adding panels
# plot2_xts(R)
# x <- current.chob()
# ls.str(x)
# ls.str(x$Env)
# 
# addDrawdowns()
# addDrawdowns()
# x <- current.chob()
# ls.str(x)
# ls.str(x$Env)
# 
# 
# chart.TimeSeries(R, auto.grid=FALSE)
# plot2_xts(R, auto.grid=FALSE)
# 
# 
# charts.TimeSeries(R)
# plot2_xts(R, byColumn=TRUE)
# title("Edhec Returns")
# 
# cl <- chartLayout(matrix(1:5), 1, c(2,2,1,1,1))
# plot2_xts(R, byColumn=TRUE, layout=cl)
# title("Edhec Returns")
# 
# x <- current.chob()
# Get the structure of the environments
# ls.str(x)
# ls.str(x$Env)

# getSymbols("YHOO", src="yahoo")
# chart_Series(YHOO)
# add_RSI()
# add_MACD()

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

