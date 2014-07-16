library(xtsExtra)
library(PerformanceAnalytics)
source("sandbox/paFUN.R")

data(edhec)
R <- edhec[,1:4]

# basic plot with defaults
plot2_xts(R)

# assign to a variable and then print it results in a plot
x <- plot2_xts(R)
class(x)
x

# small multiples, line plot of each column
plot2_xts(R, byColumn=TRUE)

# bar chart of returns
plot2_xts(R[,1], type="h")

# bar chart of returns
# NOTE: only plots the first column of returns data
plot2_xts(R, type="h")

# small multiples, bar chart of each column
plot2_xts(R, byColumn=TRUE, type="h")

# Replicate charts.PerformanceSummary
plot2_xts(R, mainPanel=list(name="CumReturns"))
addReturns(type="h")
addDrawdowns()

# layout safe
# layout(matrix(1:4, 2, 2))
# for(i in 1:4) {plot(plot2_xts(R[,i], type="h"))}
# layout(matrix(1))

# Rolling performance
plot2_xts(R, mainPanel=list(name="CumReturns"))
addRollingPerformance()
addRollingPerformance(FUN="StdDev.annualized")
addRollingPerformance(FUN="SharpeRatio.annualized")


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

# http://www.lemnica.com/esotericR/Introducing-Closures/

