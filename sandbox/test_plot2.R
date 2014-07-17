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

layout(matrix(1:2))
plot2_xts(R, byColumn=2, type="h")
layout(matrix(1))

plot2_xts(R[,1])

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


plot2_xts(R, mainPanel=list(name="CumReturns"),
          panels=c("addReturns(type='h')", "addDrawdowns()"))

layout(matrix(1:4, 2, 2))
plot2_xts(R, byColumn=1, mainPanel=list(name="CumReturns"),
          panels=c("addReturns(type='h')", "addDrawdowns()"))
layout(matrix(1))

# Replicate charts.Performance Summary in a 2x2 layout
# y-axis range here can be deceiving
layout(matrix(1:4, 2, 2))
for(i in 1:ncol(R)){
  p <- plot2_xts(R[,i], mainPanel=list(name="CumReturns"),
                 panels=c("addReturns(type='h')", "addDrawdowns()"),
                 name=colnames(R)[i])
  print(p)
}
layout(matrix(1))

# layout safe: loop over returns
layout(matrix(1:4, 2, 2))
for(i in 1:4) {plot(plot2_xts(R[,i], type="h"))}
layout(matrix(1))

# layout safe: easier to specify byColumn=1
# NOTE: y-axis matches even with multiple pages (i.e. graphics devices)
layout(matrix(1:4, 2, 2))
plot2_xts(R, byColumn=1, type="h")
layout(matrix(1))

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

