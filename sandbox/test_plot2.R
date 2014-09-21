library(xtsExtra)
library(PerformanceAnalytics)
library(quantmod)
source("sandbox/paFUN.R")

data(edhec)
R <- edhec[,1:4]

# basic plot with defaults
plot(R)

# assign to a variable and then print it results in a plot
x <- plot(R)
y <- addReturns()
x
class(x)
y

# small multiples, line plot of each column
plot(R, multi.panel=TRUE)
plot(R, multi.panel=TRUE, yaxis.same=FALSE)

layout(matrix(1:2))
plot(R, multi.panel=2, type="h")
layout(matrix(1))

plot(R[,1])

# bar chart of returns
plot(R[,1], type="h")

# bar chart of returns
# NOTE: only plots the first column of returns data
plot(R, type="h")

# small multiples, bar chart of each column
plot(R, multi.panel=TRUE, type="h")

# Replicate charts.PerformanceSummary
plot(R, FUN=CumReturns)
addReturns(type="h")
addDrawdowns()
xtsExtra::addLines(c("1999-01-01", "2000-01-01", "2005-01-01"), c("foo", "bar", "pizza"), on=1:3)
xtsExtra::addLines(c("1999-01-01", "2000-01-01", "2005-01-01"))


plot(R, FUN="CumReturns",
          panels=c("addReturns(type='h')", "addDrawdowns()"))

R <- edhec[,1:8]
layout(matrix(1:4, 2, 2))
plot(R, multi.panel=2, FUN="CumReturns",
          panels=c("addReturns(type='h')", "addDrawdowns()"))
layout(matrix(1))

# Replicate charts.Performance Summary in a 2x2 layout
# y-axis range here can be deceiving
layout(matrix(1:4, 2, 2))
for(i in 1:ncol(R)){
  p <- plot(R[,i], FUN="CumReturns",
                 panels=c("addReturns(type='h')", "addDrawdowns()"),
                 name=colnames(R)[i])
  print(p)
}
layout(matrix(1))

# layout safe: loop over returns
layout(matrix(1:4, 2, 2))
for(i in 1:4) {plot(plot(R[,i], type="h"))}
layout(matrix(1))

# layout safe: easier to specify multi.panel=1
# NOTE: y-axis matches even with multiple pages (i.e. graphics devices)
layout(matrix(1:4, 2, 2))
plot(R, multi.panel=1, type="h")
layout(matrix(1))

# Rolling performance
plot(R, FUN="CumReturns", geometric=FALSE)
plot(R, FUN="CumReturns", geometric=TRUE, wealth.index=TRUE)
addRollingPerformance()
addRollingPerformance(FUN="StdDev.annualized")
addRollingPerformance(FUN="SharpeRatio.annualized")

x <- xtsExtra:::current.xts_chob()
x$Env$call_list
x$Env$call_list[[1]]

R <- edhec[,1:4]
plot(R, FUN="CumReturns")
plot(R, FUN="CumReturns", lty=1:4)
plot(R, FUN="CumReturns", lty=1:4, lwd=c(3, 1, 1, 1))
plot(R, FUN="CumReturns", lwd=c(3, 2, 2, 2), colorset=c(1, rep("gray", 3)))

plot(R, yaxis.left=TRUE, yaxis.right=FALSE)
plot(R, grid.ticks.lwd=1, grid.ticks.lty="solid", grid.col="black")

# examples with legend functionality
R <- edhec[,1:10]
foo <- function(x){
  CumReturns(R = x)
}
plot(R, FUN=foo)
addLegend(ncol = 4)
addLegend(legend.names = c("foo", "bar"), colorset = c(1,2), ncol=2)

plot(R, FUN=foo, legend.loc="topleft")
plot(R, FUN=foo, legend.loc="left")
plot(R, FUN=foo, legend.loc="bottomleft")

plot(R, FUN=foo, legend.loc="top")
plot(R, FUN=foo, legend.loc="center")
plot(R, FUN=foo, legend.loc="bottom")

plot(R, FUN=foo, legend.loc="topright")
plot(R, FUN=foo, legend.loc="right")
plot(R, FUN=foo, legend.loc="bottomright")


plot(R, FUN=foo)
addSeries(R[,1])

plot(R, FUN="CumReturns")
addSeries(R[,1], type="h")

plot(R, FUN="CumReturns")
tmp1 <- tmp2 <- R[,1]
tmp1[,1] <- 1.5

tmp2[,1] <- 1

tmp <- CumReturns(R[,1])
tmp3 <- tmp[seq(from=1, to=NROW(R), by=10),]

addSeries(tmp1, on=1)
addSeries(tmp2, on=1, type="p", pch=5)
addSeries(tmp3, on=1, type="p", pch=2)


stock.str='AAPL'
initDate="2011-01-01" 
endDate="2012-12-31"   
getSymbols(stock.str,from=initDate,to=endDate, src="yahoo")
plot(Ad(AAPL))
addSeries(Ad(AAPL)["2012-05-28/"]-10, on=1, col = "red")
xtsExtra::addLines(c("2011-11-04", "2012-11-10", "2012-05-28"), on=1)
xtsExtra::addLines(c("2011-03-04", "2012-01-10", "2012-07-28"), on=1)
xtsExtra::addLines(c("2011-11-04", "2012-11-10", "2012-05-28"))

aapl <- Ad(AAPL)
plot(aapl)
aapl["2011-07/2012-07"] <- NA
plot(aapl)

# png("~/Documents/foo.png")
# plot(R, FUN="CumReturns")
# addDrawdowns()
# dev.off()

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

