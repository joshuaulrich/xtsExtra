library(xtsExtra)
library(PerformanceAnalytics)
library(quantmod)
source("sandbox/paFUN.R")

data(edhec)
R <- edhec[,1:4]

# basic plot with defaults
plot2_xts(R)

# assign to a variable and then print it results in a plot
x <- plot2_xts(R)
y <- addReturns()
x
class(x)
y

# small multiples, line plot of each column
plot2_xts(R, multi.panel=TRUE)
plot2_xts(R, multi.panel=TRUE, yaxis.same=FALSE)

layout(matrix(1:2))
plot2_xts(R, multi.panel=2, type="h")
layout(matrix(1))

plot2_xts(R[,1])

# bar chart of returns
plot2_xts(R[,1], type="h")

# bar chart of returns
# NOTE: only plots the first column of returns data
plot2_xts(R, type="h")

# small multiples, bar chart of each column
plot2_xts(R, multi.panel=TRUE, type="h")

# Replicate charts.PerformanceSummary
plot2_xts(R, FUN=CumReturns)
addReturns(type="h")
addDrawdowns()


plot2_xts(R, FUN="CumReturns",
          panels=c("addReturns(type='h')", "addDrawdowns()"))

R <- edhec[,1:8]
layout(matrix(1:4, 2, 2))
plot2_xts(R, multi.panel=2, FUN="CumReturns",
          panels=c("addReturns(type='h')", "addDrawdowns()"))
layout(matrix(1))

# Replicate charts.Performance Summary in a 2x2 layout
# y-axis range here can be deceiving
layout(matrix(1:4, 2, 2))
for(i in 1:ncol(R)){
  p <- plot2_xts(R[,i], FUN="CumReturns",
                 panels=c("addReturns(type='h')", "addDrawdowns()"),
                 name=colnames(R)[i])
  print(p)
}
layout(matrix(1))

# layout safe: loop over returns
layout(matrix(1:4, 2, 2))
for(i in 1:4) {plot(plot2_xts(R[,i], type="h"))}
layout(matrix(1))

# layout safe: easier to specify multi.panel=1
# NOTE: y-axis matches even with multiple pages (i.e. graphics devices)
layout(matrix(1:4, 2, 2))
plot2_xts(R, multi.panel=1, type="h")
layout(matrix(1))

# Rolling performance
plot2_xts(R, FUN="CumReturns", geometric=FALSE)
plot2_xts(R, FUN="CumReturns", geometric=TRUE, wealth.index=TRUE)
addRollingPerformance()
addRollingPerformance(FUN="StdDev.annualized")
addRollingPerformance(FUN="SharpeRatio.annualized")

x <- xtsExtra:::current.xts_chob()
x$Env$call_list
x$Env$call_list[[1]]

R <- edhec[,1:4]
plot2_xts(R, FUN="CumReturns")
plot2_xts(R, FUN="CumReturns", lty=1:4)
plot2_xts(R, FUN="CumReturns", lty=1:4, lwd=c(3, 1, 1, 1))
plot2_xts(R, FUN="CumReturns", lwd=c(3, 2, 2, 2), colorset=c(1, rep("gray", 3)))

plot2_xts(R, yaxis.left=TRUE, yaxis.right=FALSE)
plot2_xts(R, grid.ticks.lwd=1, grid.ticks.lty="solid", grid.col="black")

# examples with legend functionality
R <- edhec[,1:10]
foo <- function(x){
  CumReturns(R = x)
}
plot2_xts(R, FUN=foo)
addLegend(ncol = 4)

plot2_xts(R, FUN=foo, legend.loc="topleft")
plot2_xts(R, FUN=foo, legend.loc="left")
plot2_xts(R, FUN=foo, legend.loc="bottomleft")

plot2_xts(R, FUN=foo, legend.loc="top")
plot2_xts(R, FUN=foo, legend.loc="center")
plot2_xts(R, FUN=foo, legend.loc="bottom")

plot2_xts(R, FUN=foo, legend.loc="topright")
plot2_xts(R, FUN=foo, legend.loc="right")
plot2_xts(R, FUN=foo, legend.loc="bottomright")


plot2_xts(R, FUN=foo)
xtsExtra:::addLines2(R[,1])

plot2_xts(R, FUN="CumReturns")
addLines2(R[,1], type="h")

plot2_xts(R, FUN="CumReturns")
tmp1 <- tmp2 <- R[,1]
tmp1[,1] <- 1.5

tmp2[,1] <- 1

tmp <- CumReturns(R[,1])
tmp3 <- tmp[seq(from=1, to=NROW(R), by=10),]

addLines2(tmp1, on=1)
addLines2(tmp2, on=1, type="p", pch=5)
addLines2(tmp3, on=1, type="p", pch=2)


png("~/Documents/foo.png")
plot2_xts(R, FUN="CumReturns")
addDrawdowns()
dev.off()

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

