#   xtsExtra: Extensions to xts during GSOC-2012
#
#   Copyright (C) 2012  Michael Weylandt: michael.weylandt@gmail.com
#
#   Barplot code inspired by chart.StackedBar in the 
#   PerformanceAnalytics Package -- Thanks to P. Carl
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.

barplot.xts <- function(height, stacked = TRUE, scale = FALSE, ...) {
  # Don't like this name for input variable, 
  # but we must match S3 generic so we'll just change it
  x = height 
  
  # x should be organized as columns by category, rows by period
  # xts format assures us of this
  # 
  # stacked = TRUE is default, scale = FALSE scales percentages
  # Negatives are trickier to deal with reasonably
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  # Args from charts.StackedBar -- which of this go into signature and which from dots?
  w = x 
  colorset = NULL
  space = 0.2
  cex.axis=0.8
  cex.legend = 0.8
  cex.lab = 1
  cex.labels = 0.8
  cex.main = 1
  xaxis=TRUE
  legend.loc="under"
  element.color = "darkgray"
  unstacked = TRUE
  xlab="Date"
  ylab="Value"
  ylim=NULL
  date.format = "%b %y"
  major.ticks='auto'
  minor.ticks=TRUE
  las = 0
  xaxis.labels = NULL
  
  nc = NCOL(x)
  nr = NROW(x)
    
  time.scale = periodicity(x)$scale
  ep = axTicksByTime(x, major.ticks, format.labels = date.format)
  ep1 = ep
  
  # Vectorize this?
  posn = barplot(coredata(x), plot=FALSE, space=space)
  for(i in 1:length(ep)) 
    ep1[i] = posn[ep[i]]
    
  if(is.null(colorset)) colorset <- seq_len(nc)
    
  minmargin <- if(is.null(xlab)) 3 else 5
    
  # multiple columns being passed into 'x', 
  # so we'll stack the bars and put a legend underneith
  if(!is.null(legend.loc) ){
    if(legend.loc =="under") {# put the legend under the chart
      layout(rbind(1,2), heights=c(6,1), widths=1)
      par(mar=c(3,4,4,2)+.1) # set the margins of the barplot panel
      # c(bottom, left, top, right)
    }
  }
    
  # Much faster way to get positives and negatives than P Carl's method
  positives = x * (x > 0)
  negatives = x * (x < 0)
  
  # Set ylim to ends of stacked bars
  if(is.null(ylim)){
    ymax=max(0,apply(positives,FUN=sum,MARGIN=1))
    ymin=min(0,apply(negatives,FUN=sum,MARGIN=1))
    ylim=c(ymin,ymax)
  }
    
  # Use barplot.default to actually draw the bars
  # t() drops xts-ness and returns a named matrix so dispatches properly
  barplot(t(positives), col=colorset, space=space, axisnames = FALSE, axes = FALSE, ylim=ylim, ...)
  barplot(t(negatives), add=TRUE , col=colorset, space=space, las = las, xlab = xlab, cex.names = cex.lab, axes = FALSE, axisnames = FALSE, ylim=ylim, ...)
  axis(2, col = element.color, las = las, cex.axis = cex.axis)
  title(ylab = ylab, cex = cex.lab)
  if (xaxis) {
    if(minor.ticks)
      axis(1, at=posn, labels=FALSE, col='#BBBBBB')
    label.height = .25 + cex.axis * apply(t(names(ep1)),1, function(X) max(strheight(X, units="in")/par('cin')[2]) )
    
    if(is.null(xaxis.labels))
      xaxis.labels = names(ep1)
    else
      ep1 = 1:length(xaxis.labels)
    
    axis(1, at=ep1, labels=xaxis.labels, las=las, lwd=1, mgp=c(3,label.height,0), cex.axis = cex.axis) 
    #axis(1, at = lab.ind, lab=rownames[lab.ind], cex.axis = cex.axis, col = elementcolor)
    #             title(xlab = xlab, cex = cex.lab)
    # use axis(..., las=3) for vertical labels.
    }
    box(col = element.color)
    
    if(!is.null(legend.loc)){
      if(legend.loc =="under"){ # draw the legend under the chart
        par(mar=c(0,2,0,1)+.1) # set the margins of the second panel
        plot.new()
        if(nc <4)
          ncol= nc
        else
          ncol = 4
        PerformanceAnalytics::legend("center", legend=colnames(x), cex = cex.legend, fill=colorset, ncol=ncol, box.col=element.color, border.col = element.color)
        par(op)
      } # if legend.loc is null, then do nothing
    }
  
  invisible(height)
}
  