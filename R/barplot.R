#   xtsExtra: Extensions to xts during GSOC-2012
#
#   Copyright (C) 2012  Michael Weylandt: michael.weylandt@gmail.com
#
#   Barplot code inspired by chart.StackedBar in the PerformanceAnalytics Package 
#   Thanks to B. Peterson & P. Carl
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
  .NotYetImplemented()
  return(invisible(height))
  
  function (w, colorset = NULL, space = 0.2, cex.axis=0.8, cex.legend = 0.8, cex.lab = 1, cex.labels = 0.8, cex.main = 1, xaxis=TRUE, legend.loc="under",  element.color = "darkgray", unstacked = TRUE, xlab="Date", ylab="Value", ylim=NULL, date.format = "%b %y", major.ticks='auto', minor.ticks=TRUE, las = 0, xaxis.labels = NULL, ... ) 
  {
    # Data should be organized as columns for each category, rows for each period or observation
    
    # @todo: Set axis color to element.color
    # @todo: Set border color to element.color
    
    w.columns = ncol(w)
    w.rows = nrow(w)
    
    time.scale = periodicity(w)$scale
    ep = axTicksByTime(w, major.ticks, format.labels = date.format)
    ep1 = ep
    posn = barplot(w, plot=FALSE, space=space)
    for(i in 1:length(ep)) 
      ep1[i] = posn[ep[i]]
    
    if(is.null(colorset))
      colorset=1:w.columns
    
    if(is.null(xlab))
      minmargin = 3
    else
      minmargin = 5
    
    # multiple columns being passed into 'w', so we'll stack the bars and put a legend underneith
    if(!is.null(legend.loc) ){
      if(legend.loc =="under") {# put the legend under the chart
        op <- par(no.readonly=TRUE)
        layout(rbind(1,2), heights=c(6,1), widths=1)
        par(mar=c(3,4,4,2)+.1) # set the margins of the first panel
        # c(bottom, left, top, right)
      }
      #             else
      #                 par(mar=c(5,4,4,2)+.1) # @todo: this area may be used for other locations later
    }
    
    # Brute force solution for plotting negative values in the bar charts:
    positives = w
    for(column in 1:ncol(w)){
      for(row in 1:nrow(w)){ 
        positives[row,column]=max(0,w[row,column])
      }
    }
    
    negatives = w
    for(column in 1:ncol(w)){
      for(row in 1:nrow(w)){ 
        negatives[row,column]=min(0,w[row,column])
      }
    }
    # Set ylim accordingly
    if(is.null(ylim)){
      ymax=max(0,apply(positives,FUN=sum,MARGIN=1))
      ymin=min(0,apply(negatives,FUN=sum,MARGIN=1))
      ylim=c(ymin,ymax)
    }
    
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
        if(w.columns <4)
          ncol= w.columns
        else
          ncol = 4
        legend("center", legend=colnames(w), cex = cex.legend, fill=colorset, ncol=ncol, box.col=element.color, border.col = element.color)
        par(op)
      } # if legend.loc is null, then do nothing
    }
    #     par(op)
  }
  
}
  