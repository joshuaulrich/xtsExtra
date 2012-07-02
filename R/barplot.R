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

barplot.xts <- function(height, stacked = TRUE, scale = FALSE, auto.legend = TRUE, ...) {
  # Don't like this name for input variable, 
  # but we must match S3 generic so we'll just change it
  x = height 
  
  # x should be organized as columns by category, rows by period
  # xts format assures us of this
  # 
  # stacked = TRUE is default, scale = FALSE scales percentages
  # Negatives are trickier to deal with reasonably so not yet supported
  
  if(scale){
    if(any(x < 0)) stop("Rescaling values for negative data not yet supported")
    x <- x/rowSums(x) # Recycling makes this work I'm pretty sure
  }
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  # Args from charts.StackedBar -- which of this go into signature and which from dots?
  colorset = NULL
  space = 0.2
  cex.axis=0.8
  cex.legend = 0.8
  cex.lab = 1
  cex.labels = 0.8
  cex.main = 1
  xaxis=TRUE
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
  
  if(nr == 1L){
    stop("Time-oriented barplot for single observation is not yet supported.")
    # This should dispatch to an unstacked plot, but those aren't working just yet
  }
    
  time.scale = periodicity(x)$scale
  ep = axTicksByTime(x, major.ticks, format.labels = date.format)
  ep1 = ep
  
  # Vectorize this?
  posn = barplot(coredata(x), plot=FALSE, space=space)
  if(!stacked) posn <- posn*nc
  for(i in 1:length(ep)) 
    ep1[i] = posn[ep[i]]
  
  
    
  if(is.null(colorset)) colorset <- seq_len(nc)
    
  minmargin <- if(is.null(xlab)) 3 else 5
    
  # multiple columns being passed into 'x', 
  
  # For now we only support under-legend
  # Set up two panels if needed
  if(auto.legend){
    layout(rbind(1,2), heights=c(6,1), widths=1)
    par(mar=c(3,4,4,2)+.1) # set the margins of the barplot panel
      # Note to self: mar= order is c(bottom, left, top, right)
  }
    
  # Much faster way to get positives and negatives than P Carl's method
  positives = x * (x > 0)
  negatives = x * (x < 0)
  
  # Set ylim to ends of stacked bars and to max/min if not stacked
  if(is.null(ylim)){
    if(stacked){
      ymax <- max(0, rowSums(positives)) # Faster than apply statement
      ymin <- min(0, rowSums(negatives)) # Use rowSums to stack by dates
      ylim <- c(ymin, ymax)
    } else{
      ymax <- max(0, positives)
      ymin <- min(0, negatives)
      ylim <- c(ymin, ymax)
    }
  }
    
  # Use barplot.default to actually draw the bars
  # t() drops xts-ness and returns a named matrix so dispatches properly
  if(stacked){
    barplot(t(positives), col=colorset, space=space, axisnames = FALSE, axes = FALSE, ylim=ylim, ...)
    barplot(t(negatives), add=TRUE , col=colorset, space=space, las = las, xlab = xlab, cex.names = cex.lab, axes = FALSE, axisnames = FALSE, ylim=ylim, ...)
  } else {
    barplot(t(x), beside = TRUE, col = colorset, axes = FALSE, axisnames = FALSE, ylim = ylim, ...)
  }
  
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
    
  if(auto.legend){ # For now, only supporting under-legend
    par(mar=c(0,2,0,1)+.1) # set the margins of the second panel
    plot.new()
    
    ncol = min(nc, 4)
    
    do_barplot.legend("center", legend=colnames(x), cex = cex.legend, fill=colorset, ncol=ncol, box.col=element.color, border.col = element.color)
  }
  invisible(height)
}

do_unstacked.barplot <- function(){
  
}

do_barplot.legend <- function (x, y = NULL, legend, fill = NULL, col = par("col"),
                               lty, lwd, pch, angle = 45, density = NULL, bty = "o", bg = par("bg"),
                               pt.bg = NA, cex = 1, pt.cex = cex, pt.lwd = lwd, xjust = 0,
                               yjust = 1, x.intersp = 1, y.intersp = 1, adj = c(0, 0.5),
                               text.width = NULL, text.col = par("col"), merge = do.lines &&
                                 has.pch, trace = FALSE, plot = TRUE, ncol = 1, horiz = FALSE,
                               title = NULL, inset = 0, border.col = NULL, border.lwd = 1, border.lty = "solid", box.col = NULL, box.lwd = 1, box.lty = "solid")
{
  # Modifications to core graphics legend() function
  # @author R Core Dev Team
  # @author modifications Peter Carl
  
  # Minor modifications to the function include:
  # - added border.col so that the legend border could be colored
  # - added border.lwd to change the line width of the border
  # - added border.lty to change the line type for the border
  # - changed line segment end to a more squared type
  
  # > plot.new()
  # > par(mar = c(0, 0, 0, 0))
  # > legend("center",text.col=rainbow6equal, cex = .8, ncol=3, border.col = "grey",legend = colnames(data))
  
  if (missing(legend) && !missing(y) && (is.character(y) ||
    is.expression(y))) {
    legend <- y
    y <- NULL
  }
  mfill <- !missing(fill) || !missing(density)
  if (length(title) > 1)
    stop("invalid title")
  n.leg <- if (is.call(legend))
    1
  else length(legend)
  if (n.leg == 0)
    stop("'legend' is of length 0")
  auto <- if (is.character(x))
    match.arg(x, c("bottomright", "bottom", "bottomleft",
                   "left", "topleft", "top", "topright", "right", "center"))
  else NA
  if (is.na(auto)) {
    xy <- xy.coords(x, y)
    x <- xy$x
    y <- xy$y
    nx <- length(x)
    if (nx < 1 || nx > 2)
      stop("invalid coordinate lengths")
  }
  else nx <- 0
  xlog <- par("xlog")
  ylog <- par("ylog")
  rect2 <- function(left, top, dx, dy, density = NULL, angle, border = border.col, lty = border.lty, lwd = border.lwd, ...) {
    r <- left + dx
    if (xlog) {
      left <- 10^left
      r <- 10^r
    }
    b <- top - dy
    if (ylog) {
      top <- 10^top
      b <- 10^b
    }
    rect(left, top, r, b, angle = angle, density = density, border = border, lty = lty, lwd = lwd, ...)
  }
  segments2 <- function(x1, y1, dx, dy, ...) {
    x2 <- x1 + dx
    if (xlog) {
      x1 <- 10^x1
      x2 <- 10^x2
    }
    y2 <- y1 + dy
    if (ylog) {
      y1 <- 10^y1
      y2 <- 10^y2
    }
    segments(x1, y1, x2, y2, lend="butt", ...) # added squared end to line seg
  }
  points2 <- function(x, y, ...) {
    if (xlog)
      x <- 10^x
    if (ylog)
      y <- 10^y
    points(x, y, ...)
  }
  text2 <- function(x, y, ...) {
    if (xlog)
      x <- 10^x
    if (ylog)
      y <- 10^y
    text(x, y, ...)
  }
  if (trace)
    catn <- function(...) do.call("cat", c(lapply(list(...),
                                                  formatC), list("\n")))
  cin <- par("cin")
  Cex <- cex * par("cex")
  if (is.null(text.width))
    text.width <- max(strwidth(legend, units = "user", cex = cex))
  else if (!is.numeric(text.width) || text.width < 0)
    stop("'text.width' must be numeric, >= 0")
  xc <- Cex * xinch(cin[1], warn.log = FALSE)
  yc <- Cex * yinch(cin[2], warn.log = FALSE)
  xchar <- xc
  xextra <- 0
  yextra <- yc * (y.intersp - 1)
  ymax <- max(yc, strheight(legend, units = "user", cex = cex))
  ychar <- yextra + ymax
  if (trace)
    catn("  xchar=", xchar, "; (yextra,ychar)=", c(yextra,
                                                   ychar))
  if (mfill) {
    xbox <- xc * 0.8
    ybox <- yc * 0.5
    dx.fill <- xbox
  }
  do.lines <- (!missing(lty) && (is.character(lty) || any(lty >
    0))) || !missing(lwd)
  n.legpercol <- if (horiz) {
    if (ncol != 1)
      warning("horizontal specification overrides: Number of columns := ",
              n.leg)
    ncol <- n.leg
    1
  }
  else ceiling(n.leg/ncol)
  if (has.pch <- !missing(pch) && length(pch) > 0) {
    if (is.character(pch) && !is.na(pch[1]) && nchar(pch[1],
                                                     type = "c") > 1) {
      if (length(pch) > 1)
        warning("not using pch[2..] since pch[1] has multiple chars")
      np <- nchar(pch[1], type = "c")
      pch <- substr(rep.int(pch[1], np), 1:np, 1:np)
    }
    if (!merge)
      dx.pch <- x.intersp/2 * xchar
  }
  x.off <- if (merge)
    -0.7
  else 0
  if (is.na(auto)) {
    if (xlog)
      x <- log10(x)
    if (ylog)
      y <- log10(y)
  }
  if (nx == 2) {
    x <- sort(x)
    y <- sort(y)
    left <- x[1]
    top <- y[2]
    w <- diff(x)
    h <- diff(y)
    w0 <- w/ncol
    x <- mean(x)
    y <- mean(y)
    if (missing(xjust))
      xjust <- 0.5
    if (missing(yjust))
      yjust <- 0.5
  }
  else {
    h <- (n.legpercol + (!is.null(title))) * ychar + yc
    w0 <- text.width + (x.intersp + 1) * xchar
    if (mfill)
      w0 <- w0 + dx.fill
    if (has.pch && !merge)
      w0 <- w0 + dx.pch
    if (do.lines)
      w0 <- w0 + (2 + x.off) * xchar
    w <- ncol * w0 + 0.5 * xchar
    if (!is.null(title) && (tw <- strwidth(title, units = "user",
                                           cex = cex) + 0.5 * xchar) > w) {
      xextra <- (tw - w)/2
      w <- tw
    }
    if (is.na(auto)) {
      left <- x - xjust * w
      top <- y + (1 - yjust) * h
    }
    else {
      usr <- par("usr")
      inset <- rep(inset, length.out = 2)
      insetx <- inset[1] * (usr[2] - usr[1])
      left <- switch(auto, bottomright = , topright = ,
                     right = usr[2] - w - insetx, bottomleft = , left = ,
                     topleft = usr[1] + insetx, bottom = , top = ,
                     center = (usr[1] + usr[2] - w)/2)
      insety <- inset[2] * (usr[4] - usr[3])
      top <- switch(auto, bottomright = , bottom = , bottomleft = usr[3] +
        h + insety, topleft = , top = , topright = usr[4] -
        insety, left = , right = , center = (usr[3] +
        usr[4] + h)/2)
    }
  }
  if (plot && bty != "n") {
    if (trace)
      catn("  rect2(", left, ",", top, ", w=", w, ", h=",
           h, ", ...)", sep = "")
    rect2(left, top, dx = w, dy = h, col = bg, density = NULL, border = border.col)#added border = border.col
  }
  xt <- left + xchar + xextra + (w0 * rep.int(0:(ncol - 1),
                                              rep.int(n.legpercol, ncol)))[1:n.leg]
  yt <- top - 0.5 * yextra - ymax - (rep.int(1:n.legpercol,
                                             ncol)[1:n.leg] - 1 + (!is.null(title))) * ychar
  if (mfill) {
    if (plot) {
      fill <- rep(fill, length.out = n.leg)
      rect2(left = xt, top = yt + ybox/2, dx = xbox, dy = ybox,
            col = fill, density = density, angle = angle,
            border = box.col) #removed internal border
    }
    xt <- xt + dx.fill
  }
  if (plot && (has.pch || do.lines))
    col <- rep(col, length.out = n.leg)
  if (missing(lwd))
    lwd <- par("lwd")
  if (do.lines) {
    seg.len <- 2
    if (missing(lty))
      lty <- 1
    lty <- rep(lty, length.out = n.leg)
    lwd <- rep(lwd, length.out = n.leg)
    ok.l <- !is.na(lty) & (is.character(lty) | lty > 0)
    if (trace)
      catn("  segments2(", xt[ok.l] + x.off * xchar, ",",
           yt[ok.l], ", dx=", seg.len * xchar, ", dy=0, ...)")
    if (plot)
      segments2(xt[ok.l] + x.off * xchar, yt[ok.l], dx = seg.len *
        xchar, dy = 0, lty = lty[ok.l], lwd = lwd[ok.l],
                col = col[ok.l])
    xt <- xt + (seg.len + x.off) * xchar
  }
  if (has.pch) {
    pch <- rep(pch, length.out = n.leg)
    pt.bg <- rep(pt.bg, length.out = n.leg)
    pt.cex <- rep(pt.cex, length.out = n.leg)
    pt.lwd <- rep(pt.lwd, length.out = n.leg)
    ok <- !is.na(pch) & (is.character(pch) | pch >= 0)
    x1 <- (if (merge)
      xt - (seg.len/2) * xchar
           else xt)[ok]
    y1 <- yt[ok]
    if (trace)
      catn("  points2(", x1, ",", y1, ", pch=", pch[ok],
           ", ...)")
    if (plot)
      points2(x1, y1, pch = pch[ok], col = col[ok], cex = pt.cex[ok],
              bg = pt.bg[ok], lwd = pt.lwd[ok])
    if (!merge)
      xt <- xt + dx.pch
  }
  xt <- xt + x.intersp * xchar
  if (plot) {
    if (!is.null(title))
      text2(left + w/2, top - ymax, labels = title, adj = c(0.5,
                                                            0), cex = cex, col = text.col)
    text2(xt, yt, labels = legend, adj = adj, cex = cex,
          col = text.col)
  }
  invisible(list(rect = list(w = w, h = h, left = left, top = top),
                 text = list(x = xt, y = yt)))
}
