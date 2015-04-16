####################################################################################################
#   FUNCTION TO PLOT SCATTERPLOT WITH REGRESION, HISTOGRAM AND DENSITY PLOT IN THE SAME DEVICE
# source(http://www.r-bloggers.com/learning-r-using-a-chemical-reaction-engineering-book-part-1/)
#             modified by: C.Saavedra (camilo.saavedra@vi.ieo.es) 27/08/2013
####################################################################################################

scatterhist = function(x, y, xlab="", ylab=""){
  par.default <- par(no.readonly=TRUE)
  zones=matrix(c(2,0,1,3), ncol=2, byrow=TRUE)
  layout(zones, widths=c(4/5,1/5), heights=c(1/5,4/5))
  xhist = hist(x, plot=FALSE)
  yhist = hist(y, plot=FALSE)
  top = max(c(xhist$counts, yhist$counts))
  par(mar=c(5,5,1,1))
  plot(x,y, pch=1, cex=0.8, xlab=xlab, ylab=ylab)
  abline(v=quantile(x, na.rm=TRUE)[c(2,4)], lty=3)
  abline(h=quantile(y, na.rm=TRUE)[c(2,4)], lty=3)
  lmTot <- lm(y ~ x)
  abline(lmTot, col="black")
  par(mar=c(0,5,1,1))
  barplot(xhist$counts, axes=FALSE, ylim=c(0, top), space=0, col="gray70")
  par(mar=c(5,0,1,1))
  barplot(yhist$counts, axes=FALSE, xlim=c(0, top), space=0, horiz=TRUE, col="gray70")
  par(oma=c(0,0,2,0))
  mtext("dolphin length vs prey length (hake)", side=3, line=0.5, outer=TRUE, font=2)
  par=par(par.default)
}