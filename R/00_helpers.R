# helpers


# cutline -- slightly modified add.OCcutline function from 'oc' package to use with ggplot
cutline <- function(cutData) {

  slope <- -cutData[1]/cutData[2]
  if (is.na(slope)) {
    x <- c(cutData[1]*cutData[3],cutData[1]*cutData[3])
    y <- c(sqrt(1-(cutData[1]*cutData[3])^2),-sqrt(1-(cutData[1]*cutData[3])^2))
    slope <- NA
    intercept <- NA
  }
  else {
    intercept <- -slope*cutData[1]*cutData[3]+cutData[2]*cutData[3]
    x <- c( (-slope*intercept + sqrt( (slope*intercept)^2 -
                                        (1+slope*slope)*(intercept*intercept-1)))/(1+slope*slope),
            (-slope*intercept - sqrt( (slope*intercept)^2 -
                                        (1+slope*slope)*(intercept*intercept-1)))/(1+slope*slope) )
    if (is.na(x[1])) {
      warning("Couldn't solve for points on the unit circle!\n")
      x<-NA
      y<-NA
      slope<-NA
      intercept<-NA
    }
    else {
      y <- intercept + slope*x
      y[y < -1] <- -sqrt(1-x[y<1]^2)
      y[y >  1] <-  sqrt(1-x[y>1]^2)
    }
  }

  return(dplyr::bind_cols(x=x[1],xend=x[2],y=y[1],yend=y[2]))
}
