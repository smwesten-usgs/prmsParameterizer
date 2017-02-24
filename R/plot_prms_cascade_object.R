plot_cascade_map<-function(fname="PRMS_Cascades.pdf", cascade.obj, hru.map,
     stream.map, nhru=NA, other.map=NA) {

  # set graphical parameters
  lwd.stream<-2
  cex.casc<-8
  cex.txt<-1.5
  n.colors<-35

  # calculate midpoint of cascade for labeling purposes
  cascade.obj$meanX<- (cascade.obj$fmX + cascade.obj$toX) /2.
  cascade.obj$meanY<- (cascade.obj$fmY + cascade.obj$toY) /2.

  pdf(fname,width=48,height=36)

  plot(as(hru.map,"Spatial"),axes=TRUE)
  if(any(hru.map$HRU_TYPE==1)) {
    old.palette<-palette()
    palette(rgb(runif(n.colors,0.0,0.5),runif(n.colors,0.7,1.0),runif(n.colors,0.0,0.3)))
    plot(hru.map[hru.map$HRU_TYPE==1,],add=TRUE,col=hru.map$GRIDCODE)
    palette(old.palette)
  }
  if(any(hru.map$num.connections==0 & hru.map$HRU_TYPE !=2)) {
    plot(hru.map[hru.map$num.connections==0 & hru.map$HRU_TYPE !=2,],add=TRUE,col="grey40",border="red")
  }
  if(any(hru.map$HRU_TYPE==3)) {
    plot(hru.map[hru.map$HRU_TYPE==3,],add=TRUE,col="wheat3")
  }
  if(any(hru.map$HRU_TYPE==2)) {      # plot lakes last since they end to be surrounded
    plot(hru.map[hru.map$HRU_TYPE==2,],add=TRUE,col="skyblue")
  }

  n.streams<-length(unique(stream.map$GRIDCODE))
  old.palette<-palette()
  palette(rainbow(n=n.streams))
  plot(stream.map,add=TRUE,col=stream.map$GRIDCODE,lwd=lwd.stream,border="grey30")
  palette(old.palette)

  if(class(other.map)[1]=="SpatialLinesDataFrame") {
#  if(exists("other.map")) {
    plot(other.map,add=TRUE,lty=2,lwd=5,col="purple")
  }

  agg<-aggregate(stream.map,by=list(stream.map$GRIDCODE),FUN=mean)
  
  casc<-subset(cascade.obj,cascade.obj$hrutargettype==4)   # connection is to stream segment
  n.type4 <- nrow(casc)
  if(nrow(casc)!=0) {
    arrows(casc$fmX,casc$fmY,casc$toX,casc$toY,col="blue",lwd=casc$hru_pct_up*cex.casc)
    text(x=casc$meanX,y=casc$meanY,labels=sprintf("%2.0f%%",100.*casc$hru_pct_up),col="white")
  }
  casc<-subset(cascade.obj,cascade.obj$hrutargettype==2)   # connection is to a lake
  n.type2 <- nrow(casc)
  if(nrow(casc)!=0) {
    arrows(casc$fmX,casc$fmY,casc$toX,casc$toY,col="cyan",lwd=casc$hru_pct_up*cex.casc)
    arrows(casc$fmX,casc$fmY,casc$toX,casc$toY,col="grey80",lwd=casc$hru_pct_up*cex.casc*0.1)
    text(x=casc$meanX,y=casc$meanY,labels=sprintf("%2.0f%%",100.*casc$hru_pct_up),col="white")
  }
  casc<-subset(cascade.obj,cascade.obj$hrutargettype==1)   # connection is to a HRU
  n.type1 <- nrow(casc)
  if(nrow(casc)!=0) {
    arrows(casc$fmX,casc$fmY,casc$toX,casc$toY,col="black",lwd=casc$hru_pct_up*cex.casc)
    text(x=casc$meanX,y=casc$meanY,labels=sprintf("%2.0f%%",100.*casc$hru_pct_up),col="white")
  }
  casc<-subset(cascade.obj,cascade.obj$hrutargettype==3)   # connection is to a swale
  n.type3 <- nrow(casc)
  if(nrow(casc)!=0) {
    arrows(casc$fmX,casc$fmY,casc$toX,casc$toY,col="wheat4",lwd=casc$hru_pct_up*cex.casc)
    text(x=casc$meanX,y=casc$meanY,labels=sprintf("%2.0f%%",100.*casc$hru_pct_up),col="white")
  }
  casc<-subset(cascade.obj,cascade.obj$hrutargettype < 0)# connection is to a farfield point
  n.typeff <- nrow(casc)
  if(nrow(casc)!=0) {
    arrows(casc$fmX,casc$fmY,casc$toX,casc$toY,col="red",lwd=casc$hru_pct_up*cex.casc)
    text(x=casc$meanX,y=casc$meanY,labels=sprintf("%2.0f%%",100.*casc$hru_pct_up),col="white")
  }

  mtext(paste(n.type2,"==> lake   ",n.type1,"==> hru   ",
         n.type3,"==> swale  ",n.type4,"==> stream  ",n.typeff,"==> farfield"),
         side=1,cex=cex.txt,col="red")

  # now add HRU text labels
  if(!missing(nhru)) {   # use unique XY values for HRUs (avoids duplication of labels)

    if(any(nhru$HRU_TYPE==1)) {
      text(nhru$X[nhru$HRU_TYPE==1],nhru$Y[nhru$HRU_TYPE==1],
         nhru$GRIDCODE[nhru$HRU_TYPE==1],cex=cex.txt,col="yellow")
    }
    if(any(nhru$HRU_TYPE==2)) {
    text(nhru$X[nhru$HRU_TYPE==2],nhru$Y[nhru$HRU_TYPE==2],
         nhru$GRIDCODE[nhru$HRU_TYPE==2],cex=cex.txt,col="dodgerblue3")
    }
    if(any(nhru$HRU_TYPE==3)) {
      text(nhru$X[nhru$HRU_TYPE==3],nhru$Y[nhru$HRU_TYPE==3],
         nhru$GRIDCODE[nhru$HRU_TYPE==3],cex=cex.txt,col="tan3")
    }
    
  } else {   # use the XY from the shapefile, could result in numerous duplicated labels

    if(any(hru.map$HRU_TYPE==1)) {
      text(hru.map$X[hru.map$HRU_TYPE==1],hru.map$Y[hru.map$HRU_TYPE==1],
         hru.map$GRIDCODE[hru.map$HRU_TYPE==1],cex=cex.txt,col="yellow")
    }
    if(any(hru.map$HRU_TYPE==2)) {
    text(hru.map$X[hru.map$HRU_TYPE==2],hru.map$Y[hru.map$HRU_TYPE==2],
         hru.map$GRIDCODE[hru.map$HRU_TYPE==2],cex=cex.txt,col="dodgerblue3")
    }
    if(any(hru.map$HRU_TYPE==3)) {
      text(hru.map$X[hru.map$HRU_TYPE==3],hru.map$Y[hru.map$HRU_TYPE==3],
         hru.map$GRIDCODE[hru.map$HRU_TYPE==3],cex=cex.txt,col="tan3")
    }

  }
  
  text(agg$X,agg$Y,agg$GRIDCODE,col="cyan",font=3,cex=cex.txt)

  dev.off()
  
  return()
}
