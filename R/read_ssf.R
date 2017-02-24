#' Read PEST/TSPROC *.ssf file.
#'
#' @param filename Name of the PEST/TSPROC *.ssf file to munge.
#' @param start_date_str
#'
#' @return Return the observations stored in the 'ssf' file.
#' @export
read_ssf<-function( filename ) {

  v<-list()

  v$info<-paste("Values imported from", filename )
  v$header<-c("Site","Date","Time","Value")

  dat<-read.table( filename ,colClasses=c("character","character",
     "character","numeric"), header=F)
  colnames(dat)<-c("site","date","time","value")

  dat$date<-as.Date(dat$date,format="%m/%d/%Y")
  dat$julday<-as.numeric(dat$date)
  v$dat<-dat

  return(v)

}
