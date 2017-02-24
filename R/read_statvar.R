#' Read model output from a 'statvar' file.
#'
#' @param  filename Name of a PRMS model 'statvar' file.
#'
#' @return List containing variable names, unit numbers (HRU numbers etc.), and a dataframe
#'         containing all statvar file output.
#' @export
#'
#' @examples
read_statvar<-function( filename ) {

  # open file connection
  statvar <- file( filename , open="rt")

  num.vars <- as.numeric(readLines(statvar, n=1))
     # first item in a statvar file tells us
     # how many variables (fields) have been
     # written in the statvar file

  # create empty variables
  v <-list()
  name<-character(num.vars)
  unit<-numeric(num.vars)

  # loop over all named statvar variables
  for (i in 1:num.vars) {
    txt <- scan(statvar,what=character(0),nlines=1, quiet=TRUE)
    name[i]<-txt[1]
    unit[i]<-as.numeric(txt[2])
  } # end loop over variable names

  # paste the name and unit number together
  cnames<-paste(name,as.character(unit),sep="_")

  # read in remainder of statvar file
  dat <- read.table(statvar)

  # create date string
  dts<-paste(dat[,3],dat[,4],dat[,2],sep="/")

  # create chron object
  dat$date<-as.Date(dts, "%m/%d/%Y")
  dat$julday<-as.numeric(dat$date)
  dat$date<-as.Date(dat$date,format="%m/%d/%Y")
   # delete first useless column
  dat<-dat[,-1]

  # name the columns
  colnames(dat)<-c("year","month","day","hh","mm","ss",cnames,"date","julday")

  v$name<-name
  v$unit<-unit
  v$data.df<-dat

  # export the list, close file, and return
  close(statvar)

  return(v)

}  # end of function
