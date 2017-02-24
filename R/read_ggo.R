#' Read GSFLOW *.ggo file.
#'
#' @param filename Name of the GSFLOW *.ggo parameter file to munge.
#' @param start_date_str
#'
#' @return Calculate an observation date and return the observations stored in the 'ggo' file.
#' @export
read_ggo <- function( filename ,  start_date_str  = "01/01/1970") {

  ## INPUT:   name of MODFLOW GGO file
  ## OUTPUT:  list with variable names, and a data frame of all GGO data

  # establish the start date of the time series (i.e. date of first day of series)
   start_date<-as.Date( start_date_str ,format="%m/%d/%Y")

  # create empty list for OUTPUT
  v <-list()

  # open file connection
  ggo <- file( filename , open="rt")

  # read first line of GGO file
  l1<-readLines(ggo, n=1)
  # split first line; split on semicolons, colons, and quotes
  v$info<-strsplit(l1,"[\":;]")

  # read second line of GGO file
  l2<-readLines(ggo, n=1)
  # strip second line; split on spaces and quotes
  l2.parts<-strsplit(l2,"[ \"]")

  # select the non-null string parts of the vector
  v$header<-l2.parts[[1]][l2.parts[[1]]!=""]
  # nuke the first column of the vector
  v$header<-v$header[-1]

  # read in remainder of GGO file
  dat <- read.table(ggo)

  # name the columns
  colnames(dat)<-v$header

  # calculate a date field
  dat$date<-as.numeric(dat$Time) - 1 +  start_date
  dat$julday<-as.numeric(dat$date)

  # export the list, close file, and return
  close(ggo)
  v$dat<-dat

  return(v)

}  # end of function
