#' Write PRMS dimension information to a PRMS parameter file.
#'
#' This function is designed to be called once for each dimension written to the file.
#' For example it will be called once for the 'nmonth' dimension, again for the 'ntemp'
#' dimension, etc.
#' @param filename Name of the PRMS parameter file to write to.
#'
#' @return None.
#' @export
write_prms_dimension<-function(fname = "",
                               dimname = "one",
                               dimlength = 1,
                               dHeader = FALSE) {

  if(dHeader)  cat(file=fname,"** Dimensions **\n",sep="",append=TRUE)

  cat(file=fname,"####\n",sep="",append=TRUE)
  cat(file=fname,dimname,"\n",sep="",append=TRUE)
  cat(file=fname,dimlength,"\n",sep="",append=TRUE)

}
