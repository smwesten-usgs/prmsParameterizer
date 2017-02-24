#' Write PRMS header to a PRMS parameter file.
#'
#' @param filename Name of the PRMS parameter file to write to.
#'
#' @return None.
#' @export
write_prms_header <- function( filename ) {

  cat(file=filename,"Parameter file for PRMS/GSFLOW\n",sep="",append=FALSE)
  cat(file=filename,"Version: 1.7\n",sep="",append=TRUE)

}
