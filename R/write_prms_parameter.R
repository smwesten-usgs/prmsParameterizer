#' Write PRMS parameter information to a PRMS parameter file.
#'
#' This function is designed to be called once for each parameter written to the file.
#' For example it will be called once for the 'smidx_exp' parameters, again for the 'den_init'
#' parameters, etc.
#' @param filename Name of the PRMS parameter file to write to.
#'
#' @return None.
#' @export
write_prms_parameter<-function( filename  = "",
                                parname = "basin",
                                dimname1 = "one",
                                dimname2 = "",
                                type=2,
                                values,
                                dHeader = FALSE) {


  if(dHeader)  cat(file= filename ,"** Parameters **\n",sep="",append=TRUE)

  if(is.numeric(type)) {
    mytype <- c("long","float","double","character")[type]
  } else {
    mytype <- type
  }

  # we are assuming that there are never more than 2 dimensions in a PRMS data
  # element
  ifelse(nchar(dimname2)>0,numdim<-2,numdim<-1)

  cat(file= filename ,"####\n",sep="",append=TRUE)
  cat(file= filename ,parname," 10\n",sep="",append=TRUE)
  cat(file= filename ,numdim,"\n",sep="",append=TRUE)
  cat(file= filename ,dimname1,"\n",sep="",append=TRUE)

  if(numdim>1) {
    cat(file= filename ,dimname2,"\n",sep="",append=TRUE)
  }

  cat(file= filename ,length(values),"\n",sep="",append=TRUE)

  if(mytype=="long") {                                                # type 1: INTEGER
    cat(file= filename ,"1\n",sep="",append=TRUE)
    if(is.numeric(values)) {
      cat(file= filename ,sprintf(" %.0f\n",values),sep="",append=TRUE)
    } else {
      cat(file= filename ,sprintf(" %s\n",values),sep="",append=TRUE)
    }
  } else if(mytype=="float") {                                        # type 2: REAL*4
    cat(file= filename ,"2\n",sep="",append=TRUE)
    if(is.numeric(values)) {
      cat(file= filename ,sprintf(" %.8f\n",values),sep="",append=TRUE)
    } else {
      cat(file= filename ,sprintf(" %s\n",values),sep="",append=TRUE)
    }
  } else if(mytype=="double") {                                       # type 3: REAL*8
    cat(file= filename ,"3\n",sep="",append=TRUE)
    if(is.numeric(values)) {
      cat(file= filename ,sprintf(" %.12f\n",values),sep="",append=TRUE)
    } else {
      cat(file= filename ,sprintf(" %s\n",values),sep="",append=TRUE)
    }
  } else  {
      cat(file= filename ,"4\n",sep="",append=TRUE)
      ifelse(nchar(values)>0,
        cat(file= filename ,sprintf(" %s\n",values),sep="",append=TRUE),     # type 4: CHARACTER
          cat(file= filename ,"\n",sep="",append=TRUE))                       # type 4: CHARACTER
  }

}
