get_PRMS_GIS_varnames<-function(fname) {

  ## INPUT:   name of PRMS GIS file
  ## OUTPUT:  list with variable names

  # open file connection
  f <- file(fname, open="rt")

  var_txt <- character()

  num_vars <- 0
  inheader <- FALSE
  n <- 0

  repeat {

    n <- n + 1
    if(n>512) break()

    txt <- scan(f, what=character(),sep=",",nlines=1)
    cat(txt,"\n")

    if("# End DBF" %in% txt) {
      inheader <- FALSE
      break()
    }

    if(inheader==TRUE) {
      num_vars <- num_vars + 1
      var_txt[num_vars] <- gsub("# ","",txt[1])
    }

    if("# Begin DBF" %in% txt) {
      inheader <- TRUE
    }

  }

return(list(con=f,vars=var_txt))

}

read_PRMS_GIS_records<-function(con, hrus, vars) {

  ## INPUT:   name of PRMS GIS file, list of hrus desired
  ## OUTPUT:  data frame for a single simulation day, for selected HRUs

  if(!isOpen(con)) {
    stop("Connection must be open before calling this function. \n")
  }
  
  var_txt <- character()

  num_vars <- 0
  inheader <- FALSE
  n <- 0

  repeat {
  
    txt <- scan(f, what=character(),sep=",",nlines=1, comment.char="#")
    
    
    
  }
  
  return()
  
}
