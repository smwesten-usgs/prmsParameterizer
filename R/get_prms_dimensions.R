#' Get PRMS dimension information from a PRMS 'par_name' file.
#'
#' The input file is the output from PRMS run with the `-print` option. This function will
#' not work with an ordinary PRMS parameter file.
#' @param filename Name of the PRMS 'par_name' file to munge.
#'
#' @return List containing the name, value, and description of each PRMS dimension.
#' @export
get_prms_dimensions <- function( filename ) {

library( utils )
library( gdata )

# slurp ENTIRE parameter file into memory
s1 <- scan(file= filename ,what=character(256),sep=":")

dim_idx    <- grep( " DIMENSIONS ", s1[ 1:length(s1) ], ignore.case=FALSE )
param_idx  <- grep( " PARAMETERS ", s1[ 1:length(s1) ], ignore.case=FALSE )

n_dims     <- length( grep( "Name", s1[ dim_idx:param_idx ] ) )

dims       <- data.frame(name=character(n_dims),
                         value=numeric(n_dims),
                         desc=character(n_dims))

dims$name  <- gdata::trim(s1[grep("Name",s1[dim_idx:param_idx]) + dim_idx])
dims$value <- s1[grep("Value",s1[dim_idx:param_idx]) + dim_idx]
dims$desc  <- gdata::trim(s1[grep("Desc",s1[dim_idx:param_idx]) + dim_idx])

return(list( s1=s1, dim_idx=dim_idx, param_idx=param_idx, n_dims=n_dims, dims=dims) )

}
