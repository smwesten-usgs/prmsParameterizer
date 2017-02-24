#' Get PRMS parameter information from a PRMS 'par_name' file.
#'
#' The input file is the output from PRMS run with the `-print` option. This function will
#' not work with an ordinary PRMS parameter file.
#' @param filename Name of the PRMS 'par_name' file to munge.
#'
#' @return List containing the name, value, description, default value, etc. of each PRMS parameter.
#' @export
get_prms_parameters <- function( filename ) {

  library( gdata )

  # slurp ENTIRE parameter file into memory
  s1<-scan( file=filename ,what=character(256),sep=":" )

  dim_idx<-grep(" DIMENSIONS ",s1[1:length(s1)],ignore.case=FALSE )
  param_idx<-grep(" PARAMETERS ",s1[1:length(s1)],ignore.case=FALSE )
  n_params<-length(grep("Name",s1[param_idx:length(s1)]))

  params<-data.frame(Name=character(n_params),
                     Module=character(n_params),
                     Desc=character(n_params),
                     Help=character(n_params),
                     Ndimen=numeric(n_params),
                     Dimensions=character(n_params),
                     Dimname_1=character(n_params),
                     Dimname_2=character(n_params),
                     Size=numeric(n_params),
                     Type=character(n_params),
                     Units=character(n_params),
                     Width=numeric(n_params),
                     Min=numeric(n_params),
                     Max=numeric(n_params),
                     Default=numeric(n_params),
                     stringsAsFactors=FALSE)

  params$Name<-gdata::trim(s1[grep("Name  ",s1[param_idx:length(s1)]) + param_idx])
  params$Module<-gdata::trim(s1[grep("Module",s1[param_idx:length(s1)]) + param_idx])
  params$Desc<-gdata::trim(s1[grep("Descr ",s1[param_idx:length(s1)]) + param_idx])
  params$Help<-gdata::trim(s1[grep("Help  ",s1[param_idx:length(s1)]) + param_idx])
  params$Ndimen<-as.numeric(s1[grep("Ndimen ",s1[param_idx:length(s1)]) + param_idx])
  params$Dimensions<-gdata::trim(s1[grep("Dimensions",s1[param_idx:length(s1)]) + param_idx])
  params$Size<-as.numeric(s1[grep("Size  ",s1[param_idx:length(s1)]) + param_idx])
  params$Type<-gdata::trim(s1[grep("Type   ",s1[param_idx:length(s1)]) + param_idx])
  params$Units<-gdata::trim(s1[grep("Units   ",s1[param_idx:length(s1)]) + param_idx])
  params$Width<-as.numeric(s1[grep("Width  ",s1[param_idx:length(s1)]) + param_idx])
  params$Min<-as.numeric(s1[grep("Min   ",s1[param_idx:length(s1)]) + param_idx])
  params$Max<-as.numeric(s1[grep("Max   ",s1[param_idx:length(s1)]) + param_idx])
  params$Default<-as.numeric(s1[grep("Default",s1[param_idx:length(s1)]) + param_idx])

  for(i in 1:n_params) {

    # split on comma
    dimcharc<-unlist(strsplit(params$Dimensions[i],","))

    if(params$Ndimen[i]==1) {
      dimchar<-strsplit(dimcharc,"-")
      params$Dimname_1[i]<-gdata::trim(unlist(dimchar)[1])
      params$Dimname_2[i]<-""

    } else if (params$Ndimen[i]==2) {
      dimchar<-strsplit(dimcharc[1],"-")
      params$Dimname_1[i]<-gdata::trim(unlist(dimchar)[1])
      dimchar<-strsplit(dimcharc[2],"-")
      params$Dimname_2[i]<-gdata::trim(unlist(dimchar)[1])

    }

  }

  return(params)

}
