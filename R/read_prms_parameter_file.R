#' Read PRMS parameter from a PRMS model parameter file.
#'
#' The input file is *NOT* the output from PRMS run with the `-print` option. This function will
#' *ONLY* work with an ordinary PRMS parameter file.
#' @param filename Name of the PRMS parameter file to munge.
#'
#' @return List containing the name, value, description, default value, etc. of each PRMS parameter.
#' @export
read_prms_parameter_file <- function( filename ) {
  # slurp ENTIRE parameter file into memory
  s1<-scan(file= filename ,what=character(256))

  # obtain the index values for the lines containing "Parameters" and
  # "nhru" tags
  param<-grep("Parameters",s1[1:length(s1)])
  nhru<-match("nhru",s1[1:max(param)])

  # find index values for PARAMETER NAMES
  namesIndex<-grep("####",s1[1:length(s1)]) + 1

  # redefine to include only indices that come *after* the first
  # occurrance of "Parameters" in the input file
  paramNamesIndex<-namesIndex[namesIndex > param]
  paramNames<-s1[paramNamesIndex]

  # break out the dimension names and values
  dimNamesIndex <- namesIndex[namesIndex < param]
  dimNames <- s1[dimNamesIndex]
  dimVals <- s1[dimNamesIndex + 1]

  # in cases where the parameter width has been left off, this
  # will subtract one from the index to point to the correct value
  offset<-ifelse(!is.na(as.numeric(s1[paramNamesIndex + 2])),0,-1)

  paramNumDimIndex<-paramNamesIndex + 2 + offset
  paramNumDim<-as.numeric(s1[paramNumDimIndex])

  paramDim1Index<-paramNamesIndex + 3 + offset
  paramDim1<-s1[paramDim1Index]

  paramDim2Index<-paramNamesIndex + 4 + offset
  paramDim2<-s1[paramDim2Index]

  #retroactively nuke all parameters that have only one dimension
  paramDim2Index[paramNumDim==1]<-NA
  paramDim2[paramNumDim==1]<-""

  paramLengthIndex<-paramNamesIndex + paramNumDim + 3 + offset
  paramLength<-as.numeric(s1[paramLengthIndex])

  paramTypeIndex<-paramNamesIndex + paramNumDim + 4 + offset
  paramType<-as.numeric(s1[paramTypeIndex])

  paramBeginIndex<-paramNamesIndex + paramNumDim + 5 + offset
  paramEndIndex<-paramBeginIndex + paramLength - 1
  alphaOrder<-order(paramDim1,paramDim2,paramNames)

   p <- list(
       s1 = s1,
       dimNames = dimNames,
       dimVals = dimVals,
       paramNames = paramNames,
       paramNumDim = paramNumDim,
       paramType = paramType,
       paramDim1 = paramDim1,
       paramDim2 = paramDim2,
       paramLength = paramLength,
       paramBeginIndex = paramBeginIndex,
       paramEndIndex =  paramEndIndex,
       alphaOrder = alphaOrder)

   return(p)

}
