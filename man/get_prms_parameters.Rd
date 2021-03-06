% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_prms_parameters.R
\name{get_prms_parameters}
\alias{get_prms_parameters}
\title{Get PRMS parameter information from a PRMS 'par_name' file.}
\usage{
get_prms_parameters(filename)
}
\arguments{
\item{filename}{Name of the PRMS 'par_name' file to munge.}
}
\value{
List containing the name, value, description, default value, etc. of each PRMS parameter.
}
\description{
The input file is the output from PRMS run with the \code{-print} option. This function will
not work with an ordinary PRMS parameter file.
}
