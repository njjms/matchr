% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_na.R
\name{find_na}
\alias{find_na}
\title{find_na}
\usage{
find_na(raw_tests)
}
\arguments{
\item{raw_tests}{Can take data with either one or two categorical factors}
}
\description{
Missing values, blank cells, and extraneous characters in your original CSV can
become NA values when you import them into R. These NA values prevent the 
function loops from running. Run this function on them to see if there are 
any NA values present in your original CSV data; all NA values should
be removed either in your original CSV file or in R itself.
}
\details{
Takes one data.frame structure at a time. Can be run on both test and control data.frames, but
test group is more important since functions more dependent upon integrity of
test data files.
}
\examples{
raw_tests <- read.csv("raw_tests.csv")
raw_controls <- read.csv("raw_controls.csv")
find_na(raw_tests)
}
\author{
Nicholas Sun <nicholas.sun@rutgers.edu>
}

