#' find_na
#' 
#' Missing values, blank cells, and extraneous characters in your original CSV can
#' become NA values when you import them into R. These NA values prevent the 
#' function loops from running. Run this function on them to see if there are 
#' any NA values present in your original CSV data; all NA values should
#' be removed either in your original CSV file or in R itself.
#' 
#' Takes one data.frame structure at a time. Can be run on both test and control data.frames, but
#' test group is more important since functions more dependent upon integrity of
#' test data files. 
#' 
#' @param raw_tests Can take data with either one or two categorical factors
#' @export 
#' @author Nicholas Sun <nicholas.sun@rutgers.edu>
#' @examples 
#' raw_tests <- read.csv("raw_tests.csv")
#' raw_controls <- read.csv("raw_controls.csv")
#' find_na(raw_tests)

find_na <- function (raw_tests) {
  if (dim(raw_tests)[2] == 3){
    
    check <- is.na(raw_tests[,3])
    if (length(check[check == TRUE]) == 0) {
      print("There are no NA values in this data.")
    } else {
      print("There are NA values present in this data located at the following: ")
      which(check)
    }
    
  } else if (dim(raw_tests)[2] == 4){
    
    check <- is.na(raw_tests[,4])
    if (length(check[check == TRUE]) == 0) {
      print("There are no NA values in this data.")
    } else {
      print("There are NA values present in this data located at the following: ")
      which(check)
    }
    
  } else {
    print("Too many columns; recheck your data")
  }
}