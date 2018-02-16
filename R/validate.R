#' validate
#' 
#' Provides some diagnostics on the goodness of control fit to the test groups:
#' average difference, proportion of difference to test group average, and if 
#' there are any duplicats present in the controls.
#' 
#' If there are duplicates in the controls, the functions output will be a vector
#' of row numbers which contain the duplicated controls.
#' 
#' @param output_df The data frame that contains all tests and selected controls
#' @export
#' @author Nicholas Sun <nicholas.sun@rutgers.edu> 
#' @examples
#' output <- one_factor_search(raw_tests, raw_controls)
#' validate(output)

validate <- function (output_df) {
  
  diff <- (mean(output_df$Test_Sales) - mean(output_df$Control_Sales))
  print(paste("The average difference between test and controls is", toString(diff)))

  diff.prop <- (mean(output_df$Control_Sales) - mean(output_df$Test_Sales))/mean(output_df$Test_Sales)
  print(paste("This difference is", toString(diff.prop),"% of the test group average"))

  dupes <- duplicated(output_df$Control_ID)
  
  if ((length(dupes[dupes == "TRUE"])) == 0){
    
    print("There are no duplicates in the selected controls.")
    
  }else{
    
    print("There are duplicates in the selected controls, located in the following rows:")
    
    dupes.locs <- which(dupes)
    print(dupes.locs)
    
  }
  
}
       