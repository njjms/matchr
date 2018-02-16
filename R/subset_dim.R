#' subset_dim
#' 
#' Gives the dimensions of each subset of the data; allows the analyst
#' to know the distribution of the tests after being divided along
#' one or two cateogorical factors, as well as the possible control pool
#' for each subset. A low ratio between tests and controls might indicate
#' that R was forced to make selections for certain controls that might not
#' be a good fit.
#' 
#' The number of categorical factors defaults to 1. 
#  Can also be done on 2 categorical factors. Make sure that the 
#' categorical factors are in the leftmost columns of each data set.
#' 
#' @param raw_tests The imported test data
#' @param raw_controls The imported control data
#' @param num_cat_factors The number of categorical factors we are subsetting on
#' @export
#' @author Nicholas Sun <nicholas.sun@rutgers.edu>
#' @examples 
#' raw_tests <- read.csv("raw_tests_2_categories.csv")
#' raw_controls <- read.csv("raw_controls_2_categories.csv")
#' subset_dim(raw_tests, raw_controls, 2)

subset_dim <- function (raw_tests, raw_controls, num_cat_factors = 1){
  
  subset_dim <- data.frame(test_size = numeric(0), control_size = numeric(0), ratio = numeric(0))
  
  if(num_cat_factors == 1){
    
    colnames(raw_tests)[1] <- "FACTOR"
    colnames(raw_controls)[1] <- "FACTOR"
    
    for (q in unique(raw_tests$FACTOR)){
      
      a <- raw_tests[raw_tests$FACTOR==q,]
      b <- raw_controls[raw_controls$FACTOR==q,]
      
      add.subset_dim <- c(dim(a)[1], dim(b)[1], (dim(b)[1]/dim(a)[1]))
      subset_dim <- rbind(subset_dim, add.subset_dim)
      
    }
    
    colnames(subset_dim) <- c("Number of Tests", "Number of Controls", "Ratio of Controls to Tests")
    print(subset_dim)
    
  } else if ( num_cat_factors == 2) {
    
    colnames(raw_tests)[1] <- "FACTOR"
    colnames(raw_controls)[1] <- "FACTOR"
    
    colnames(raw_tests)[2] <- "FACTOR2"
    colnames(raw_controls)[2] <- "FACTOR2"
    
    for (q in unique(raw_tests$FACTOR)){
      
      a <- raw_tests[raw_tests$FACTOR==q,]
      b <- raw_controls[raw_controls$FACTOR==q,]
      
      for (w in unique(a$FACTOR2)) {
        
        c <- a[a$FACTOR2 == w,]
        d <- b[b$FACTOR2 == w,]
        
        add.subset_dim <- c(dim(c)[1], dim(d)[1], (dim(d)[1]/dim(c)[1]))
        subset_dim <- rbind(subset_dim, add.subset_dim)
    
      }
      
    }
    
    colnames(subset_dim) <- c("Number of Tests", "Number of Controls", "Ratio of Controls to Test")
    print(subset_dim)
    
  } else {
    
    print("There are more categorical factors (>2) than these functions can take.")
  
  }
}