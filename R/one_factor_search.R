#' one_factor_search
#'
#' Takes test group and control group (each as 3 column data.frames) with each
#' observation being defined by one categorical value and one value from a continuous 
#' variable. Performs 1-1 matching on both factor and continuous values.
#' Begins with first test observation, choosing closest match, and removing
#' it from consideration. Moves down the list, continuing the selection process.
#' Then performs the process in reverse, starting at the last test observation and
#' working up. The forward and backward selections are evaluated based upon which
#' has the lower sum of absolute deviations.
#' 
#' The tests are first parameter, and the controls are second. Make sure that the
#' order of each data.frame is FACTOR, OBSERVATION ID, and CONTINUOUS VALUE.
#' 
#' @param raw_tests The data.frame containing all of the tests 
#' @param raw_controls The data.frame containing all of the controls
#' @export 
#' @author Nicholas Sun <nicholas.sun@rutgers.edu>
#' @examples 
#' raw_tests <- read.csv("raw_tests.csv")
#' raw_controls <- read.csv("raw_controls.csv")
#' two_factor_search(raw_tests, raw_controls) 

one_factor_search <- function (raw_tests, raw_controls) {
  
  colnames(raw_tests) <- c("factor","id","sales")
  colnames(raw_controls) <- c("factor","id","sales") 
  
  tests <- raw_tests$sales; gsub("$", "", tests, fixed = TRUE) 
  testfactor <- raw_tests$factor          
  testid <- raw_tests$id           
  
  controls <- raw_controls$sales; gsub("$", "", controls, fixed = TRUE) 
  controlfactor <- raw_controls$factor  
  controlid <- raw_controls$id
  
  mastertests <- data.frame(testid, testfactor, tests)
  mastercontrols <- data.frame(controlid, controlfactor, controls)
  
  finaldf <- data.frame(district = character(0), control_id= numeric(0), control_value= numeric(0), test_id= numeric(0),test_value=numeric(0))
  
  for (q in unique(mastertests$testfactor)){
    
    a <- mastertests[mastertests$testfactor==q,]
    b <- mastercontrols[mastercontrols$controlfactor==q,]
    
    tests <- a$tests
    testid <- a$testid
    controls <- b$controls
    controlid <- b$controlid
    
    alltests <- data.frame(testid, tests)
    allcontrols <- data.frame(controlid, controls)
    
    ri <- c() 
    
    variance <- c() 
    
    len_tests <- length(tests) 
    
    district <- rep(q, len_tests)
    
    controlvalues <- controls 
    
    max <- 10*max(controlvalues)
    
    for (i in 1:len_tests){
      z <- which.min(abs(controlvalues - alltests$tests[i]))
      variance[i] <- (controlvalues[z] - alltests$tests[i])
      ri <- c(ri, z)
      controlvalues[z] <- max
    }
    
    f.controls <- allcontrols[ri,] 
    
    controlvalues <- controls; length(controlvalues[controlvalues == 0]) 
    
    b.ri <- c() 
    
    b.variance <- c()
    
    for (i in 1:(len_tests)){
      z <- which.min(abs(controlvalues - alltests$tests[len_tests + 1 - i]))
      b.variance[i] <- (controlvalues[z] - alltests$tests[len_tests + 1 - i])
      b.ri <- c(b.ri, z)
      controlvalues[z] <- max
    }
    
    b.ri <- rev(b.ri)
    
    b.controls <- allcontrols[b.ri,]
    
    if (sum(abs(variance)) < sum(abs(b.variance))){
      final <- data.frame(alltests, f.controls)
    } else {
      final <- data.frame(alltests, b.controls)
    }
    
    final <- cbind(district, final)
    
    finaldf <- rbind(finaldf,final)
    
  }
  
  colnames(finaldf) <- c("FACTOR", "Test_ID", "Test_Sales", "Control_ID","Control_Sales")
  return(finaldf)
  
}