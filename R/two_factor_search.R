#' two_factor_search
#' 
#' Takes test group and control group (as 4 column data.frame) with 
#' exactly two categorical variables and one continuous variable for each
#' obsevation. Performs 1-1 matching on both factors and a continuous value.
#' Begins with first test, removing selected controls as it travels down.
#' Then reperforms search going from the last test up to the first test.
#' Forwards and backwards selections evaluated based on which has lower sum
#' of absolute deviations from continuous values. 
#'
#' The tests are the first parameter, controls are second.
#' Make sure that order in each data.frame input is FIRST FACTOR,
#' SECOND FACTOR, OBSERVATIOIN ID, and CONTINUOUS VALUE. 
#'
#' @param raw_tests The data.frame containing the tests
#' @param raw_controls The data.frame containing the controls
#' @export 
#' @author Nicholas Sun <nicholas.sun@rutgers.edu>
#' @examples 
#' raw_tests <- read.csv("raw_tests.csv")
#' raw_controls <- read.csv("raw_controls.csv")
#' two_factor_search(raw_tests, raw_controls)

two_factor_search <- function(raw_tests, raw_controls) {
  
  colnames(raw_tests) <- c("factor1","factor2","id","sales")
  colnames(raw_controls) <- c("factor1","factor2","id","sales")
  
  testvalues <- raw_tests$sales; gsub("$", "", testvalues, fixed = TRUE)      
  testfactors <- raw_tests$factor1                       
  testfactors2 <- raw_tests$factor2            
  testid <- raw_tests$id                               
  
  controlvalues <- raw_controls$sales; gsub("$", "", testvalues, fixed = TRUE)      
  controlfactors <- raw_controls$factor1                  
  controlfactors2 <- raw_controls$factor2             
  controlid <- raw_controls$id      
  
  mastertests <- data.frame(testfactors, testfactors2, testid, testvalues)
  mastercontrols <- data.frame(controlfactors, controlfactors2, controlid, controlvalues)
  
  output_df <- data.frame(district = character(0), SIC = character(0), test_id= numeric(0), test_value= numeric(0), SIC = character(0), control_id= numeric(0),control_value=numeric(0))
  subset_dim <- data.frame(test_size = numeric(0), control_size = numeric(0))
  
  for (q in unique(mastertests$testfactors)){
    
    a <- mastertests[mastertests$testfactors==q,]
    b <- mastercontrols[mastercontrols$controlfactors==q,]
    
    t.values <- a$testvalues
    t.id <- a$testid
    t.testfactors2 <- a$testfactors2
    
    c.values <- b$controlvalues
    c.id <- b$controlid
    c.testfactors2 <- b$controlfactors2
    
    factor1tests <- data.frame(t.testfactors2, t.id, t.values)
    factor1controls <- data.frame(c.testfactors2, c.id, c.values)
    
    factor1output <- data.frame(district = character(0), SIC = character(0), test_id= numeric(0), test_value= numeric(0), SIC = character(0), control_id= numeric(0),control_value=numeric(0))
    
    for (w in unique(factor1tests$t.testfactors2)){
      
      c <- factor1tests[factor1tests$t.testfactors2==w,]
      d <- factor1controls[factor1controls$c.testfactors2==w,]
      
      t2.values <- c$t.values
      t2.id <- c$t.id
      t2.testfactors2 <- c$t.testfactors2
      
      c2.values <- d$c.values
      c2.id <- d$c.id
      c2.controlfactors2 <- d$c.testfactors2
      
      factor2tests <- data.frame(t2.testfactors2, t2.id, t2.values)
      factor2controls <- data.frame(c2.controlfactors2, c2.id, c2.values)
      
      add.subset_dim <- c(length(t2.id), length(c2.id))
      subset_dim <- rbind(subset_dim, add.subset_dim)
      
      len_tests <- length(c$t.id)
      
      district2 <- rep(q, len_tests)
      
      ri <- c() 
      
      variance <- c() 
      
      controlvalues2 <- c2.values
      
      max <- 10*max(controlvalues2)
      
      for (i in 1:len_tests){
        z <- which.min(abs(controlvalues2 - factor2tests$t2.values[i]))
        variance[i] <- (controlvalues2[z] - factor2tests$t2.values[i])
        ri <- c(ri, z)
        controlvalues2[z] <- max
      }
      
      f.controls <- factor2controls[ri,] 
      
      controlvalues2 <- c2.values
      
      b.ri <- c() 
      
      b.variance <- c()
      
      for (i in 1:len_tests){
        z <- which.min(abs(controlvalues2 - factor2tests$t2.values[len_tests + 1 - i]))
        b.variance[i] <- (controlvalues2[z] - factor2tests$t2.values[len_tests + 1 - i])
        b.ri <- c(b.ri, z)
        controlvalues2[z] <- max
      }
      
      b.ri <- rev(b.ri)
      
      b.controls <- factor2controls[b.ri,]
      
      if (sum(abs(variance)) < sum(abs(b.variance))){
        final <- data.frame(district2,factor2tests, f.controls)
      } else {
        final <- data.frame(district2,factor2tests, b.controls)
      }
      
      factor1output <- rbind(factor1output,final)
    }
    
    output_df <- rbind(output_df, factor1output)
    
  }
  
  colnames(output_df) <- c("FACTOR 1", "FACTOR 2", "Test_ID", "Test_Sales","Control_FACTOR2", "Control_ID","Control_Sales")
  return(output_df)
  
}
  