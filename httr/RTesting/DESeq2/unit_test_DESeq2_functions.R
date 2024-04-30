

message(strwrap("The functions in unit_test_DESeq2_functions.R are now depreciated and replaced with the httrlib R package. 
 To use httrlib, first install it with: R CMD INSTALL --build httrlib_0.0.0.9000.tar.gz and 
then load it with: library(httrlib)"))


unit_subtest_finish = function(col = NA, row = NA, test_name, test_status, input){
  # unit_subtest_finish - generate a single row of output for the outcome of one test in a unit test and append it to current results
  #
  # Parameters:
  #  col (int) = column number within the entire tested object that the test was run on
  #  row (int) = column number within the entire tested object that the test was run on
  #  test_name (character) = description of what was tested
  #  test_status (character) = either "PASS" or "FAIL" - the outcome of the test
  #  input (data.frame) = the growing result object report - 4 columns deep 
  # Return Value:
  #  (data.frame) = returns a dataframe with 4 columns 
  output <- rbind(input, data.frame("name" = test_name,
                                    "col" = col, 
                                    "row" = row,
                                    "outcome" = test_status))
  return(output)
}



unit_subtest_type <- function(result_1, result_2, running_result){
  # unit_subtest_type - test whether two objects are of the same type - a wrapper around typeof()
  #
  # Parameters:
  #  result_1 (any format) = object to be tested
  #  result_2 (any format) = object to be used as a reference
  #  running_result (data.frame) = result object to which subtest results should be appanded
  # Return Value:
  #  (data.frame) = returns a dataframe with 4 columns
  tryCatch(expr ={
    test_name <- "Test that objects are of the same type"
    message(test_name)
    test_status <- "PASS"
    test_that(test_name,{
      expect_identical(typeof(result_1), typeof(result_2))
    })},
    error = function(e){
      message('Caught error!')
      test_status <<- "FAIL"
      print(e)},
    finally = {output <- unit_subtest_finish(test_name = test_name, 
                                     test_status = test_status,
                                     input = running_result)})
  return(output)
}


unit_subtest_length <- function(result_1, result_2, running_result, col = NA, row = NA){
  # unit_subtest_length - test whether two objects are of the same length
  #
  # Parameters:
  #  result_1 (vector) = object to be tested
  #  result_2 (vector) = object to be used as a reference
  #  running_result (data.frame) = result object to which subtest results should be appanded
  #  col (int) = column number within the entire tested object that the test was run on
  #  row (int) = row number within the entire tested object that the test was run on
  # Return Value:
  #  (data.frame) = returns a dataframe with 4 columns
  tryCatch(expr ={
    test_name <- "Test of equal number of elements"
    message(test_name)
    test_status <- "PASS"
    test_that(test_name,{
      expect_equal(length(result_1), length(result_2))
    })},
    error = function(e){
      message('Caught error!')
      test_status <<- "FAIL"
      print(e)},
    finally = {output <- unit_subtest_finish(test_name = test_name, 
                                     test_status = test_status,
                                     input = running_result,
                                     col = col,
                                     row = row)})
  return(output)
}

unit_subtest_set <- function(result_1, result_2, running_result, col = NA, row = NA){
  # unit_subtest_set - test whether two vectors contain the same set of elements
  #
  # Parameters:
  #  result_1 (vector) = object to be tested
  #  result_2 (vector) = object to be used as a reference
  #  running_result (data.frame) = result object to which subtest results should be appanded
  #  col (int) = column number within the entire tested object that the test was run on
  #  row (int) = row number within the entire tested object that the test was run on
  # Return Value:
  #  (data.frame) = returns a dataframe with 4 columns
  tryCatch(expr ={
    test_name <- "Test for identical sets of elements"
    message(test_name)
    test_status <- "PASS"
    test_that(test_name,{
      expect_setequal(result_1, result_2)
    })},
    error = function(e){
      message('Caught error!')
      test_status <<- "FAIL"
      print(e)},
    finally = {output <- unit_subtest_finish(test_name = test_name, 
                                     test_status = test_status,
                                     input = running_result,
                                     col = col,
                                     row = row)})
  return(output)
}


unit_subtest_ident <- function(result_1, result_2, running_result, row = NA, col = NA){
  # unit_subtest_set - test whether two vectors contain the same set of elements
  #
  # Parameters:
  #  result_1 (vector) = object to be tested
  #  result_2 (vector) = object to be used as a reference
  #  running_result (data.frame) = result object to which subtest results should be appanded
  #  col (int) = column number within the entire tested object that the test was run on
  #  row (int) = row number within the entire tested object that the test was run on
  # Return Value:
  #  (data.frame) = returns a dataframe with 4 columns
  tryCatch(expr ={
    test_name <- "Test that elements are identical"
    message(test_name)
    test_status <- "PASS"
    test_that(test_name,{
      expect_equal(result_1, result_2)
    })},
    error = function(e){
      message('Caught error!')
      test_status <<- "FAIL"
      print(e)},
    finally = {output <- unit_subtest_finish(test_name = test_name, 
                                     test_status = test_status,
                                     input = running_result,
                                     row = row, 
                                     col = col)})
  return(output)
}


unit_subtest_values <- function(result_1, result_2, running_result, tolerance){
# depreciated - use unit_subtest_values_1 instead
  message("unit_subtest_values depreciated in favor of unit_subtest_values_1 -- Please use unit_subtest_values_1 instead")
  return(unit_subtest_values_1(result_1, result_2, running_result, tolerance)
}



unit_subtest_values_1 <- function(result_1, result_2, running_result, tolerance){
  # unit_subtest_values_1 - test whether two vectors contain the same values
  #
  # Parameters:
  #  result_1 (vector) = object to be tested
  #  result_2 (vector) = object to be used as a reference
  #  running_result (data.frame) = result object to which subtest results should be appanded
  #  tolerance (numeric) = tolerance level expressed as a decimal for how much the results can vary without triggering a test failure 
  # Return Value:
  #  (data.frame) = returns a dataframe with 4 columns
  for (i in 1:length(result_1[1,])){
    message(paste0("testing ", names(result_1[i]), " column for equality"))
    expect_col = result_1[,i]
    actual_col = result_2[,i]
    
    #remove NAs, as they break the test - if the vectors are not equivalent it will fail either way
    expect_col <- expect_col[!is.na(expect_col)]
    actual_col <- actual_col[!is.na(actual_col)]
    
    temp_result <-   tryCatch(
      expr = {
        test_name <- paste0("Test for identity in column ", i)
        test_status <<- "PASS"
        test_that(test_name, {
          expect_equal(actual_col, expected = expect_col, tolerance = tolerance, scale = abs(expect_col))
        })
      }
      ,
      error = function(e){
        message('Caught error!')
        test_status <<- "FAIL"
        print(e)
        
      }
    )
    if(inherits(temp_result, "error")){
      #REAL WORK
      running_result <- unit_subtest_finish(test_name = test_name, 
                                    test_status = test_status,
                                    input = running_result,
                                    row = "NA", 
                                    col = i)
    }
  }
  
  return(running_result)
}




unit_test <- function(result_1, result_2, tolerance){
  # unit_test- test whether two data frames contain the same values
  #
  # Parameters:
  #  result_1 (data.frame) = object to be tested
  #  result_2 (data.frame) = object to be used as a reference
  #  tolerance (numeric) = tolerance level expressed as a decimal for how much the results can vary without triggering a test failure 
  # Return Value:
  #  (data.frame) = returns a dataframe with 4 columns
  require(testthat)
  output <- data.frame()
  
  #preemptively relevel factors if that was an issue - we don't care about this being an issue
  #result_2$probe_id <- factor(result_2$probe_id, levels = levels(result_1$probe_id))
  
  #first test whether the objects are comparable in structure
  message("testing structure")
  output <- unit_subtest_type(result_1, result_2, output)
  if ("FAIL" %in% output$outcome){
    return(output)
  }
  output <- unit_subtest_length(result_1[1,],result_2[1,], output)
  if ("FAIL" %in% output$outcome){
    return(output)
  }
  output <- unit_subtest_set(names(result_1), names(result_2), output)
  if ("FAIL" %in% output$outcome){
    return(output)
  }
  output <- unit_subtest_ident(names(result_1), names(result_2), output)
  if ("FAIL" %in% output$outcome){
    return(output)
  }
  output <- unit_subtest_length(result_1$probe_id, result_2$probe_id, output, col = 1)
  if ("FAIL" %in% output$outcome){
    return(output)
  }
  output <- unit_subtest_set(result_1$probe_id, result_2$probe_id, output, col = 1)
  if ("FAIL" %in% output$outcome){
    return(output)
  }
  output <- unit_subtest_ident(result_1$probe_id, result_2$probe_id, output, col = 1)
  if ("FAIL" %in% output$outcome){
    output$outcome[length(output$outcome)] <- "PROBES MISALIGNED"
    message("attempting to align objects")
    #attempt to reorder probes
    result_1$temp <- paste0(result_1$probe_id, result_1$trt_name)
    result_2$temp <- paste0(result_2$probe_id, result_2$trt_name)
    result_2 <- result_2[match(result_1$temp, result_2$temp),]
    result_1$temp <- NULL
    result_2$temp <- NULL
    output <- unit_subtest_ident(result_1$probe_id, result_2$probe_id, output, col = 1)
    output[length(output$outcome),]$name <- "test for probe identity after reordering"
    
    #if this fails too, just break out of the test
    if (output[length(output$outcome),]$outcome == "FAIL"){
      return(output)  
    }
  }   
  
  #if the function has gotten this far without hitting a return, the objects are formatted the same
  # now iterate through every column and test whether the elements are within tolerance
  
  message("testing values")
  output <- unit_subtest_values_1(result_1, result_2, output, tolerance = tolerance)
  
  
  return(output)
}



DESeq2_cut <- function(saved_results, treatments){
  # DESeq2_cut- (compartmentalized unit test) - test whether a localled generated DESeq2 result matches the the saved result
  #
  # Parameters:
  #  saved_results (list) = object with archived DESeq2 results and all necessary objects to recreate that object with local instalation
  #  treatments (data.frame) = dataframe containing information about the analysis parameters that went into calculating DESeq2 results (shrinkage, normalization)
  # Return Value:
  #  (boolean) = if all tests pass, returns true, if *any* fail, returns false
  
  
  #initialize list to catch DESeq2 results as they are generated 
  live_results <- list()
  unit_test_result <- data.frame()
  
  #iterate through rows in treatment dataframe
  for (i in 1:length(treatments[,1])){
    
    debug=getOption("debug",default=FALSE)
    
    #generate a result for this set of parameters
    live_result <- runDESeq2(COUNTS = saved_results[[i]]$COUNTS, CONDS = saved_results[[i]]$CONDS, ref_level=saved_results[[i]]$ref_level, plate_effect=treatments[i,]$plate_effect,   shrinkage=as.character(treatments[i,]$shrinkage), threads=5, debug=debug)
    
    mean_cnt= saved_results[[i]]$anl_opt[[1]]
    plate_effect = saved_results[[i]]$anl_opt[[2]]
    shrinkage = saved_results[[i]]$anl_opt[[3]]
    
    anl_name <- getAnlName(mean_cnt = mean_cnt, plate_effect = plate_effect, shrinkage = shrinkage)
    anl_opt <- list(meanncnt0=mean_cnt, plateteffect=as.integer(plate_effect), shrinkage=shrinkage)          
    
    #save this result along with the arguments that generated it
    temp_output <- list("COUNTS" = saved_results[[i]]$COUNTS, 
                        "CONDS" = saved_results[[i]]$CONDS,
                        "results" = live_result, 
                        "anl_name" = anl_name, 
                        "anl_opt" = anl_opt,
                        "ref_level" = saved_results[[i]]$ref_level)
    
    #append the full result to the growing list of result lists
    live_results <- list.append(live_results, temp_output)
    
  }
  
  
  #after all DESeq2 results are generated, rename elements of the list with anl_name
  for (i in 1: length(live_results)){
    name <- live_results[[i]]$anl_name
    names(live_results)[i] <- name
  }
  
  
  #initialize a dataframe to catch test results
  unit_test_result = data.frame()
  
  #iterate through the result objects and test each one
  for (i in 1: length(live_results)){
    live_result <- data.frame(live_results[[i]]$results[1])
    saved_result <- data.frame(saved_results[[i]]$results[1])
    
    #compare both results
    message(paste0("testing ", saved_results[[i]]$anl_name, " result"))
    test <- unit_test(result_1 = live_result,
                         result_2 = saved_result,
                         tolerance = 0.001)
    test$anl_name <- saved_results[[i]]$anl_name
    
    unit_test_result <- rbind(unit_test_result, test)
  }
  
  
  #if any of the outcomes in this object are failures, return false
  if ("FAIL" %in% unit_test_result$outcome){
    return(FALSE)
  }
  
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  #if none of the outcomes in the result object are failures, return true
  if ("FAIL" %!in% unit_test_result$outcome){
    return(TRUE)
  }
}







