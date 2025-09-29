


validateProbeManifest_test <- function(){
  #' unit_validateProbeManifest - test that validateProbeManifest on kmown probe return same validated data.frame
  #'
  #' @import testthat
  #' @return N/A
  #' @export validateProbeManifest_test

  validateProbeManifest_expected <- readRDS("../httr/RTesting/validateProbeManifest/validateProbeManifest_expected")
  
  name_and_db <- Sys.getenv(c("TEST_HOST","TEST_DB"))
   
  validateProbeManifest_returned <- validateProbeManifest("../httr/RTesting/validateProbeManifest//human_wt_1.2.csv", db_host=name_and_db["TEST_HOST"], db_name=name_and_db["TEST_DB"])
        
      
  tryCatch(expr ={
    test_name <- "Test that validateProbeManifest consistent"
    message(test_name)
    test_status <- "PASS"
    test_that(test_name,{
      expect_equal(validateProbeManifest_expected, validateProbeManifest_returned)
    })},
    error = function(e){
      message('Caught error!')
      test_status <<- "FAIL"
      print(e)}
   )
}