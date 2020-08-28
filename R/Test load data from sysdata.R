#' Test_reread_data_from_sysdata
#' Test to read the data by packagename:::dataname in R/sysdata.rda.  ##
#' @examples
#' \dontrun{
#' the_Greek_to_Enl_df <- Test_reread_data_from_sysdata()
#' }
#' @return re
#' @export



Test_reread_data_from_sysdata <- function(){

Loafer:::.packageName %>% cat()
Loafer:::Greek_to_Enl_df

}
