#' @title not.na
#' @details find out items not NA
#' @param x quary vector
#' @examples
#' \dontrun{
#' TEST_C <- c(1:3,NA,3)
#' is.na(TEST_C)
#' not.na(TEST_C)
#' }
#' @export
not.na <- function(x){
    is.na(x = x) %>% not()
}


#' @title not.na
#' @details find out items not NA
#' @param x quary vector
#' @examples
#' \dontrun{
#' not.null(NULL)
#' not.null(3)
#' }
#' @export
not.null <- function(x){
    is.null(x = x) %>% not()
}
