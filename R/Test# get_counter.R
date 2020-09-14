#' @title get a counter for count sth.
#' @details
#'   provide a function factory. counter can be generate by this function.
#'   each time the counter function was called. the counter will add by one
#'   automatically.
#'   the param # NULL no para was needed
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{counter_one <- get_a_counter()
#' counter_one()
#' counter_one()
#' }
#' @export

# magrittr::`%>%`

get_a_counter <- function(){
  i <- 0
  function() {
    i <<- i + 1
    i %>% return()
  }
}
