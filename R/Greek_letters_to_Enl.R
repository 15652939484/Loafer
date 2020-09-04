#' @title Trans Greek letters to English.
#' @description
#' the param Greek_to_Enl_df, dictionary for trans. depracted
#' @param input ,vector needs to trans into English
#'
#' @importFrom reshape2 melt
#'
#' @examples
#' \dontrun{
#'  Greek_to_Enl(input = c("Γ", "Δ", "δ"), Greek_to_Enl_df = Greek_to_Enl_df)
#' }
#' @return
#' @export

Greek_to_Enl <- function(input){
  # trans greek (i character vector) into Enl.
  # dictionary <- Loafer:::Greek_to_Enl_df %>% melt(., "pinyin")
  dictionary <- Greek_to_Enl_df %>% melt(., "pinyin") ## quote the data name directly can be enough.
  matched <- input %in% dictionary$value
  position_in_dic <- input[matched] %>% match(., dictionary$value)
  input[matched] <- position_in_dic  %>% dictionary$pinyin[.]
  return(input)
}
