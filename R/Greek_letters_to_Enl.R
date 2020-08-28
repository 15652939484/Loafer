#' Trans Greek letters to English.
#' @param input, vector needs to trans into English
#' @param Greek_to_Enl_df, dictionary for trans.
#' @examples
#' \dontrun{
#'  Greek_to_Enl(input = c("Γ", "Δ", "δ"), Greek_to_Enl_df = Greek_to_Enl_df)
#' }
#' @return
#' @export
#'

Greek_to_Enl <- function(input, Greek_to_Enl_df = Greek_to_Enl_df){
  ## trans greek (i character vector) into Enl.
  dictionary <- Greek_to_Enl_df%>% melt(., "pinyin")
  matched <- input %in% dictionary$value
  position_in_dic <- input[matched] %>% match(., dictionary$value)
  input[matched] <- position_in_dic  %>% dictionary$pinyin[.]
  return(input)
}
