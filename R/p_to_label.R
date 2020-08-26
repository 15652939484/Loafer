#' trans p to labels.
#' a wrap function for cut. To handle the p value transformation.
#' @param input numeric vector. p need to be transed.
#' @param digits the length of the digits to be reserved
#' @param breaks numeric vector; to cut the p value
#' @param labels character vector; the labels to show for different p value.
#' @param trans_to_character logical, indicating wether to summary those p less than 0.0001 as "<0.0001"
#' @param drop_p logical, indicating wether to drop p or show p in the return.
#' @param include_right_bound logical, default F, indicating if the intervals should be closed
#'     on the right (and open on the left) or vice versa.
#' @param
#' @examples
#' \dontrun{
#'p_c <- c(0.001, 0.03, 0.05, 0.00001)
#'p_to_label(p_c)
#'p_to_label(p_c, drop_p = T)
#'}
#' @return return a vector of p value labels.
#' @export

p_to_label <- function(input, digits= 4,
                       breaks = c( -0.00000000001,0.001,0.01,0.05,1.0001) ,
                       labels =c("***","**","*",""),
                       trans_to_character = T,
                       drop_p = T,
                       include_right_bound = F){
  cat ( " <0.001, <0.01, <0.05 and <= 1 were used to cut the raw p values in default ") ###

  label_vector   <-  cut ( input , breaks , labels , right = include_right_bound  ) %>% str_pad ( . , width = 3, side = "right") ## 先生成标记
  neat_number    <-  neat_round ( input , digits =  digits ) ### get data
  if(trans_to_character == T){ ## trans qualified p to "<0.0001".
    neat_number[input <0.0001] <- "<0.0001" ## 替换特殊值
  }
  if(drop_p == T){
    output <- label_vector
  }else{
    output <-  paste (neat_number, label_vector ) ### combine the pvalue and the label.
  }
  return ( output )
}
