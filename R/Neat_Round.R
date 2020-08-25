#' works like format function.
#' round the data and keep the data with selected length.
#' zero in the tail will also be keeped.
#' @param num numeric, vector, num need to be round.
#' @param digits
#' @examples
#' \dontrun{
#' num <- c(1,3,5.2,-2.3)
#' neat_round(num, digits=3)#'
#' }
#' @export
neat_round <- function(num, digits=3){
  " version 5.0 "
  scipen_length = 20000
  options(scipen = scipen_length) ## control the length to carry out the sci notation.
  num <- round(num,digits = digits)
  zheng_shu_bu_fen  <- trunc(num)  # trunc the data.
  zheng_shu_char <- sub("([-+]?[0-9]+)\\.[0-9]*","\\1",num)
  # 利用正则表达式提取整数部分，防止0开头的负小数时，符号被忽略
  # 核心工作流程： 先取绝对值再加1,（确保不是0打头），乘以10的有效位数次方
  # 最后需要的全部都跑到了小数点左边（可以被截取）
  # 然后刨除前面整数部分，取后面的小数部分进行衔接。
  new_xiao_shu <-  abs(num-zheng_shu_bu_fen)  ### 用原数值减整数部分直接得到小数部分,取绝对值,保障是正数. 且首位是0
  new_xiao_shu <- new_xiao_shu+0.1^(digits+1) ### 保留 n位 小数, 就加一个小数点后 n个0,1个1的数,使前面的零强制显示
  xiao_shu_bu_fen <- substr(new_xiao_shu,3,2+digits)
  final <- paste(zheng_shu_char,".",xiao_shu_bu_fen,sep = "")
  return(final)
}
