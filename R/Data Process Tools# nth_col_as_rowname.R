#' @title Set nth col to rownames
#' @description 将数据框的第n列设定为行名。 同时可以去除/保留第n列前面的内容
#' @param df input data.frame
#' @param nth default 1, the nth col will be set as new rowname.
#' @param drop_before_nth logical. wether drop the contains before nth col.
#' @examples
#' \dontrun{
#' df <- iris
#' set.seed(1028)
#' df$var3 <- rnorm(nrow(df))
#' df$var4 <- rnorm(nrow(df))
#' df %>% head
#' df %>% head
#' nth_col_as_rowname(df, 6)
#' nth_col_as_rowname(df, 6, drop_before_nth = F)
#' }
#' @return a data.frame with new rownames
#' @export
nth_col_as_rowname <- function(df, nth = 1, drop_before_nth = T){
    rownames(df) <-  df[,nth]
    if(drop_before_nth == T){
        df <-  df[,-c(1:nth), drop = F]
    }
    return(df)
    ## 或许 以後应该加个检测重复值的功能。
}


#' @title Trans data.frame rowname to the first col
#' @param df a df
#' @param colname character, name of the new col for the rownames
#' @examples
#' \dontrun{
#' rownames_to_first_column(iris, colname = "New col")
#' }
#' @export
rownames_to_first_column <- function(df, colname = "new"){
  # rownames_to_first_column <- function(df, var = "rowname") {
  ## turn rownames as the first col.
  # df
  if(!is.data.frame(df)){stop("输入得是个数据框或者是矩阵，老哥")}



  left_df <- data.frame(rownames(df), stringsAsFactors = F, check.names = F)
  colnames(left_df) <- colname
  final_df <- cbind(left_df, df)
  return(final_df)
}
