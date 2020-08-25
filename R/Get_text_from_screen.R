#'redirect some obj(such as some model summary, which can be assigned directly.)
#'this function redirect the text into a temp file and reread it.
#'@param  my_message, obj print in the screen
#' and can't be assigned to other variables directly.
#'@examples
#'\dontrun{data("iris")
#'head(iris)
#'z <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
#'z
#'summary(z)
#'k <- summary(z)
#'k
#'q <- Redirect_text_from_screen(summary(z))
#'q
#'}
#'@return text by line. trans imformation into text and can be value assigned to other vars.
#'@export

### Function needs to be exported. or else can't be accessed nor used by other users.
Redirect_text_from_screen <-  function(my_message){
  ### 从屏幕输出中获取结果。
  ### 将结果文字返回。
  sink(file = "temp.txt",append = F, type = "output")
  print(my_message)
  sink()
  temp_df <-  read.delim("temp.txt",sep = "\n")
  file.remove("temp.txt")
  return(temp_df)
}
