#' @title not.na
#' @details find out items not NA
#' 提供各种函数的镜像版本
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

#' @title not.grepl
#' @details find out items not contains sth.
#' reverse side of the grepl
#' @param x quary vector
#' @examples
#' \dontrun{
#' "The belowing two equals while the first can save one brackets, cause the trd line(without bracket) will cause wrong."
#' not.grepl("F",LETTERS) %>% LETTERS[.]
#' (grepl("F",LETTERS) == F) %>% LETTERS[.]
#' try(grepl("F",LETTERS) == F %>% LETTERS[.])
#' }
#' @export
not.grepl <- Negate(grepl)

#' @title not.in
#' @details find out items not contained in sth.
#' reverse side of the %in%
#' ## 中缀运算符。
#' 在R包中创建中缀运算函数
#' @examples
#' \dontrun{
#' "A" %notin% "B"
#' "A" %notin% "A"
#' "A" %notin% LETTERS
#' }
#' @export
`%notin%` <- Negate(`%in%`)

