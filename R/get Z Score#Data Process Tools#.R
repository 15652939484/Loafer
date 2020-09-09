#' @title Get Z score from matrix or data.frame(pure number.)
#' @details 使用纯数据数据框或者矩阵做为输入，选择按照行或列进行Z得分转换
#' @param m a mun matrix or data.frame to be scaled.
#' @param by specified "col" or "row" to be scaled.
#' @return a matrix/data.frame with scaled value.
#' @examples
#' \dontrun{
#' get_z_score(m = iris[,1:4], by = row)
#' get_z_score(m = iris[,1:4], by = "row")
#' get_z_score(m = iris[,1:4], by = "col")
#' get_z_score(m = iris[,1:4] %>% as.matrix(), by = "row")
#' get_z_score(m = iris[,1:4] %>% as.matrix(), by = "col")
#' }
#' @export

### 获取Z得分
### 输入一个数据框/矩阵
### 确定按行/列进行Z得分变换。
### 输出：转换后的Z得分矩阵。

get_z_score <- function(m = df,### 输入纯数据矩阵。可以使用矩阵或者数据框。
                        by = "col"){
    # 判定是否有NA及其他字符。
    # 若无，强制数据转换为数字
    # 按照行/列进行Z转换
    raw_m <- m
    direction <- 2
    if( (is.na(m) %>% sum) > 0){stop("存在NA，请先去除或者填充")}
    ## 转换为数据格式。
    if(by == "row"){ ## 按照列计算
        m <- m %>% t
    }
    the_rowname <- rownames(m)
    m <- m %>% apply(., 2, function(each_col){each_col %>% as.character() %>% as.numeric()})
    # m %>% str
    z_score_m <- m %>% apply(.,direction, function(each_item){
        z_score <- (each_item - mean(each_item) )/sd(each_item)
    })
    rownames(z_score_m) <- the_rowname
    if(by == "row"){ ## 按照列计算
        z_score_m <- z_score_m %>% t
    }

    if(class(raw_m) == "data.frame"){z_score_m <- data.frame(z_score_m, check.names = F)}
    return(z_score_m)
}
