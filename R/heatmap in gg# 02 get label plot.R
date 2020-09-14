#' @title get label plots
#' @details 
#' 输入  ExpressionSet对象。 
#' 根据其中的表型数据和特征数据进行绘图。
#' 输入颜色。 输出一个label用的图。
#' 多列的注释可以分别出图，每个占一行。 分别都可以有label的。
#' 生成图片可以附到之前的对象中。
#' @param ExpreessionSet_input
#' @param heatmap_ls heatmap_ls for append heatmap pics
#' @param color_ls color list for mannually set colors for class.
#' @param tile.size.top.height tile height for top label.
#' @param tile.size.top.width tile width for top label.
#' @param tile.size.left.height tile height for left label.
#' @param tile.size.left.width tile width for top label.
#' @param expansion.top.y unit = "cm". expansion for 
#' @param expansion.left.x unit = "cm"
#' \dontrun{
#'     ## 绘制热图主干
#'     # 调整tile 的宽、高，并且使空隙保持为固定值/不要有空隙。
#'     library(pacman)
#'     library(Loafer)
#'     get_or_set_dir()
#'     p_load(reshape2)
#'     # p_load(shadowText)
#'     # p_load_current_gh("GuangchuangYu/shadowtext")
#'     library(shadowtext)
#'     library(dplyr)
#'     library(ggplot2)
#'     library(plyr)
#'     library(magrittr)
#'     library(tibble) ### 此包有毒。可能导致控制台无法进行屏幕输出。
#'     getwd()
#'     ### 需要解决的问题：
#'     # 使用geom tile 绘制热图的主干。
#'     # 进行环形绘图(放弃，需要另外写代码及函数)
#'     # 能够挑一个比较合适的配色，白色放在中间。
#'     # 使用 ggplot2包绘制热图主干。shadowText包添加注释。计划使用ggtree绘制热图用的聚类树。
#'     # 使用 aplot包拼接各个图片。
#'     ## 进行Z变换的函数
#'     # trans_lines(mode = "add #'")
#'     data("iris")
#'     assayData <- iris[1:30,1:4] %>% t
#'     phenoData <- iris[1:30,5, drop = F]
#'     featureData <-  data.frame(variable = iris %>% colnames %>% `[`(.,1:4), type = c("A", "A", "B", "B"))
#'     rownames(featureData) <- featureData[,1]
#'     
#'     ExpressionSet <- ExpressionSet(assayData = assayData, ## 要求是matrix格式，并且sample by col and feature by row.
#'                                    phenoData = new("AnnotatedDataFrame",
#'                                                    data = phenoData), ## 可以是数据框
#'                                    featureData = new("AnnotatedDataFrame", data = featureData), ## 可以是数据框。
#'                                    ## 采用protocol Data存储
#'                                    protocolData   = new("AnnotatedDataFrame", data = assayData %>% t %>% data.frame), ## 可以是数据框
#'                                    experimentData = new("MIAME", title = "test"))
#'     ExpressionSet@assayData$exprs
#'     ExpressionSet@phenoData@data
#'     ExpressionSet@featureData@data
#'     ExpressionSet@protocolData@data
#'     
#'     heatmap_ls <- heatmap_trunk(ExpreessionSet_input = ExpressionSet)
#' p_load(aplot)
#' library(Loafer)
#' ### 需要能够调节字号。
#' ExpreessionSet_input = ExpressionSet
#' heatmap_ls <- get_label_tiles(ExpreessionSet_input = ExpressionSet, heatmap_ls = heatmap_ls,
#'                               tile.size.top.height = .57,
#'                               tile.size.top.width = .95,
#'                               tile.size.left.height = .9,
#'                               tile.size.left.width = .5,
#'                               expansion.top.y = .03,
#'                               expansion.left.x = .03)
#' heatmap_ls$Left_labels$variable
#' heatmap_ls$Left_labels$type
#' heatmap_ls$Top_labels$Species
#' 
#' heatmap_ls$heatmap_trunk %>% insert_top(heatmap_ls$Top_labels$Species, height = .05) %>%
#'     # insert_top(heatmap_ls$Top_labels$NewClass, height = .04) %>%
#'     insert_left(heatmap_ls$Left_labels$variable, width = .03) %>% 
#'     insert_left(heatmap_ls$Left_labels$type, width = .03) %>% 
#'     ggsave("測試label图2.png", ., width = 30, height = 9)
#' }
#' @importFrom tibble rownames_to_column
#' @export
get_label_tiles <- function(ExpreessionSet_input = ExpressionSet,
                            heatmap_ls = heatmap_ls,
                            color_ls = list(
                                ## 以列名为向量的名称。 每个向量内部是一系列赋值的因子-颜色对子。
                                Species = c(setosa = "pink", versicolor = "black"),
                                NewClass = c(New = "red", Old = "cyan"),
                                variable = c(Sepal.Length = "green", Sepal.Width = "yellow", 
                                             Petal.Length = "red", Petal.Width = "black"),
                                type = c(A = "grey30", B = "cyan")),
                            tile.size.top.height = .87,
                            tile.size.top.width = .5,
                            tile.size.left.height = .5,
                            tile.size.left.width = .97,
                            expansion.top.y = .1,
                            expansion.left.x = .05){
    ### Get labels on top
    dat <- ExpreessionSet_input@phenoData@data
    dat <- rownames_to_first_column(df = dat, colname = "rowname")
    for (i in 2:ncol(dat)){
        eachvar = colnames(dat)[i]
        pic_labels <-  ggplot(dat, aes_string(x = "rowname", y = 0, fill = eachvar)) + 
            geom_tile(aes(width = tile.size.top.width, height = tile.size.top.height), 
                      size = 1, position = position_nudge(y = 9)) +
            scale_fill_manual(values = color_ls[[eachvar]]) +
            # scale_x_discrete(expand = expansion(add = .1)) +
            scale_y_discrete(expand = expansion(add = expansion.top.y)) +
            theme_void() +
            theme(plot.margin = unit(x = c(100, 10, 10, 10), units = "cm"))  ###
        heatmap_ls[["Top_labels"]][[eachvar]] <- pic_labels
    }
    # heatmap_ls$Top_labels
    ### Get labels on Left
    dat <- ExpreessionSet_input@featureData@data
    dat <- rownames_to_first_column(df = dat, colname = "rowname")
    for(i in 2:ncol(dat)){
        eachvar = colnames(dat)[i]
        pic_labels <-  ggplot(dat, aes_string(x = 0, y = "rowname", fill = eachvar)) + 
            geom_tile(aes(width = tile.size.left.width, height = tile.size.left.height), size = 1) +
            scale_fill_manual(values = color_ls[[eachvar]]) +
            scale_x_discrete(expand = expansion(add = expansion.left.x)) +
            # scale_y_discrete(expand = expansion(add = .01)) +
            theme_void()
        heatmap_ls[["Left_labels"]][[eachvar]] <- pic_labels
    }
    return(heatmap_ls)
}