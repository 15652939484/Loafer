#' @title Plot the trunk of a heatmap via geom_tile.
#' Plot the trunk (tiles) for heatmap
#' @details plot the trunk via geom_tile and add labels via geom_shadowtext.
#' 需求：
#' 调整各个文字的缩放比例
#' 调整格子内部label的填充色和外部轮廓色。
#' 调整mapping使用的颜色 ## 后续搞
#' @param ExpreessionSet_input ExpreessionSet if set no more need to set dat and label df seperately.
#' in the ExpreessionSet_input obj. label data were saved in the protocolData slot.
#' @param dat df.
#' @param label_df df. names need to be match with dat.
#' @param axis.text.size size for axis text. 坐标轴文字的大小
#' @param label.size size for label text.
#' @param Z_by_row_or_col NULL or “row” or “col” 按照行或列进行Z标准化。
#' @param color.key.height unit cm. 图例的高
#' @param color.key.width  unit cm. 图例的宽
#' @param axis.y.position left or right. right as default. Position for the axis.text.y 调整坐标轴标题的位置。
#' @param tile.colors colors v tiles fill in heatmap trunk. 注意调整此数据，要让平衡点的颜色是白色等分界色。
#' @param tile.size size of a tile, numeric, 0 ~ 1.
#' @param tile.border.col color breaks for tiles in heatmap trunk. 改变格子边界的颜色
#' @param tile.border.size size of a tile border.
#' @examples
#' \dontrun{
#' data("iris")
#' assayData <- iris[1:30,1:4] %>% t
#' phenoData <- iris[1:30,5, drop = F]
#' featureData <-  data.frame(variable = iris %>% colnames %>% `[`(.,1:4), type = c("A", "A", "B", "B"))
#' rownames(featureData) <- featureData[,1]
#'
#' ExpressionSet <- ExpressionSet(assayData = assayData, ## 要求是matrix格式，并且sample by col and feature by row.
#'                                phenoData = new("AnnotatedDataFrame",
#'                                                data = phenoData), ## 可以是数据框
#'                                featureData = new("AnnotatedDataFrame", data = featureData), ## 可以是数据框。
#'                                ## 采用protocol Data存储
#'                                protocolData   = new("AnnotatedDataFrame", data = assayData %>% t %>% data.frame), ## 可以是数据框
#'                                experimentData = new("MIAME", title = "test"))
#' ExpressionSet@assayData$exprs
#' ExpressionSet@phenoData@data
#' ExpressionSet@featureData@data
#' ExpressionSet@protocolData@data
#'
#' heatmap_ls <- ggheatmap(
#'     ExpreessionSet_input = ExpressionSet,
#'     # dat = iris[1:20,1:4] ,
#'     # label_df = iris[1:20,1:4] %>% round(., 2),
#'     Z_by_row_or_col = "row",
#'     axis.y.position = "right",
#'     axis.text.size = 18 , ## 调整坐标轴标签的字号（其他字号是在此基础上缩放得到的）
#'     label.size = 7.8   , ## 调整label的字号
#'     label.y.adj = .03 , ##  调整label的位置
#'     label.color = "white" , ## label 的填充色
#'     label.color.bg = "firebrick" , ## label 轮廓的颜色。
#'     color.key.height = 5,
#'     color.key.width  = 1,
#'     tile.border.col = "black" , ### color for tile border
#'     tile.colors = c("blue","steelblue","white","orange","red"),
#'     tile.size = .7,
#'     tile.border.size = 2)
#'
#' # heatmap_ls$heatmap_trunk
#' ggsave("test newzi trunk.png", plot = heatmap_ls$heatmap_trunk, height = 5, width = 36)
#' }
#' @importFrom shadowtext geom_shadowtext
#' @importFrom reshape2 melt
#' @importFrom reshape2 dcast
#' @importFrom Biobase ExpressionSet
#' @importFrom Biobase aggenv
#' @return return a pic plot (ggplot2.)
#' @export

ggheatmap <- function(ExpreessionSet_input = NULL,
                      dat = iris[1:20,1:4], label_df = iris[1:20,1:4] %>% round(., 2),
                      Z_by_row_or_col = "row",
                      axis.y.position = "right",
                      axis.text.size = 18 , ## 调整坐标轴标签的字号（其他字号是在此基础上缩放得到的）
                      label.size = 7.8   , ## 调整label的字号
                      label.y.adj = -.09 , ##  调整label的位置
                      label.color = "white" , ## label 的填充色
                      label.color.bg = "firebrick" , ## label 轮廓的颜色。
                      color.key.height = 5,
                      color.key.width  = 1,
                      tile.border.col = "black" , ### color for tile border
                      tile.colors = c("black","blue","white","orange","red"),
                      tile.size = .7,
                      tile.border.size = 2){

  heatmap_ls <- list()
  if(is.null(ExpreessionSet_input) == F){
    dat <-  ExpreessionSet_input@assayData$exprs %>% data.frame(., check.names = F, stringsAsFactors = F)
    label_df <- ExpreessionSet_input@protocolData@data %>% t %>% data.frame(., check.names = F, stringsAsFactors = F)
  }

  ### match args to ensure the input is right.
  Z_by_row_or_col <- Z_by_row_or_col %>%  match.arg(c("row", "col",NULL))
  axis.y.position %<>% match.arg(c("left", "right")) ## 可以调整y轴文字的位置。
  # Z_by_row_or_col <- "夏继尔打" %>%  match.arg(c("row", "col"))

  dat <- Loafer::get_z_score(dat, by = "row")
  dat$ID <- rownames(dat)
  dat <- dat %>% melt(., "ID")
  label_df$ID <- rownames(label_df)
  label_df <- label_df %>% melt(., "ID")
  colnames(label_df)[3] <- "label"
  dat <- full_join(dat, label_df, by = c("ID","variable"))
  dat %>% head

  heatmap_trunk <- ggplot(data = dat, aes(x = variable, y = ID))+
    geom_tile(aes(fill = value, width = tile.size, height = tile.size),
              size = tile.border.size
              # 其他函数的结果： 横、纵轴推移的位置。制作gap时需要，暂时不用搞
              # ,position = position_nudge(y = dat$nudge_y, x = dat$nudge_x) ##可以通过这个对格子的位置进行微调。 但是其他的组合图片可能不能同步此调整，因此不建议使用。
              ,color = tile.border.col)+
    scale_fill_gradientn(colors = tile.colors
                         # ,values = c(0,.1,.6,.7,1)
    )+ ## 一般不需要， 可以颜色调整mapping的间隔。从0到1之间的数字向量。
    theme_minimal()+
    # coord_fixed(1)+ ## 锁定纵横比之后会在拼图时出问题。
    xlab(NULL)+ylab(NULL)+
    # geom_shadowtext(aes(label = label), color = "white",
    geom_shadowtext(aes(label = label), color = label.color,
                    nudge_y = label.y.adj,
                    size = label.size, ### 调整border.
                    # geom_text(aes(x = variable, y = ID, label = value, color = "black"),
                    bg.color = label.color.bg)+
    ## 调整图例尺寸
    guides(fill = guide_colourbar(barheight = unit(color.key.height, "cm"),
                                  barwidth = unit(color.key.width,"cm")))+
    scale_y_discrete(position = axis.y.position)+ ## 调整坐标轴位置。
    theme(panel.grid.major=element_blank()
          ,panel.grid.minor=element_blank()
          ,panel.border = element_blank()
          ,legend.title = element_text(size = axis.text.size * 1.3)
          ,legend.text = element_text(size = axis.text.size * 1.1)
          ,legend.position = "right" ## 调整图例位置.
          ,axis.text.x = element_text(size = axis.text.size, angle = 90)
          ,axis.text.y = element_text(size = axis.text.size))


  # ggsave("test newzi trunk.png", plot = heatmap_trunk, height = 13, width = 5)
  heatmap_ls$heatmap_trunk <- heatmap_trunk
  return(heatmap_ls)
}


## 若是初始化一个对象，在初始化的过程中进行赋值，则何如？还是说通过函数进行赋值。
## 函数赋值的缺点在于，若遇到连续调用时，传参比较费劲。
## 要不就分几个图片分别绘制吧。
if(F){

  # 调整tile 的宽、高，并且使空隙保持为固定值/不要有空隙。
  library(pacman)
  library(Loafer)
  get_or_set_dir()
  p_load(reshape2)
  # p_load(shadowText)
  # p_load_current_gh("GuangchuangYu/shadowtext")
  library(shadowtext)
  library(dplyr)
  library(ggplot2)
  getwd()
  ### 需要解决的问题：
  # 使用geom tile 绘制热图的主干。
  # 进行环形绘图(放弃，需要另外写代码及函数)
  # 能够挑一个比较合适的配色，白色放在中间。
  # 使用 ggplot2包绘制热图主干。shadowText包添加注释。计划使用ggtree绘制热图用的聚类树。
  # 使用 aplot包拼接各个图片。
  ## 进行Z变换的函数

  trans_lines(mode = "add #'")


  data("iris")
  assayData <- iris[1:30,1:4] %>% t
  phenoData <- iris[1:30,5, drop = F]
  featureData <-  data.frame(variable = iris %>% colnames %>% `[`(.,1:4), type = c("A", "A", "B", "B"))
  rownames(featureData) <- featureData[,1]

  ExpressionSet <- ExpressionSet(assayData = assayData, ## 要求是matrix格式，并且sample by col and feature by row.
                                 phenoData = new("AnnotatedDataFrame",
                                                 data = phenoData), ## 可以是数据框
                                 featureData = new("AnnotatedDataFrame", data = featureData), ## 可以是数据框。
                                 ## 采用protocol Data存储
                                 protocolData   = new("AnnotatedDataFrame", data = assayData %>% t %>% data.frame), ## 可以是数据框
                                 experimentData = new("MIAME", title = "test"))
  ExpressionSet@assayData$exprs
  ExpressionSet@phenoData@data
  ExpressionSet@featureData@data
  ExpressionSet@protocolData@data

  heatmap_ls <- ggheatmap(
    ExpreessionSet_input = ExpressionSet,
    # dat = iris[1:20,1:4] ,
    # label_df = iris[1:20,1:4] %>% round(., 2),
    Z_by_row_or_col = "row",
    axis.y.position = "right",
    axis.text.size = 18 , ## 调整坐标轴标签的字号（其他字号是在此基础上缩放得到的）
    label.size = 7.8   , ## 调整label的字号
    label.y.adj = .03 , ##  调整label的位置
    label.color = "white" , ## label 的填充色
    label.color.bg = "firebrick" , ## label 轮廓的颜色。
    color.key.height = 5,
    color.key.width  = 1,
    tile.border.col = "black" , ### color for tile border
    tile.colors = c("blue","steelblue","white","orange","red"),
    tile.size = .7,
    tile.border.size = 2)
  # heatmap_ls$heatmap_trunk
  ggsave("test newzi trunk.png", plot = heatmap_ls$heatmap_trunk, height = 5, width = 36)

}
