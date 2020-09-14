#' @title get ggtree form ExpressionSet
#' @details
#' 使用ggtree绘制树形图。
#' 需要实现的功能：
#' 绘制出树形结构带/不带tip.label
#' 调节横向/纵向的间距
#' 同时产出两个树形图（横、竖方向）
#' 调整树的颜色、粗细、线性
#' 调整树的叶子的颜色、形状。
#' @description 像 ggtree 创建的Tree对象添加数据的方法：创建一个数据框。
#' 确保第一列是树的根节点的名字，其他的列为各种属性即可。
#' @param ExpressionSet_input 输入数据集
#' @param heatmap_ls a list to store the heatmap subplots.
#' @param tree.color color for the tree.
#' @param selected_var.feature
#' @param selected_var.pheno
#' @param heighted_v the nodes need to be heighted in branch.
#' @param tip.shape the shape of the nodes
#' @param tip.size the size of the nodes
#' @param colors_c_readonly the colors for mapping.#'
#' @param heighted_nodes 要进行重点强调的Nodes ## 一般不建议与叶子的图形强调一同使用。
#' @param as_tip_fill.feature character ## 要作为填充依据的表型列名
#' @param as_tip_fill.pheno  # 要作为填充色依据的特征列名。
#' @param axis.text.size base size of text
#' @param plot.margin plot.margin
#' @param show_labels logical. If True, the labels will be showned. which can be helpful for user to
#' varify the sub plots.
#' @examples
#' \dontrun{
#' library(pacman)
#' p_load(Loafer)
#' p_load(ggtree)
#' p_load(ggplot2)
#'
#' get_or_set_dir()
#'
#' heatmap_ls <- get_heatmap_tree(ExpressionSet_input = ExpressionSet,
#'                  heatmap_ls = heatmap_ls,
#'                  tree.size = 3,
#'                  tree.color = "red",
#'                  selected_var.feature = NULL, ##  用来强调的叶节点
#'                  selected_var.pheno = NULL,
#'                  heighted_v = NULL,
#'                  tip.shape = 21,
#'                  tip.size = 8,
#'                  colors_c_readonly = colors(distinct = T) %>% `[`
#'                  (.,c(6,325,3,400,5,7,29,38,356, 129, 76, 293, 156)))
#' heatmap_ls$Left_Tree
#' heatmap_ls$Top_Tree
#' }
#' @return heatmap_ls 返回包含左、上两棵树的图片列表结果。
#' @export
get_heatmap_tree <- function(ExpressionSet_input,
                             heatmap_ls,
                             tree.size = 3,
                             tree.color = "red",
                             selected_var.feature = "type", ##  用来强调的叶节点
                             selected_var.pheno = "Species",
                             heighted_v = c("Petal.Length","Petal.Width"),
                             tip.shape = 21,
                             tip.size = 8,
                             show_labels = F,
                             axis.text.size = 20,
                             plot.margin = c(1,1,1,1),
                             colors_c_readonly = colors(distinct = T) %>% `[`
                             (.,c(6,325,3,400,5,7,29,38,356, 129, 76, 293, 156))){

  {
    colors_c <- colors_c_readonly
    # get_left pic:
    r_df <- ExpressionSet_input@assayData$exprs
    df <- ExpressionSet_input@featureData@data
    df <- rownames_to_first_column(df,"rowname")
    hc <- hclust(dist(r_df), "ave") ## hclust method
    pic <- ggtree(hc, size = tree.size, color = tree.color) %<+% df +
      theme(panel.grid.major=element_blank()
            ,panel.grid.minor=element_blank()
            ,panel.border = element_blank()
            ,plot.margin = unit(plot.margin, "cm") ### 调整四周与其他图片的间距
            ,legend.title = element_text(size = axis.text.size * 1.3)
            ,legend.text = element_text(size = axis.text.size * 1.1)
            ,legend.position = "right")

    if(show_labels == T){
      pic <- pic + theme(axis.text.x = element_text(size = axis.text.size, angle = 90)
                         ,axis.text.y = element_text(size = axis.text.size))
    }

    ## 树的线宽和树的颜色。
    heighted_nodes <- pic$data$label %in% heighted_v %>% pic$data$node[.]
    if(heighted_nodes %>% length > 0){
      ## 是否添加其他的特征。
      for(each_node in heighted_nodes){
        pic <- pic + geom_highlight(node = each_node, fill = "red", alpha = .4, extend = 0)
      }
    }
    shape_c <- c()
    levels <- df[,selected_var.feature] %>% unique
    shape_c <- rep(tip.shape, length(levels))
    colors_c <- colors_c[1:length(levels)]
    names(shape_c) <- names(colors_c) <- levels

    if(is.null(selected_var.feature) %>% `!`){
      data_for_pic <- pic$data %>% as.data.frame
      data_for_pic <- data_for_pic$label %>% is.na() %>% `!` %>% data_for_pic[.,]
      pic <- pic +  #+
        # geom_tiplab(aes(color = 1, fill = c(2))) +
        geom_point(data = data_for_pic, aes_string(fill = selected_var.feature,
                                                   shape = selected_var.feature),
                   size = tip.size, na.rm = T) +
        scale_shape_manual(values = shape_c)+ ## 需要使用breaks 来限制不展示NA。
        scale_fill_manual(values = colors_c)
    }
    # pic
    heatmap_ls$Left_Tree <- pic
  }

  # heighted_v <- c(23, 29)

  { ## get top tree:
    colors_c <- colors_c_readonly
    r_df <- ExpressionSet_input@assayData$exprs
    df <- ExpressionSet_input@phenoData@data
    df <- rownames_to_first_column(df,"rowname")
    # df %>% class
    # df$Species <- df$Species %>% as.character()
    # df$Species[1:5] <- "New class"
    # df$New_test <- "111"
    # df %>% str
    # df
    hc <- hclust(dist(r_df %>% t), "ave")
    pic <- ggtree(hc, size = tree.size, color = tree.color) %<+% df  + layout_dendrogram()+
      theme(panel.grid.major=element_blank()
            ,panel.grid.minor=element_blank()
            ,panel.border = element_blank()
            ,plot.margin = unit(plot.margin, "cm") ### 调整四周与其他图片的间距
            ,legend.title = element_text(size = axis.text.size * 1.3)
            ,legend.text = element_text(size = axis.text.size * 1.1)
            ,legend.position = "right")

    if(show_labels == T){
      pic <- pic + theme(axis.text.x = element_text(size = axis.text.size, angle = 90)
                         ,axis.text.y = element_text(size = axis.text.size))
    }

    ## 树的线宽和树的颜色。
    pic$data
    heighted_nodes <- pic$data$label %in% heighted_v %>% pic$data$node[.]
    if(heighted_nodes %>% length > 0){
      ## 是否添加其他的特征。
      for(each_node in heighted_nodes){
        pic <- pic + geom_highlight(node = each_node, fill = "red", alpha = .4, extend = 0)
      }
    }
    # pic
    shape_c <- c()
    levels <- df[,selected_var.pheno] %>% unique
    shape_c <- rep(tip.shape, length(levels))
    colors_c <- colors_c[length(colors_c) + 1 - 1:length(levels)]
    names(shape_c) <- names(colors_c) <- levels
    # df %>% head
    # pic
    # pic$data$Species
    if(is.null(selected_var.feature) %>% `!`){
      data_for_pic <- pic$data %>% as.data.frame
      data_for_pic <- data_for_pic$label %>% is.na() %>% `!` %>% data_for_pic[.,]
      pic <- pic +
        # geom_tiplab(aes(color = 1, fill = c(2))) +
        geom_point(data = data_for_pic, aes_string(fill = selected_var.pheno,
                                                   shape = selected_var.pheno),
                   # geom_point(aes_string(fill = "Species", shape = "Species"),
                   # geom_point(aes(fill = Species, shape = Species),
                   # geom_point( #aes_string(fill = "Species", shape = "Species"),
                   size = tip.size, na.rm = T) +
        scale_shape_manual(values = shape_c) + ## 需要使用breaks 来限制不展示NA。
        scale_fill_manual(values = colors_c)

    }
    heatmap_ls$Top_Tree <- pic
  }
  return(heatmap_ls)
}

