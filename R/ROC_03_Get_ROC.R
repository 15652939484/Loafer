#' @title Draw the lines by using the real_I and pred_num of the ROC.
#' ## 头文档和函数之间仅可以放注释，不可以添加其他代码，哪怕是if(F)也不行，
#' 会妨碍函数名的获取导致文档问题。
#' @param real_I the real group of a each sample in dataset
#' @param pred_num the predicted num of each sample
#' @param fit_note the note to be used in the filename to seperate the results
#' @param IS_smooth wether to smooth the ROC lines.
#' @importFrom pROC roc
#' @examples
#' \dontrun{
#' pred_num <- c(0.9,0.1,0.33,0.87)
#' real_I <- c(0,0,1,1)
#' plot_roc(real_I = real_I, pred_num = pred_num)
#' }
#' @export

# pred_num <- c(0.9,0.1,0.33,0.87)
# real_I <- c(0,0,1,1)
# plot_roc(real_I = real_I, pred_num = pred_num)

plot_roc_by_pROC <- function(real_I, pred_num,
                     fit_note = "ROC plot",
                     IS_smooth = F){

    ### 创建文件夹：
    # dir.create(file_path, recursive = T, showWarnings = F)
    filename <-  paste(fit_note,".pdf")
    pdf(file = filename, family = "Times")
    roc_by_proc <- pROC::roc(response  = real_I,
                             predictor = pred_num,
                             percent=TRUE, ## 使用百分比还是使用什么
                             smooth=IS_smooth, ## 是否平滑
                             ### plot 通用参数
                             cex.axis = 1.15, ## 坐标轴标签
                             cex.lab = 1.2, ##  坐标轴标题
                             cex.main = 1, ## 标题字体大小
                             # arguments for auc
                             # partial.auc=c(100, 90),
                             # partial.auc.correct=TRUE,
                             # partial.auc.focus="sens",
                             # arguments for ci

                             # xlim=c(100,0),
                             ci=TRUE, ### 是否生成置信区间，
                             boot.n=1000, ## 迭代次数。
                             ci.alpha=0.9,
                             # stratified=T,
                             print.ci.cex = 1.1,
                             stratified=FALSE,
                             # arguments for plot
                             plot=TRUE,
                             ### AUC的区域
                             auc.polygon=T,
                             auc.polygon.col = "grey96",
                             max.auc.polygon.col = "white", ### 整个范围的颜色
                             max.auc.polygon=T,
                             grid=TRUE,
                             print.auc=TRUE,
                             show.thres=T,
                             # legacy.axes=TRUE
                             legacy.axes=T, ## 控制横坐标轴显示特异度还是1-特异度
                             ### 是否绘制最佳切分点
                             print.thres = T,  ## 是否绘制
                             print.thres.pch = 19,  ## 切分点的大小
                             # print.thres.adj = c(- 0.1,-0.1),  ## 水平调节文字位置
                             # print.thres.col = "red",
                             # print.thres.fil = "black",
                             print.thres.cex = 1.1
    )
    dev.off()
    cat("\nROC has been drawn and saved in the ",filename,"\n")
}




#' @title Get roc by module obj via pROC package.
#' @details
#' 同時繪製訓練集和驗證集的ROC曲線
#' @param module_obj inheritParams from get_the_auc
#' @importFrom pROC roc
#' @inheritParams plot_roc_by_pROC
#' @param Note control the filename in order to change the file names.
#' @export
plot_roc_from_module_obj <- function(module_obj, IS_smooth = F, Note = "say sth"){
    plot_roc_by_pROC(real_I = module_obj$for_plot$train$real_I,
                     pred_num = module_obj$for_plot$train$pred_num,
                     fit_note = module_obj$method_note %>% paste("train data ROC",Note,.),
                     IS_smooth = IS_smooth)

    plot_roc_by_pROC(real_I = module_obj$for_plot$test$real_I,
                     pred_num = module_obj$for_plot$test$pred_num,
                     fit_note = module_obj$method_note %>% paste("test data ROC",Note,.),
                     IS_smooth = IS_smooth)
}
