#' Get S4 obj ExperimentData from MP data file directly
#' 从麦特绘谱的结果文件直接生成S4对象。
#'
#'
#' depracted param Folder_path 若给出文件夹所在的位置。 直接从中解析出需要的数据,感觉用处不大，不够灵活
#'           param Treat_involved  涉及到的Treats，不够灵活，手动搞定文件吧
#' @param Allmet_file # allmet data file name
#' @param group_info_file group info file name
#' @param groups_involved_c groups involved in the analysis.
#' @param obj_note note for the obj
#' @param ClassNote_involved_c charactor vector, select groups need to be imports
#' @param remove_QC charactor vector, remove the QC if necessary.
#' @return 返回一个Experiment 的S4对象
#' @importFrom Biobase ExpressionSet
#' @examples
#' \dontrun{
#' ## 搞几个数据文件。放在工作目录中，然后执行下述代码：
#' getwd()
#' import_MP_datas(Allmet_file = "04_AllMet 训练集.csv",
#'     group_info_file = "Class_Info 训练集.csv",
#'     obj_note = "The note")
#' exprs_m <- ExpressionSet@assayData$exprs ## expr data.
#' pheno_df <- ExpressionSet@phenoData@data ## phenoData data.
#' feature_df <- ExpressionSet@featureData@data ## feature data.
#'
#' }
#' @export

# library(Loafer)
# get_or_set_dir()
## From Folder

# ExpressionSet <-  import_MP_datas(Allmet_file = "04_AllMet 训练集.csv",
#     group_info_file = "Class_Info 训练集.csv",
#     obj_note = "The note")
# ExpressionSet$SampleID
# ExpressionSet$ClassNote
# exprs_m <- ExpressionSet@assayData$exprs ## expr data.
# pheno_df <- ExpressionSet@phenoData@data ## phenoData data.
# feature_df <- ExpressionSet@featureData@data ## feature data.
#
#
# Allmet_file <- "04_AllMet 训练集.csv"
# group_info_file <- "Class_Info 训练集.csv"
# obj_note <- "The note"
# selector_file <- "Selector.csv"
# groups_involved_c <- c("")
# ClassNote_involved_c <- c("Control","PD")
# samples <- phenoData$ClassNote %in% ClassNote_involved_c %>% rownames(phenoData)[.]


import_MP_datas <-  function(Allmet_file, group_info_file, obj_note,
                             remove_QC = c("QC","qc","Qc"),
                             ClassNote_involved_c = phenoData$ClassNote %>% unique()){


    Allmet_df <- Allmet_file %>% read.csv(., check.names = F, stringsAsFactors = F)
    ## Get feature_df
    featureData <- Allmet_df[,1:4]
    rownames(featureData) <- Allmet_df[,4]
    ## Get assay data:
    assay_df <- Allmet_df[,-c(1:4)]
    rownames(assay_df) <- Allmet_df[,4]
    assayData <-  assay_df %>% as.matrix()## turn into matrix
    # assayData_new <-  assay_df ## turn into matrix
    # assayData %>% class
    ## Get group_info data
    phenoData <- group_info_file %>% read.csv(., check.names = F, stringsAsFactors = F)

    ClassNote_involved_c <- ((ClassNote_involved_c %in% remove_QC) == F) %>% ClassNote_involved_c[.]
    phenoData <- phenoData$ClassNote %in% ClassNote_involved_c %>% phenoData[.,]
    rownames(phenoData) <- phenoData[,1]
    assayData <- colnames(assayData) %in% rownames(phenoData) %>% assayData[,.]

    sth_to_print <- paste("Group involved was:", ClassNote_involved_c %>% paste(., collapse = " and "))
    print(sth_to_print)

    ExpressionSet <- Biobase::ExpressionSet(assayData = assayData, ## 要求是matrix格式，并且sample by col and feature by row.
                                      phenoData = new("AnnotatedDataFrame",
                                                      data = phenoData), ## 可以是数据框
                                      featureData = new("AnnotatedDataFrame", data = featureData), ## 可以是数据框。
                                      experimentData = new("MIAME",
                                                           title = obj_note))
    return(ExpressionSet)
}


# get_or_set_dir()
# use_data(ExpressionSet_train, internal = F, overwrite = F)
# load("ExpressionSet_train.rda")
