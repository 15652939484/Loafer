#' @title Get ls from exl module.
#' 从指定格式的模板文件中获取注释。
#' 进行相关分析的函数汇总。
#' 主要针对菌群数据做了一定的优化，化简需要的操作，并且自动处理一部分问题。
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

Get_ls_from_exl_5.0 <- function(file_name = "filename for module.xlsx",
                                sep_code =  "|",
                                adj_index =  1,
                                remove_letter__  = T,
                                extract_method = "sig in group", ### "sig in group", 仅将各组的差异提取出来。"sig pulled" 将各个组的差异的并集提取出来。
                                match_mode = "cleaned_match",
                                ### 匹配方式，默认使用"cleaned_match"## 后面还会增加一个 tail_match. 使用最后一个截断符及其后面的字符进行截断匹配。
                                get_level_by = "sep_code", ### 选择 "sep_code" 时使用分隔符来进行匹配。选择"letter__"时采用各个层级+下划线的形式来确定level。
                                # alternative_code  = "meta" ,### 指定的其余代码，当费菌群数据是会使用。
                                IS_microbiota = F){ ### 是否是菌群数据,若是，选择此选项。

  if(T){  ### 对象的初始化

    ### 对象的初始化
    my_ls <- list()
    # my_ls$input
    my_ls$input <- list()
    my_ls$input$file_name <- file_name
    ### 当数据非菌群时，会使用这个标记 ## 使用文件名生成这个，先去除.xlsx尾部，再在开头添加空格。
    my_ls$input$alternative_code <-  file_name %>% sub(".xlsx$","",.) %>% paste0(" ",.) %>% sub("模板.*","",.) ## 再去掉模板两个字。
    ## 菌群的变量
    my_ls$input$IS_microbiota <- IS_microbiota
    my_ls$input$sep_code <- sep_code
    my_ls$input$adj_index <- adj_index
    my_ls$input$remove_letter__ <- remove_letter__
    my_ls$input$get_level_by <- get_level_by
    ## 匹配的依据方法
    my_ls$input$match_mode <- match_mode
    my_ls$input$extract_method <- extract_method
    # my_ls
  }

  # str(my_ls)
  # 获取差异提取信息
  my_ls <- get_sigs_Loafer(my_ls)
  # my_ls$Variable_for_selection %>% head


  # 获取分组信息
  my_ls <- get_group_info_Loafer(my_ls)
  # my_ls$Variable_for_selection

  # 获取原始数据，菌群数据时，自动检查并提出结尾的分隔符。
  my_ls <- get_raw_data_Loafer(my_ls)
  # 依据分组信息中的"_-_"，创建新的分组信息、变量名、以及求值
  if(my_ls$note$IS_dif_required == T){ ### 如果需要，就进行新变量的求值。
    my_ls <- get_dif_data_Loafer(my_ls)
  }

  ## 获取菌的层级信息
  if(IS_microbiota == T){
    my_ls <-  get_microbiota_levels_Loafer(my_ls)
    ## 提取变量，并且依据菌的层级进行数据切分
  }
  my_ls <- match_and_extract_Loafer(my_ls)

  return(my_ls)
}



get_sigs_Loafer <- function(my_ls = my_ls){
  file_name <-  my_ls$input$file_name
  ### 處理待提取变量的数据
  data_to_split <-  read.xlsx(file_name, sheet = "差异提取")
  data_to_split <-  data_to_split[,-1, drop = F]
  group_involved <-  data_to_split[1,] %>% t() %>% c() %>% na.omit()
  data_width <-  length(group_involved)


  ### 使分组中的数据重排列，保障跨组别比较的一致性。
  my_ls$note$group_involved <- group_involved
  # my_ls$group_involved <-  group_involved
  Test_Code <- data_to_split[2,] %>% t() %>% c() %>% na.omit()
  my_ls$note$Test_Code <- Test_Code
  Variable_for_selection <-  read.xlsx(file_name, sheet = "差异提取", startRow = 4)
  Variable_for_selection <-  Variable_for_selection[,-1, drop = F]
  Variable_for_selection <-  Variable_for_selection[, 1:data_width,drop = F]
  dim(Variable_for_selection)
  # head(Variable_for_selection) ### 获取对应的
  my_ls$Variable_for_selection <- Variable_for_selection

  ### New 提取要产生的新组别：
  my_ls <- get_dif_values_Loafer(my_ls = my_ls)

  return(my_ls)
}


get_group_info_Loafer <- function(my_ls = my_ls){
  group_info <- read.xlsx(my_ls$input$file_name, sheet = "分组信息")
  my_ls$note$group_info <-  group_info
  return(my_ls)
}

# my_ls$note
get_raw_data_Loafer <- function(my_ls = my_ls){


  raw_data <- read.xlsx(my_ls$input$file_name, sheet = "原始数据", check.names = F, rowNames = T)
  ## remove tail sep code ## 去除末尾的“;" 或者”|"或者“."
  if(my_ls$input$IS_microbiota == T){
    rownames(raw_data) <- raw_data %>% rownames %>% sub("[;|.]$", "", .)
  }
  my_ls$raw_data <-  raw_data
  return(my_ls)
}


get_dif_values_Loafer <- function(my_ls = my_ls){
  IS_dif_required <-  my_ls$note$group_involved %>% grepl("_-_",.) %>% sum() > 0
  my_ls$note$IS_dif_required <- IS_dif_required ### 根据这个变量来判断是否要进行做差。
  if(IS_dif_required){
    groups_to_cal_v <- my_ls$note$group_involved  %>% grepl("_-_",.) %>% my_ls$note$group_involved[.] %>% unique()

    paired_groups_df <- get_paired_groups(groups_to_cal_v = groups_to_cal_v)
    my_ls$note$paired_groups_df <-  paired_groups_df

  }
  return(my_ls)
}


get_dif_data_Loafer <- function(my_ls){ ### 根据前述信息，生成衍生的表格，将新的分组信息、样本名称添加到分组文件和原始数据文件的后面。
  group_info_df <- my_ls$note$group_info #应当和之前的公司结果一致
  group_info_df <- group_info_df$SampleID %in% colnames(my_ls$raw_data) %>% group_info_df[.,]
  final_group_df <- group_info_df
  for( i in 1:ncol(my_ls$note$paired_groups_df)){
    each_group_pair <- my_ls$note$paired_groups_df[,i] ### 分组，第一个的组别减第二个的分组。

    part_end <- group_info_df$Pair_ID[group_info_df$ClassNote == each_group_pair[1]]
    part_start <- group_info_df$Pair_ID[group_info_df$ClassNote == each_group_pair[2]]

    sample_end <- group_info_df$SampleID[group_info_df$ClassNote == each_group_pair[1]]
    sample_start <- group_info_df$SampleID[group_info_df$ClassNote == each_group_pair[2]]



    core_Pair_ID <- intersect(part_end,part_start)

    ### 拼接分组文件
    new_group_df <- data.frame(
      SampleID =  paste0(sample_end,"_minus_",sample_start),
      ClassNote = each_group_pair %>% paste0(.,collapse = "_-_"),
      Pair_ID = core_Pair_ID,
      stringsAsFactors = F)

    # str(new_group_df)
    final_group_df <- rbind(final_group_df, new_group_df)

    my_ls$note$group_info <- final_group_df

    ### 根据前面的样本，做差即可。
    df_end <- my_ls$raw_data[,sample_end]



    df_start <- my_ls$raw_data[,sample_start]
    df_diff <- df_end-df_start
    colnames(df_diff) <- new_group_df$SampleID
    final_df <- cbind(my_ls$raw_data,df_diff)
    my_ls$raw_data <- final_df
  }
  return(my_ls)
}

get_microbiota_levels_Loafer <- function(my_ls){
  IS_microbiota  <-  my_ls$input$IS_microbiota
  raw_data <- my_ls$raw_data
  sep_code <- my_ls$input$sep_code
  get_level_by <- my_ls$input$get_level_by
  adj_index  <- my_ls$input$adj_index
  alternative_code <- my_ls$input$alternative_code

  if(IS_microbiota == T & get_level_by == "sep_code" ){ ### 提取等级的依据， 根据分隔符数量还是根据字母加下划线来提取
    taxon_level_code  <- get_levels_for_taxons_Loafer(taxon_name = rownames(raw_data),
                                                      sep_code =  sep_code,
                                                      adj_index =  adj_index)
  } else if(IS_microbiota == T & get_level_by == "letter__"){
    taxon_level_code  <-  get_levels_for_taxons_by_letter___Loafer (
      taxon_name = rownames(raw_data),
      sep_code =  sep_code
    )
  }

  ### 将等级中的 LNA替换为Other.
  taxon_level_code <- taxon_level_code %>% sub("LNA", "Other",.)
  # taxon_level_code

  ### 若非菌群数据，则不使用相应的等级变量。
  if(IS_microbiota == F){
    taxon_level_code <-  rep(alternative_code,nrow(raw_data))
  }

  my_ls$note$taxon_level_code <- taxon_level_code
  return(my_ls)
}
## 提取变量，并且依据菌的层级进行数据切分

match_and_extract_Loafer <- function(my_ls){
  # 建立一个mapping字典，负责所有变量到raw_table的mapping
  all_to_be_matched <- my_ls$Variable_for_selection %>% as.list() %>% unlist() %>% unique() %>% na.omit()
  my_ls$raw_data %>% head
  match_mode <-  my_ls$input$match_mode

  for(i in 1:length(my_ls$Variable_for_selection)){
    each_note <-  names(my_ls$Variable_for_selection)[i]
    each_v <-  my_ls$Variable_for_selection[[each_note]] %>% na.omit()
    sig_between_that_groups <- each_v ### 实际的组间差异的

    if(my_ls$input$extract_method == "sig pulled"){ ### 增加一个控制提取方式/依据的参数
      ### 如果用的提取模式是将所有的数据全部提取出来，
      full_sig_v <- my_ls$Variable_for_selection %>% unlist %>% unique %>% na.omit()
      each_v <- full_sig_v ## 如果使用全部的
    }

    each_group <- my_ls$note$group_involved[i]
    ## 获取待匹配数据在原始值中的匹配对应位置

    if(match_mode == "cleaned_match"){
      sig_matched_rownum <- get_match_by_cleaned_name_Loafer(cleaned_met = sig_between_that_groups,
                                                             remove_letter__ = remove_letter__,
                                                             cleaned_raw = rownames(my_ls$raw_data))
      matched_rownum <- get_match_by_cleaned_name_Loafer(cleaned_met = each_v,
                                                         remove_letter__ = remove_letter__,
                                                         cleaned_raw = rownames(my_ls$raw_data))


    } else if (match_mode == "tail_match"){

      sig_matched_rownum <- get_match_by_cleaned_tail_Loafer( cleaned_met = sig_between_that_groups, ## 待匹配物质
                                                              remove_letter__ = remove_letter__,
                                                              cleaned_raw = rownames(my_ls$raw_data))
      matched_rownum <- get_match_by_cleaned_tail_Loafer( cleaned_met = each_v, ## 待匹配物质
                                                          remove_letter__ = remove_letter__,
                                                          cleaned_raw = rownames(my_ls$raw_data))
    }
    ### 对应组差异的变量匹配到的名字
    sig_name <- rownames(my_ls$raw_data)[sig_matched_rownum]
    my_ls$output[["Sig_variables_matched"]][[each_note]] <- sig_name

    matched_levels <- my_ls$note$taxon_level_code[matched_rownum]
    existed_levels <- matched_levels %>% unique() %>% sort()
    if(is.null(existed_levels)){
      existed_levels <- ""
      matched_levels <- ""
    } ### 当没有层级时，填充空值

    my_ls$final_variable <- list()
    for(each_levels in existed_levels){ ### 按照层级提取数据。
      temp_v <- matched_levels == each_levels
      matched_v <- matched_rownum[temp_v] %>% rownames(my_ls$raw_data)[.]

      final_code <- paste0(each_note, each_levels)
      final_group <- each_group
      my_ls$output$final_group[[final_code]] <- each_group
      my_ls$final_variable[[final_code]] <- matched_v

      # my_ls$final_variable %>% str()
      #### 执行函数进行提取。
      if(T){ ### 依据依据进行提取, 只需要把"_-_“连接的通过计算变成实际存在的分组即可。

        group_to_be_extracted <- each_group %>% strsplit2(., split =  "_\\+_") %>% as.list() %>% unlist()

        # group_to_be_extracted <- group_to_be_extracted[[1]]
        matched_sample <- my_ls$note$group_info$ClassNote %in% group_to_be_extracted %>% my_ls$note$group_info$SampleID[.]
        selected_sample <-  colnames(my_ls$raw_data) %in% matched_sample

        my_ls$final_sample[[final_code]] <- matched_sample

        my_ls$output$final_data[[final_code]]   <- my_ls$raw_data[my_ls$final_variable[[final_code]] ,selected_sample, drop = F]
      }
    }
  }
  return(my_ls)
}



get_levels_for_taxons_Loafer <- function(taxon_name = rownames(raw_data),
                                         sep_code =  "|",
                                         adj_index =  1){
  gsub_text <- paste0("[^",sep_code,"]+")
  taxon_level <- taxon_name %>% gsub(gsub_text,"",.) %>% nchar() + adj_index
  taxon_level_code <-  paste0("L",taxon_level)
  return(taxon_level_code)
}


get_levels_for_taxons_by_letter___Loafer <- function(taxon_name = rownames(raw_data),
                                                     sep_code =  "|"){
  # gsub_text <- paste0("[^",sep_code,"]+")
  gsub_text <- paste0(".*[",sep_code,"]+ ?")### 可以允许分隔符后面有空格。
  taxon_name[20]
  taxon_name %>% gsub(gsub_text,"",.)
  letter__ <- taxon_name %>% gsub(gsub_text,"",.) %>% StrLeft(.,3)
  taxon_level <- factor(letter__, levels = c("d__","k__","p__","c__","o__","f__","g__","s__","t__"),
                        labels = 0:8
  )
  # taxon_level
  taxon_level_code <-  paste0("L",taxon_level)

  return(taxon_level_code)
}

get_match_by_cleaned_name_Loafer <- function(cleaned_met = each_v,
                                             remove_letter__  = T,  ### 控制匹配前是否去除掉 "letter__"
                                             cleaned_raw = rownames(raw_data)){
  cleaned_met.readonly <- cleaned_met
  cleaned_met <- cleaned_met  %>% gsub("[^a-zA-Z0-9]+","",.) %>% gsub("_","",.)
  cleaned_raw <- cleaned_raw  %>% gsub("[^a-zA-Z0-9]+","",.) %>% gsub("_","",.)

  # if(remove_letter__ == T){
  #     cleaned_met <- cleaned_met %>% gsub("[a-z]__","",.)
  #     cleaned_raw <- cleaned_raw %>% gsub("[a-z]__","",.)
  # }


  # cleaned_met %in% cleaned_raw

  matched_rownum <- match(cleaned_met, cleaned_raw)
  if( (is.na(matched_rownum) %>% sum() )> 0 ) {

    unmatched <- is.na(matched_rownum) %>% cleaned_met.readonly[.] %>% paste0(., collapse = "\n")
    cat("如下变量未能成功匹配:\n",unmatched, sep = "")

    stop("快点瞅瞅咋回事儿呀,大兄弟！\n")
  }
  return(matched_rownum)

}


get_match_by_cleaned_tail_Loafer <- function(cleaned_met = each_v, ## 待匹配物质
                                             remove_letter__  = T,
                                             cleaned_raw = rownames(raw_data)){ ### 总表，到时候从这里提取数据

  cleaned_met.readonly <- cleaned_met
  cleaned_met <- cleaned_met  %>% gsub("[^a-zA-Z0-9]+","",.) %>% gsub("_","",.)
  cleaned_raw <- cleaned_raw  %>% gsub("[^a-zA-Z0-9]+","",.) %>% gsub("_","",.)

  for(i in 1:length(cleaned_met) ){
    if(i == 1){matched_rownum <- c()}
    each_meta <- cleaned_met[i]
    char_length <-  nchar(each_meta)### 获取字符串长度
    cutted_tail_c <- StrRight(x = cleaned_raw, char_length)
    matched_rownum[i] <-  match(each_meta, cutted_tail_c)
  }

  if( (is.na(matched_rownum) %>% sum() )> 0 ) {
    ### 将多个未匹配变量的原始值都输出来。
    unmatched <- is.na(matched_rownum) %>% cleaned_met.readonly[.] %>% paste0(., collapse = "\n")
    cat("如下变量未能成功匹配:\n",unmatched, sep = "")

    stop("快点瞅瞅咋回事儿呀,大兄弟！\n")
  }
  return(matched_rownum)
}
