#' @title trans meiji(美吉) 16S OTU data into normal data
#' @details
#' Substract
#' 主要是将几种无明确分类的部分去掉，并且将各个层级的名称进行合并
#' 同时还将重复的物质进行了去重
#' 此外，可能会有一个本来就是空的（从第二层开始就鉴定不到东西），OTU，会被自动去掉。
#' @param xlsx.filename file name of the xlsx data.
## 输入xlsx格式的文件
#' @export
meiji_otu_to_human <- function(xlsx.filename = "文件名.xlsx"){
    df <- read.xlsx(xlsx.filename)
    # df %>% str
    # df
    df[,1:8] <- df[,1:8] %>% apply(., 2, function(each_col){
        each_col <- each_col %>% sub("[a-z]__(norank|unclassified|uncultured).*", "others", .)
    }) %>% data.frame(., check.names = F)
    df %>% head
    df %>% colnames
    # pulled_name <- df[,3:8] %>% apply(., 1, function(each_row){
    #     each_row %>% paste(., collapse = ";") %>% sub(";+$", "", .)
    # }) %>% data.frame(pulled_name = ., check.names = F)
    # # # pulled_name[1:5,]
    # # # df %>% head
    # # pulled_df <-  cbind(pulled_name, df)
    # pulled_df %>% head
    # # pulled_df %>% str
    # # pulled_df %>% colnames
    #
    # pulled_df <- pulled_df %>% ddply(., "pulled_name", function(each_df){
    #     temp_line <- each_df[,11:ncol(each_df)] %>% apply(., 2, function(each_col){
    #         each_col %>% sum(., na.rm = T)
    #     })
    #     each_df[1,11:ncol(each_df)] <- temp_line
    #     return(each_df[1,])
    # })
    # pulled_df <- pulled_df[pulled_df$pulled_name != "",]
    # pulled_df[pulled_df$pulled_name == "",]
    # pulled_df %>% head

    final_df <- NULL
    L <- c("Phylum", "Class", "Order", "Family", "Genus", "Species")
    for(i in 1:6){
        # each_name <- "Phylum"
        each_name <- L[i]
        # df %>% head
        new_df <- df %>% ddply(., each_name, function(each_df){
            temp_line <- each_df[,10:ncol(each_df)] %>% apply(., 2, function(each_col){
                each_col %>% sum(., na.rm = T)
            })
            each_df[1,11:ncol(each_df)] <- temp_line
            return(each_df[1,])
        })

        new_df %>% head
        # new_df[,2 + 1:i] %>% head
        new_df$realname <- new_df[,2 + 1:i, drop = F] %>%  apply(., 1, function(each_row){
            each_row %>% paste(., collapse = ";") %>% sub(";+$", "", .)
        })
        if(i < 6){
            new_df[,(2+i+1):8] <- ""
        }
        final_df <- rbind(final_df, new_df)
    }
    final_df %>% head
    # final_df
    # (final_df$realname %>% table > 1) %>% sum
    final_df <- final_df[,c("realname", colnames(final_df)[2:ncol(final_df) -1]) ]
    ### 没有重复的。
    # final_df <- final_df %>% ddply(., "realname", function(each_df){
    #     temp_line <- each_df[,11:ncol(each_df)] %>% apply(., 2, function(each_col){
    #         each_col %>% sum(., na.rm = T)
    #     })
    #     each_df[1,11:ncol(each_df)] <- temp_line
    #     return(each_df[1,])
    # })
    new_filename <- paste0(xlsx.filename, "名称处理 + 重复合并后.xlsx")
    write.xlsx(final_df, new_filename)
}


# get_or_set_dir()
#
# xlsx.filename =  "OTU表2020-09-25—美吉抽平表.xlsx"
# meiji_otu_to_human(xlsx.filename = filename)
