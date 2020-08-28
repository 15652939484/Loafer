if(F){
  library(openxlsx)
  library(reshape2)
  getwd()

  rbind(pulled_df[,c(1,3)], pulled_df[,c(2,3)])
  # data 中的数据用户也可以看见
  # 在
  ?read_html

  ### Load the pulled_df data
  load("data/Greek letters to English.rda")
  input <- c("φ", "χ", "π")
  Greek_to_Enl(input = input)

  # devtools::(pulled_df, pulled_df, internal = T)
  ?use_data_raw

  pulled_df <- read.xlsx("data/希腊字母和英文读写的 对照表.xlsx")
  save(pulled_df,file = "data/Greek letters to English.rda")
  Greek_to_Enl <- function(input){ ## trans greek (i character vector) into Enl.
    dictionary <- pulled_df %>% melt(., "pinyin")
    matched <- input %in% dictionary$value
    position_in_dic <- input[matched] %>% match(., dictionary$value)
    input[matched] <- position_in_dic  %>% dictionary$pinyin[.]
    return(input)
  }


  pulled_df <- NULL



}
