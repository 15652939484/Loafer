## code to prepare `DATASET` dataset goes here

setwd("J:/Github_Repository/Loafer/data-raw/0_data_source")
pulled_df <- read.xlsx("希腊字母和英文读写的 对照表.xlsx")
Greek_to_Enl_df <- pulled_df
usethis::use_data(Greek_to_Enl_df, overwrite = TRUE)
## 不导出的数据
usethis::use_data(Greek_to_Enl_df, overwrite = TRUE, internal = T)
load("pulled_df.rda")
load('R/sysdata.rda')
data("pulled_df.rda")
