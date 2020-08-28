### 用来生成相应的数据文件。通过 use_data进行保存即可。

# source_path
library(pacman)
p_load(usethis)
p_load(openxlsx)

# setwd(source_path)
get_or_set_dir()
getwd()
# setwd("J:/Github_Repository/Loafer/inst/extdata")
# pulled_df <- read.xlsx("希腊字母和英文读写的 对照表.xlsx")
Greek_to_Enl_df <- read.xlsx("希腊字母和英文读写的 对照表.xlsx")
# save(pulled_df,file = "data/Greek letters to English.rda")
## 不导出的数据
usethis::use_data(Greek_to_Enl_df, overwrite = TRUE, internal = T)
# remove.packages("Loafer")
# p_load_current_gh("15652939484/Loafer")
