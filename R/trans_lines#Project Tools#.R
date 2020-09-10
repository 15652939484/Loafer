#' @title trans mutiple lines and remove or add sth.
#' @description 将多行代码的行首 统一 添加/去除 "#' "前缀## 
#' 或者将多行代码统一 添加/去除 行末的 “,". 注意, 去除的时候，可以直接替换成空并且换行，也可以直接替换为”：”
#' 同时，需要注意，在注释中的内容不需要换行。#
#' 在苹果中恐怕无法正常使用。因为苹果系统的剪贴板函数略有不同。
#' @param input_lines input lines. Clipboard containings as default input. ## 输入的变量
#' @param mode add_#', remove_#', add "," or remove ","
#' @return lines with decoration.
#' @export
#' 
trans_lines <- function(input_lines = readClipboard(), 
                        mode = "add #'"){
    mode <- mode %>% match.arg(., c("add #'", "add ,", "remove #'", "remove ,"))
    if(mode == "add #'"){output_lines <- input_lines %>% paste0("#' ", .)}
    if(mode == "remove #'"){output_lines <- input_lines %>% sub("^#' ", "", .)}
    if(mode == "add ,"){
        output_lines <- input_lines %>% sub("(#[^']|#$)", ", \\1", .)
        clean_lines <- output_lines %>% grepl("#",.) == F
        output_lines[clean_lines] <- output_lines[clean_lines] %>% paste0(.,",")
        output_lines[length(input_lines)] <- input_lines[length(input_lines)] ## 最后一行不需要替换
        # output_lines
    }
    if(mode == "remove ,"){
        output_lines <- input_lines %>% gsub("(#[^']|#$).*","",.) %>% gsub(",","\n",.) %>% sub("\n *$","",.)
    }
    writeClipboard(output_lines)
    output_lines
}
