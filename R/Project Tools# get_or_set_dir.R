#' @title  get or set the dir by Rstudio
#' @details
#'     Get the dir where the script were lying in.
#'     Set the dir as main_path and create some sub file folder inside the dir.
#'     Chose one file as the new working dir.
#'@param main_trunk_as_wd, Boolean, IF T, the main dir will be set as new wd.
#'@param source_path, character, where raw data will be stored in a project.
#'@param output_path, character, where the output of an analysis will be saved.
#'@param other_dirs, character vector with var name.Can be NULL. the name will be assigned and be returned to global env.
#'@param Change_wd, Boolean; Wether set source path as the working dir.
#'@return return a summary about working dir and the source and output dir.
#'@examples
#'\dontrun{# set the dir as wd:
#'get_or_set_dir(main_trunk_as_wd = T,
#'    source_path = "0_data_source/",
#'    output_path = "2_output/",
#'    other_dirs = c(function_path = "DIY Function/", other_dir_test = "Test dir"),
#'    Change_wd = F)
#'# set the source as wd:
#' get_or_set_dir(main_trunk_as_wd = F,
#'    source_path = "0_data_source/",
#'    output_path = "2_output/",
#'    other_dirs = c(function_path = "DIY Function/", Dir_test = "Test dir"),
#'    Change_wd = T)
#' get_or_set_dir(main_trunk_as_wd = F,
#'    source_path = "0_data_source/",
#'    output_path = "2_output/",
#'    other_dirs = NULL,
#'    Change_wd = T)
#'}
#'@export

## export is required for the functions export to users.

## Need: rstudioapi###
get_or_set_dir <- function(main_trunk_as_wd = F,
                           source_path = "0_data_source/",
                           output_path = "2_output/",
                           other_dirs = NULL,
                           Change_wd = T){
  script_dir <- rstudioapi::getActiveDocumentContext()
  main_path <<- sub("/[^/]+.R$", "/", script_dir[[2]])


  if(main_trunk_as_wd == T){
    setwd(main_path)
  }else{
    other_dirs <- c(source_path = source_path, output_path = output_path, other_dirs)
  }

  for(i in 1:length(other_dirs)){
    path_tail <- other_dirs[i] %>% sub("([^/])$","\\1/",.) ## add a "/" tail in the path.
    path <- paste0(main_path, path_tail)
    ## create the path
    path_create(path = path)
    ### return the dirname to ClobalEnv.
    try(assign(x = other_dirs[i] %>% names(), value = path, envir = .GlobalEnv))
  }
  if(main_trunk_as_wd == F & Change_wd == T){
    real_time_source_path <- paste0(main_path,source_path)
    setwd(real_time_source_path) ## 虽然source_path 被赋值为全局变量，但是在这里面不能直接被调用更新，所以需要手动拼出来这个文件夹。
  }
  ### 询问是否要自动替换工作目录.
  cat("Working Dir was set to be ", getwd(), "\nsource_path  was", source_path, "\noutput_path was ", output_path)
} #### function 1 End #####

#'check and create a path.
#'@param path a path need to be checked.

## no need to export, cause the useres didn't need this function.
path_create <- function(path){
  if (dir.exists(path) == F) {dir.create(path, showWarnings = F, recursive = T)}
}

#' @title scripts_sourcer source scripts and load local functions into Global.env.
#' @details
#' 指定（绝对路径或相对当前目录的相对路径）某个文件夹
#' 该文件夹及其子文件夹内的脚本将按照字母顺序全部依次加载运行。
#' 各个函数会被加载到环境变量中。脚本中的初始化设置也会起效
#' 若想不加载某些内容。可以设定安全词，这样具有安全词的脚本边不会被运行。
#' @param my_function_path the path in which your functions and env initialization scripts stored.
#' @param functions_collection_path the path for the functions collection xlsx file.
#' @param safe_word character, scripts with the 'safe_word' won't be sourced.
#' @param repl_safe_word character, safe_word pass to grepl function like: grepl(repl_safe_word, filename)
#' @param encoding encoding of the scripts
#' @return final_function_table, a table of Functions.
#' @examples
#' \dontrun{
#' get_or_set_dir(other_dirs = c(fun_path = "DIY Function"))
#' load_functions(my_function_path = fun_path, functions_collection_path = main_path)
#' }
#' @export
load_functions <- function(my_function_path = fun_path,
                           functions_collection_path = main_path,
                           safe_word = "# be quiet",
                           repl_safe_word = "#\\s*silence",encoding = "utf-8"){

  all_files <- list.files(path = my_function_path, recursive = T, full.names = T)
  all_files <- all_files %>% gsub("\\\\","\\",.)
  all_files <- all_files %>% gsub("//","/",.)
  all_files <- grepl("\\.R$", all_files) %>% all_files[.]
  all_files <- not.grepl(repl_safe_word, all_files, ignore.case = T) %>% all_files[.]
  all_files <- not.grepl(safe_word, all_files, ignore.case = T) %>% all_files[.]
  all_files <- all_files %>% order %>% all_files[.]
  final_function_table <- NULL
  for (eachfile in all_files) {
    ### 依次加载文件
    cat(eachfile, " is to be sourced \n")
    source(eachfile, local = F, encoding = encoding)
    final_function_table <- function_catcher(eachfile = eachfile, final_function_table = final_function_table)
    ## local  ,TRUE, FALSE or an environment, determining where the parsed expressions are evaluated. FALSE (the default) corresponds to the user's workspace (the global environment) and TRUE to the environment from which source is called.
    ### source 时 local = F  才能将函数载入到 global 环境中
  }

  openxlsx::write.xlsx(final_function_table, paste0(functions_collection_path, "Functions Collection at ",Sys.Date(),".xlsx"))
  cat("All the  packages has been loaded!!")
  return(final_function_table)
}

#' @title Function Catcher
#' @details catch the functions in the scripts.
#' sub function for the load_functions
#' @param eachfile path of the script file
#' @param final_function_table the final table of functions.
#' @importFrom readr read_file
#' @export
function_catcher <- function(eachfile, final_function_table){

  ori_dir <- getwd() ## store the original dir.
  file_trunc <- eachfile %>% sub("/[^/]*$","/",.)
  relative_path <-eachfile %>% sub(".*/","",.)
  setwd(file_trunc)
  script_as_char <- read_file(relative_path)
  temp_function_finded <- gsub("(.*\n[ ]{0,10})([a-z][a-z_1-9]{0,50})([- <]+)(function[ (].*)", "\\2", script_as_char)
  ## \\2 for fuctionnames ### note!! using \\s or [ ] to match a blank character, cause
  #"neither [\\s] nor [\\t] couldn't perform well "
  script_as_char <- gsub("(.*\n[ ]{0,10})([a-z][a-z_1-9]{0,50})([- <]+)(function[ (].*)", "\\1\\4", script_as_char)
  ## character after kick the finded funciton out
  if (temp_function_finded != script_as_char) { temp_line <- data.frame(funtion_name = temp_function_finded,
                                                                        file_location = eachfile)
  final_function_table <- rbind(final_function_table, temp_line)
  }

  setwd(ori_dir) ## reset the original dir.
  return(final_function_table)
}
