#' @tile get or set the dir by Rstudio
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
