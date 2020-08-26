#' get colors and breaks for plot(usually for continue var in heatmap)
#' @param max the top end of the data to be mapped.
#' @param min the below end of the data to be mapped.
#' @param breaks the breaks for diff colors.
#' @param print_notes wether to print the demo codes for users
#' @param balance_color the color in the middle.
#' @param zero_for_balance the colors between the 'balance color' and other color.
#' @param white_color_expend the length expand round the zero point.
#' @param blue_raw the blue colors
#' @param red_raw the red colors
#' @examples
#' \dontrun{
#' result <- get_colors(max = 3,min = -3,
#'     breaks = 0.05,
#'     white_color_expend = 2)
#' }
#' @return a list with two sub vector
#' @return result$my_color the colors vector.
#' @return result$my_breaks the breaks vector.
#'
#' @export

#' @importFrom grDevices colorRampPalette
get_colors <- function(max = 3,min = -3,
                       breaks = 0.05,
                       print_notes = F,
                       white_color_expend = 2,
                       blue_raw = c("#5000B8","#2A52BE","#1B53DE","#72AEEE"),
                       red_raw  = c("#FFFAB3","#FFBF00","#EB4E00","#DF0029","#C50023","#8B0016"),
                       zero_for_balance = 0,
                       balance_color  = "white"){

  result <- list()
  ### get the range for the value.
  red_part  <- max - zero_for_balance
  blue_part <- zero_for_balance - min
  if(white_color_expend == 0){
    white_color <-  balance_color
  }else{
    white_color <- colorRampPalette(c(blue_raw[length(blue_raw)],balance_color,red_raw[1]))(1+white_color_expend *2)
  }
  ## use the
  red_color   <- colorRampPalette(red_raw)(red_part/breaks-white_color_expend)
  blue_color  <- colorRampPalette(blue_raw)(blue_part/breaks-white_color_expend)

  my_color  <- c(blue_color,white_color,red_color) ### order is from lower to upper from left to the right.
  my_breaks <- seq(-1*blue_part,red_part, by = breaks)

  result$my_color  <-  my_color
  result$my_breaks <-  my_breaks
  return(result)
  if(print_notes){print_the_notes()}
}

print_the_notes <- function(){
  cat("#Belowing codes might be helpful\n
        result <- get_colors_and_breaks(max = max(final_df,0.1*max(abs(final_df))),min = min(final_df,-0.1*max(abs(final_df))),
                                        breaks = 0.05,
                                        white_color_expend = 3)

        # brewer.pal(9, \"Set1\") ### 查看颜色
        # annotation_colors = list( ## 配置图例的颜色
        #     Class = c(zt0=\"red\", zt12 = \"#2A52BE\") #连续数值型分组可设置成渐变
        #     ,Meta_Class = Meta_color_vec
        # )

        pheatmap_filename <-  paste0(output_path,Sys.Date(),\"#heatmap 南方医科大\",\".pdf\")
        pic <-  pheatmap(final_df,
                         annotation_row = annotation_row,
                         annotation_col = annotation_col,
                         annotation_colors = annotation_colors,
                         # display_numbers = p_df_label,
                         color  = result$my_color,
                         breaks = result$my_breaks,
                         # fontsize_number = 12,
                         number_color = \"white\", ## 否则显著差异的符号不明显。
                         # number_color = \"black\",
                         angle_col = 45,
                         cellwidth = 16,
                         cellheight = 16,
                         border_color = \"grey70\",
                         cluster_rows = F,
                         cluster_cols = F
        )
        ggsave (pheatmap_filename, plot = print(pic) , width =21 , height = 21, units = c(\"cm\"),scale = 1, limitsize = F)")
}









