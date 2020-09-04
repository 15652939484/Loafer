#'@title Create beautiful notes conviently
#'@param word_vector character vector. Words to be shown in the notes.
#'@examples
#'fast_notes(c("This is a demo for note","Type your own notes here"))
#'@return a multi-line character. which can be used to note.
#'@importFrom stringr str_pad
#'@export


fast_notes <- muti_notes <- function(word_vector){

  head <- str_pad("   Start  +++ $$$$$ +++  Start    ",70,side="both",pad = "#")
  body<- c()
  tail <- str_pad("     END  --- _____ ---  END      ",70,side="both",pad = "#")

  middle_line <- str_pad("  *** __ ***  *** __ ***   ",70,side="both",pad="#")
  for( i in 1:length(word_vector)){
    temp <- paste("##","   ",word_vector[i],"    ",sep = "")
    temp <- str_pad(temp,60,side = "right",pad = " ")
    temp <- str_pad(temp,70,side = "both",pad = "#")
    body <- c(body,temp)### join body by temp line by line
  }
  final <- c(head,body,middle_line,"\n\n",middle_line,body,tail)
  cat(paste(final),sep="\n")
  # return(final)
}
