thenames <- c('elmo', 'bert')
list.files(pattern = '.csv') -> thecsvs



naming_off_vector <- function(thenames, thefiles, sep = ",", header = TRUE,  quote = "\"", fill = TRUE, comment_char = '') {
  for (i in c(1:length(thefiles)) ) {
          assign(thenames[i], value = read.table(thefiles[i], sep =sep, header= header, quote = quote, fill = fill, comment.char = comment_char
                                                 ),  envir = parent.frame()) }
}




