#the packages needed
install.packages('stringr')
library(stringr)
library(tm)
library(dplyr)

#some text with line ids. Thanks wikipedia!
df <- data.frame(line = c(1,2,3), mytext = c('The giant panda lives in a few mountain ranges in central China, mainly in Sichuan, but also
in neighbouring Shaanxi and Gansu.[9] As a result of farming, deforestation, and other development,
the giant panda has been driven out of the lowland areas where it once lived. The giant panda is a conservation reliant vulnerable species',
                                       
'While the dragon has often served as Chinas national symbol, internationally the giant panda appears at 
least as commonly.[citation needed] As such, it is becoming widely used within China in international contexts, for 
example since 1982 issuing gold panda bullion coins or as one of the five Fuwa mascots of the Beijing Olympics.',

'The name giant panda is sometimes used to distinguish it from the unrelated red panda.Though it belongs to the order Carnivora, the giant pandas 
diet is over 99% bamboo.[6] Giant pandas in the wild will occasionally eat other grasses, wild tubers, 
or even meat in the form of birds, rodents, or carrion. In captivity, they may receive honey, eggs, fish, yams, shrub leaves, oranges, or bananas along with 
specially prepared food.[7][8]'))

#use stringr to locate all instances of word of interest (woi)
str_locate_all(df$mytext, 'panda|bear')-> indices
names(indices) <- df$line

#attach line number to extracted indices of word of interest and use in lapply iterating through all lines of text. 
add_line_to_indices <- function(indices, x) if(length(indices[[x]])>0) (cbind(indices[[x]], line = names(indices[x]))) 

#use in apply over c(1:length(df$mytext))
lapply(df$line, function(x) (add_line_to_indices(x, indices = indices))) -> indices_line

#some reaarranging to make a data frame consisting of line number, start and end character index of w.o.i. 
do.call(rbind, indices_line) -> indices_line
as.data.frame(apply(indices_line, 2, function(x)(as.numeric(x))))-> indices_line


#pulling character indices by line and associating with the indices and line in df
#pass in col names as character. ref_cols for df with text and index df are equal by default. if not enter id col of 
#indices in ref_i_col. begin and end indices references default to 'start' and 'end'. 
#by default, this is set to +/-50 char. 
#this seems a messy way to do this... 

extract_around_woi <- function(y, df, ref_col, text_col, indices, begin_col = 'start', end_col = 'end', ref_i_col = ref_col, S=50, E=50)
  if (length(which(indices[,ref_i_col] == y)) >0)
  {df[,text_col][which(df[,ref_col] == y)] -> Strang
    lapply(which(indices[,ref_i_col] == y), function(x) (
      cbind(indices[x,],
            extract = substr(Strang, start = max(0, indices[,begin_col][x]-S) , stop = indices[,end_col][x]+E
            ))))}

#use number of text fields as number of lines or c(1:length(df$mytext)) 
lapply(c(1:3), function(x) (extract_around_woi(y=x, df = df, ref_col = 'line', text_col = 'mytext', indices = indices_line))) -> extracted_phrases

#this produces N list of  J lists.... 
#bind all the lists together

#first bind J lists together
#listN is list of lists
bind_up <-function(listN, x)
  if (!is.null(listN[[x]])) {
    do.call(rbind, listN[[x]]) 
  }
# then bind all the lists together into a lovely data frame
bind_up_all <- function(listN){
  do.call(rbind, lapply(c(1:length(listN)), function(x) (bind_up(x))) )}

bind_up_all(extracted_phrases)
