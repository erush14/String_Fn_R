df <- data.frame(pirate = c(1,2,3,4,5), fav_item = c("gold", "cannon","plank","grog", "silver"), fav_phrase = c("gold", "plank", "plank", "ahoy", "noquarter"))

dc <- c('fav_item', 'fav_phrase')

a <- c("DANGEROUS_PIRATE")
b <-   c(y=c("UNICORNS","plank","quarter") )#, v=  c("gold", "silver", "platinum"))


#this function will query a subset of a df given the column names for specific strings and then create a flag indicating
#their presence in the row
#desiredcols is a character vector with the desired subset of columns to query (eg. CPT1, CPT2... or Diag1, Diag2)
#df is the data frame with diag codes and whatever other demographics from which the flag will be generated
# search vector is a character vector that you would like to query in the data frame
#new.col.name   is a character vector with the name for the flag column

string.in.row <- function(desiredcols, df, searchvector, new.col.name){
  #get the colindex
  getcolindex <- function (desiredcols, df) {sapply(desiredcols, function(x) (which(colnames(df) == x)))}
  g<- getcolindex(desiredcols, df)
  
  #collapse search string into regex friendly string
  sv <- paste(searchvector, collapse ="|")
  #iterate over each row in given col subset df[,g]
  query_out <- apply(df[,g],2, function(x) ifelse(grepl(sv, x),1,0))
  #now return 1 if the string occurs in any of the rows and attach to the df.... aside: why must I force the rename?
  h<- cbind(df,  MYNEWCOL = rowSums(query_out))
  names(h)[names(h) == 'MYNEWCOL'] <- new.col.name
  h
  }

string.in.row(dc, df, b, a)


