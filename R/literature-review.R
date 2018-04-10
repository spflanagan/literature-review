#Author: Sarah P. Flanagan (spflanagan.phd@gmail.com)
#Purpose: Easily parse text-based data, such as from a literature review

#' Find and keep or remove lines containing any of a list of keywords from a column 
#' @param keywords A list of character strings you want to use to search through your data.frame
#' @param column.name The name of the column you want to search through
#' @param dat The data frame you're searching through
#' @param inverse If inverse is TRUE, then it removes the rows with the keywords. Otherwise, it only keeps rows with the keywords
#' @return a dataframe with or without rows containing your search terms in column.name
#' @examples
#' dat<-data.frame(text1=sample(c("data frame","data table", "analysis", "text table"),15,replace = TRUE),text2=sample(c("alphabet","numbers", "my numbers", "your numbers"),15,replace = TRUE))
#' dat<-search_line(c("data","table"),"text1",dat)
#' dat<-search_line(c("your","my"),"text2",dat,inverse=TRUE)
#' @export
search_line<-function(keywords,column.name,dat,inverse=FALSE){
  rmvec<-unlist(lapply(keywords,grep,x=dat[,column.name]))
  if(isTRUE(inverse)){ dat<-dat[-rmvec[!duplicated(rmvec)],] }else{
    dat<-dat[rmvec[!duplicated(rmvec)],]
  }
  return(dat)
}