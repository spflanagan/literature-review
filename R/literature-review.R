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
search_line<-function(keywords,column.name,dat,inverse=FALSE,remwords=NULL){
  rmvec<-unlist(lapply(keywords,grep,x=dat[,column.name]))
  if(isTRUE(inverse)){ dat<-dat[-rmvec[!duplicated(rmvec)],] }else{
    dat<-dat[rmvec[!duplicated(rmvec)],]
  }
  if(!is.null(remwords)){ 
    rmvec<-unlist(lapply(remwords,grep,x=dat[,column.name]))
    if(length(rmvec)>0){ dat<-dat[-rmvec[!duplicated(rmvec)],] }
  }
  return(dat)
}

#' Find and count the number of lines containing any of a list of keywords from a column 
#' @param keywords A list of character strings you want to use to search through your data.frame
#' @param column.name The name of the column you want to search through
#' @param dat The data frame you're searching through
#' @param remwords Keywords to search for to remove those lines from the count
#' @return a dataframe with or without rows containing your search terms in column.name
#' @examples
#' dat<-data.frame(text1=sample(c("data frame","data table", "analysis", "text table"),15,replace = TRUE),text2=sample(c("alphabet","numbers", "my numbers", "your numbers"),15,replace = TRUE))
#' ndat<-count_lines(c("data","table"),"text1",dat)
#' ndat<-count_lines(c("data","table"),"text1",dat,c("text"))
#' @export
count_lines<-function(keywords,column.name,dat,remwords=NULL){
  kpvec<-unlist(lapply(keywords,grep,x=dat[,column.name]))
  dat<-dat[kpvec[!duplicated(kpvec)],]
  if(!is.null(remwords)){ 
    rmvec<-unlist(lapply(remwords,grep,x=dat[,column.name]))
    if(length(rmvec)>0){ dat<-dat[-rmvec[!duplicated(rmvec)],] }
  }
  return(nrow(dat))
}

#' Count the number of lines that are not empty in a range of 
#' @param dat The data frame you're searching through
#' @param range The range of columns you're going to look at
#' @param nomatch Optional string of what you don't want the search to match (default is "")
#' @return a dataframe with or without rows containing your search terms in column.name
#' @examples
#' dat<-data.frame(text1=sample(c("x",""),15,replace = TRUE),text2=sample(c("x",""),15,replace = TRUE),text3=sample(c("x",""),15,replace = TRUE))
#' ndat<-count_filled_lines(dat,1:3)
#' ndat<-count_filled_lines(dat[text1!="",],2:3)
#' @export
count_filled_lines<-function(dat,range,nomatch=""){
  cnt<-apply(dat[,range],2,function(c){ length(c[c!=""])})
  return(cnt)
}