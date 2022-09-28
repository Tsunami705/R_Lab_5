#' return_df
#'
#' `return_df`  return the dataframe data
#'
#' @param listl the data of Kolada database
#' @returns The dataframe data
#' @export
#'
#return the dataframe data
return_df<-function(listl){
  lens<-listl[[2]][[1]]
  for(i in 1:lens){
    listl[[2]][[2]]$count[i]<-listl[[2]][[2]][[i,4]][[1]]
    listl[[2]][[2]]$gender[i]<-listl[[2]][[2]][[i,4]][[2]]
    listl[[2]][[2]]$status[i]<-listl[[2]][[2]][[i,4]][[3]]
    listl[[2]][[2]]$value[i]<-listl[[2]][[2]][[i,4]][[4]]
  }
  df1<-listl[[2]][[2]][,-4]
  df1$value=as.numeric(df1$value)
  df1$count=as.numeric(df1$count)

  return(listl[[2]][[2]][,-4])
}
