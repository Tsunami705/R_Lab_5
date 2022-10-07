#' Use kolada API
#'
#' Returns the data of Kolada database, where kpi, municipality, period are parameters,2 of them should be given
#'
#' @param link A string
#' @param kpi A string
#' @param municipality A string
#' @param period A string
#' @import jsonlite
#' @import httr
#' @import tidyverse
#' @seealso https://github.com/Hypergene/kolada
#' @example
#' get_data(link="http://api.kolada.se/v2/data/",kpi="N00945",period="2009")
#' @export
#'
#'
get_data<-function(link="http://api.kolada.se/v2/data/",
                   kpi="",
                   municipality="",
                   period=""){
  encoding = "UTF-8"
  Sys.setenv(TZ="Europe/Stockholm")

  if(kpi!=""){kpi<-paste("kpi",kpi,sep="/")}
  if(municipality!=""){municipality<-paste("municipality",municipality,sep="/")}
  if(period!=""){period<-paste("year",period,sep="/")}

  if(kpi!="" && municipality!="" && period!=""){
    url<-paste(link,
               kpi,
               municipality,
               period,
               sep="/")
  }else if(kpi!="" && municipality!="" &&period==""){
    url<-paste(link,
               kpi,
               municipality,
               sep="/")
  }else if(kpi!="" && municipality=="" &&period!=""){
    url<-paste(link,
               kpi,
               period,
               sep="/")
  }else if(kpi=="" && municipality!="" &&period!=""){
    url<-paste(link,
               municipality,
               period,
               sep="/")
  }

  response<-httr::GET(url)
  #print the content of json text
  jsonlite::toJSON(jsonlite::fromJSON(content(response,as="text")),pretty=TRUE)
  result<-jsonlite::fromJSON(content(response,as="text"))

  listl<-list(response,result)
  return(listl)
}

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

#return the content of response
return_json<-function(listl){
  return(listl[[1]])
}
