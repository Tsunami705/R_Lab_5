library(jsonlite)
library(httr)
library(tidyverse)


#Use kolada API
#Returns the data of Kolada database, where kpi, municipality, period are parameters
#example:
#    ree<-get_data(link,kpi="N00945",period="2009")
#    return_df(ree)

get_data<-function(link="http://api.kolada.se/v2/data/",
                   kpi="",
                   municipality="",
                   period=""){
  Sys.setenv(TZ="Europe/Stockholm")
  
  if(kpi!=""){kpi<-paste("kpi",kpi,sep="/")}
  if(municipality!=""){municipality<-paste("municipality",municipality,sep="/")}
  if(period!=""){period<-paste("year",period,sep="/")}

  url<-paste(link,
                kpi,
                municipality,
                period,
                sep="/")
  
  response<-GET(url)
  #print the content of json text
  toJSON(fromJSON(content(response,as="text")),pretty=TRUE)
  result<-fromJSON(content(response,as="text"))
  
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

  return(listl[[2]][[2]][,-4])
}

#return the content of response
return_json<-function(listl){
  return(listl[[1]])
}
