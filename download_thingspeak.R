
# This package is required for Accessing APIS (HTTP or HTTPS URLS from Web)
library(httr)
#This package exposes some additional functions to convert json/text to data frame
library(rlist)
#This package exposes some additional functions to convert json/text to data frame
library(jsonlite)
#This library is used to manipulate data
library(dplyr)
#this is for plotting
library(ggplot2)
#this is for "melting" data
library(reshape2)
library(dplyr)

theme_ts_space<-theme_grey() +
  theme(
    #		panel.grid.major = element_blank(),
    #		panel.grid.minor = element_blank(),
    panel.background = element_rect(fill="white", colour="black", size=2),
    #		legend.key       = element_blank(),
    #		legend.text      = element_text(size=20.5),
    #		legend.text      = element_blank(),
    #		legend.title     = element_text(size=20.5),
    axis.text.x = element_text(size=10,colour="black",hjust=1,angle=45),
    axis.text.y = element_text(size=10,colour="black",vjust=.3),
    axis.title.x = element_text(size=10),
    axis.title.y = element_text(size=10,vjust=-1),
    #		plot.title = element_text(hjust = 0.5,size=10,face="bold"),		
    #		legend.position  = "left",
    #		legend.position  = "none",
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.border = element_rect(colour = "black", fill=NA, size=2)
  )

ts_dl<-function(start_dt,end_dt,channel_id){
start<-as.Date(start_dt,format="%Y-%m-%d",tz="EST")
end<-as.Date(end_dt,format="%Y-%m-%d",tz="EST")
days<-end-start
rm(total)
for(i in 1:days){
  rm(morning)
  rm(afternoon)
  daynow<-start+i-1
  resp<-GET(paste("https://thingspeak.com/channels/",channel_id,"/feeds.json?results=8000&start=",daynow,"%2000:00:00&end=",daynow,"%2011:59:00&timezone=America%2FNew_York",sep=""))
  #.When we get the response from API we will use to very basic methods of httr.
  jsonRespParsed<-content(resp,as="parsed") 
  modJson<-jsonRespParsed$feeds #. Access data element of whole list and ignore other vectors
  if(length(modJson)>0){
  try(morning<-modJson%>%bind_rows%>%select(created_at,field1,field2,field3,field4,field5,field6,field7,field8),silent=TRUE)

  
  resp<-GET(paste("https://thingspeak.com/channels/",channel_id,"/feeds.json?results=8000&start=",daynow,"%2012:00:00&end=",daynow,"%2023:59:00&timezone=America%2FNew_York",sep=""))
  #.When we get the response from API we will use to very basic methods of httr.
  jsonRespParsed<-content(resp,as="parsed") 
  modJson<-jsonRespParsed$feeds #. Access data element of whole list and ignore other vectors
  try(afternoon<-modJson%>%bind_rows%>%select(created_at,field1,field2,field3,field4,field5,field6,field7,field8),silent=TRUE)
  if(exists("morning")&exists("afternoon")){
  day<-bind_rows(as.data.frame(morning),as.data.frame(afternoon))}else{
    if(exists("morning")){
      total<-as.data.frame(morning)
    }else{if(exists("afternoon")){total<-as.data.frame(afternoon)}}
  }
  if(!exists("total")){
    total<-day[0,]
  }
  fieldnames<-data.frame(jsonRespParsed[[1]])
  #fieldnames[1,grep("field",names(fieldnames))]
  names(day)[grep("field",names(day))]<-as.character(unlist(fieldnames[1,grep("field",names(fieldnames))]))
  total<-bind_rows(total,day)
  }
}
if(exists("total")){
total$dt<-gsub("-04:00","",total$created_at,fixed=TRUE)
total$dt<-gsub("-05:00","",total$dt,fixed=TRUE)
total$dt<-as.POSIXct(total$dt,format="%Y-%m-%dT%H:%M:%S",tz="EST")
total$site_id<-channel_id
return(total)}else(print("nada"))
}

data_1030130<-ts_dl("2020-03-01",Sys.Date()+1,"1030130")
data_1036916<-ts_dl("2020-03-01",Sys.Date()+1,"1036916")
data_1036917<-ts_dl("2020-03-01",Sys.Date()+1,"1036917")
data_1036924<-ts_dl("2020-03-01",Sys.Date()+1,"1036924")
data_1036925<-ts_dl("2020-03-01",Sys.Date()+1,"1036925")
data_1036874<-ts_dl("2020-03-01",Sys.Date()+1,"1036874")
data_1036907<-ts_dl("2020-03-01",Sys.Date()+1,"1036907")
data_1036891<-ts_dl("2020-03-01",Sys.Date()+1,"1036891")
data_1036908<-ts_dl("2020-03-01",Sys.Date()+1,"1036908")
data_1036910<-ts_dl("2020-03-01",Sys.Date()+1,"1036910")
data_1036911<-ts_dl("2020-03-01",Sys.Date()+1,"1036911")

airq_data<-bind_rows(data_1030130,data_1036916,data_1036925,data_1036891,data_1036907)
names_no<-names(airq_data)[grep("field",names(airq_data))]
airq_data<-airq_data[,!names(airq_data)%in%names_no]
airq_data<-subset(airq_data,Temperature!="" & !is.na(Temperature))
airq_data$"AQ Sensor Slope"[!is.na(airq_data$"Aq Sensor Slope")]<-airq_data$"Aq Sensor Slope"[!is.na(airq_data$"Aq Sensor Slope")]
airq_data$"AQ Sensor Raw"[!is.na(airq_data$"Aq Sensor Raw")]<-airq_data$"Aq Sensor Raw"[!is.na(airq_data$"Aq Sensor Raw")]

airq_data<-airq_data[,!grepl("Aq Sensor Raw",names(airq_data),fixed=TRUE)]
airq_data<-airq_data[,!grepl("Aq Sensor Slope",names(airq_data),fixed=TRUE)]

saveRDS(airq_data,"/srv/shiny-server/Data_Dashboard/airq_data.rds")

range(airq_data$dt)



d1 <- data.frame(y1 = c(1, 2, 3), y2 = c(4, 5, 6))
d2 <- data.frame(y1 = c(3, 2, 1), y2 = c(6, 5, 4))
my.list <- list(d1=d1, d2=d2)



metadata<-content(GET("https://api.thingspeak.com/channels/1030130/feeds.json?metadata"),as="text")
metadata2<-content(GET("https://api.thingspeak.com/channels/1030130/feeds.json?metadata=true"),as="parsed")
metadata2<-content(metadata,as="parsed") 

resp<-GET("https://thingspeak.com/channels/1030130/feeds.json?results=8000&start=2020-04-03%2003:00:00&timezone=America%2FNew_York")
#.When we get the response from API we will use to very basic methods of httr.
http_type(resp)  #. This method will tell us what is the type of response fetched from GET() call to the API.

jsonRespText<-content(resp,as="text") 
#jsonRespText

jsonRespParsed<-content(resp,as="parsed") 
#jsonRespParsed


modJson<-jsonRespParsed$feeds #. Access data element of whole list and ignore other vectors
#modJson

#Using dplyr and base R
modJson%>%bind_rows%>%select(created_at,field1,field2,field3,field4,field5,field6,field7,field8)


HosenArgon<-as.data.frame(modJson%>%bind_rows%>%select(created_at,field1,field2,field3,field4,field5,field6,field7,field8))
names(HosenArgon)<-c("DateTime","Temperature","Humidity","Pressure","Voltage","AQSlope","AQVoltage","LPO","DustPercent")
HosenArgon$DateTime<-as.POSIXct(gsub("-04:00","",HosenArgon$DateTime,fixed=TRUE),format="%Y-%m-%dT%H:%M:%S")

df_clean <- HosenArgon %>% mutate_if(is.factor, as.character)  %>% mutate_if(is.character, as.numeric)

melt4plots<-melt(df_clean,id.vars=c("DateTime"))

ggplot(melt4plots,aes(DateTime,value))+
  theme_ts_space+
  theme(
    strip.background = element_blank(),
    strip.placement="outside")+
  facet_wrap(~variable,scales="free_y",strip.position="left")+
  geom_point(size=2)+
  scale_x_datetime(limits=c(as.POSIXct("2020-04-03 03:29:00",tz="GMT+4"),as.POSIXct("2020-04-03 14:46:28",tz="GMT+4")))+
  ylab("")
