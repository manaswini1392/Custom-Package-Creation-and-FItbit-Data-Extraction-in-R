#' Work out summary
#'
#' Takes in a date, fitbit login id and password and gives the work out summary
#' of that person on that date
#'
#' @name activity
#' @param analysis_date : date mail : string pass : string
#' @export



activity<- function(analysis_date,mail,pass)
{

  df<-data.frame()
  cookie <- login(email=mail, password=pass)
  df <- get_intraday_data(cookie, what="distance", date= analysis_date)
  df1 <- get_intraday_data(cookie, what="steps", date=analysis_date)

  df<-cbind(df,df1$steps)

  i=0
  var=seq(1,nrow(df),by=1)
  for(i in var)
  {

    df$speed[i] <- ifelse(i==1,0,(df$distance[i])*1609.34/900)
  }
  names(df)<-c("Datetime","distance in miles","steps","speed in meters/sec")

  a = c(Average_Speed=0,Work_Rest_Ratio=0,distance_covered=0,Average_Pace=0)
  count=0
  var<- seq(1,nrow(df),by=1)
  for(i in var)
  {
    if(df$`speed in meters/sec`[i]>0.02)
    {
      count=count+1
    }

  }

  a["Average_Speed"]<- mean(df$`speed in meters/sec`)
  a["Work_Rest_Ratio"]<- (count*15)/((nrow(df)*15)-(count*15))
  a["distance_covered"]<- sum(df$`distance in miles`)
  a["average_pace"] <- 1/(mean(df$`speed in meters/sec`))

  return(a)

}


