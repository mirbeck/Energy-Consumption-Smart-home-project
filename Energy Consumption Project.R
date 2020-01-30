
#Libraries part
pacman::p_load(RMySQL, dplyr, lubridate, plotly, tidyverse, ggplot2, zoo, reshape2, forecast, imputeTS)

## Create a database connection 
con = dbConnect(MySQL(), user='deepAnalytics', 
                password='Sqltask1234!', 
                dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

## List the tables contained in the database 
dbListTables(con)

## Use attribute names to specify specific attributes for download
query <- "SELECT * FROM yr_2006 UNION ALL
          SELECT * FROM yr_2007 UNION ALL
          SELECT * FROM yr_2008 UNION ALL
          SELECT * FROM yr_2009 UNION ALL
          SELECT * FROM yr_2010"

DF <- dbGetQuery(con, query)

#------------PRE-PROCESSING THE DATA-------------------------

## Changing all metrics to kW/hours

DF$Sub_metering_1 <- round(DF$Sub_metering_1/1000, digits = 3)
DF$Sub_metering_2 <- round(DF$Sub_metering_2/1000, digits = 3)
DF$Sub_metering_3 <- round(DF$Sub_metering_3/1000, digits = 3)

DF$Global_active_power <- round(DF$Global_active_power/60, digits = 4)
DF$Global_reactive_power <- round(DF$Global_reactive_power/60, digits = 4)

## Combining Date and Time attribute values in a new attribute column
DF <-cbind(DF,paste(DF$Date,DF$Time), stringsAsFactors=FALSE)

## Giving the new attribute in the 6th column a header name 

colnames(DF)[11] <-"DateTime"

## Moving the DateTime attribute within the dataset
DF <- DF[,c(ncol(DF), 1:(ncol(DF)-1))]
head(DF)

## Converting DateTime from POSIXlt to POSIXct 
DF$DateTime <- as.POSIXct(DF$DateTime, "%Y/%m/%d %H:%M:%S")

## Adding the time zone
attr(DF$DateTime, "tzone") <- "Europe/Paris"  
# Droping year 2006, "id", "Date","Time", "Global intensity", "Voltage" from our data set
DF <- DF[ -c(1:21992), -c(2,3,4,7,8)]
View(DF)
#------------------------ MISSING VALUES -------------------------------

#Creating another DateTime with same time intervals

dt <- seq.POSIXt(as.POSIXct("2007-01-01 01:00:00",'%Y/%m/%d %H:%M:%S'), as.POSIXct("2010-11-26 22:02:00",'%Y/%m/%d %H:%M:%S'), by="min") #When we set Eurpo/Paris time automatical adds 1 hour difference
dt <- seq.POSIXt(as.POSIXlt("2007-01-01 01:00:00"), as.POSIXlt("2010-11-26 22:02:00"), by="min")

df <- data.frame(DateTime=dt)
attr(df$DateTime, "tzone") <- "Europe/Paris"
df$DateTime <- as.POSIXct(df$DateTime, "%Y/%m/%d %H:%M:%S")
#Merging df and DF
DF_with_NA <- full_join(df,DF)
View(DF_with_NA)

# Add year, month, week, day, hour, minute coLumns to th DF_with_NA for better filtering our data

DF_with_NA <- DF_with_NA %>% mutate(year = year(DateTime), 
                                    month = month(DateTime), 
                                    week = week(DateTime), 
                                    day = day(DateTime), 
                                    hour = hour(DateTime), 
                                    minute = minute(DateTime))


colnames(DF_with_NA)[4] <-"Kitchen"
colnames(DF_with_NA)[5] <-"Laundry"
colnames(DF_with_NA)[6] <-"Water_heater_AC"

#impute NA using na_kalman

clean_df <- imputeTS::na_kalman(DF_with_NA)

#-------------------------FEATURE ENGENEERING---------------------------------

#Creating new columns as Submeterings, other areas for better filtering

clean_df$Submeterings <- clean_df$Kitchen+clean_df$Laundry+clean_df$Water_heater_AC
clean_df$other_areas <- clean_df$Global_active_power-clean_df$Submeterings

#New rows for year, month, week, day, hour, minute
clean_df$year <- year(clean_df$DateTime)
clean_df$month <- month(clean_df$DateTime)
clean_df$week <- week(clean_df$DateTime)
clean_df$day <- day(clean_df$DateTime)
clean_df$hour <- hour(clean_df$DateTime)
clean_df$minute <- minute(clean_df$DateTime)

View(clean_df)

#Granularity by years,seasons, months, weeks

getSeason <- function(date) {
  WS <- as.Date("2008-12-21", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2008-3-20",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2008-6-21",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2008-9-22",  format = "%Y-%m-%d") # Fall Equinox
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(date, format="2008-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}

#created a season column 
clean_df$season <- getSeason(clean_df$DateTime)

#seasonal groups
seasons <- clean_df %>% group_by(season) %>%
  summarize_at(vars(Kitchen,Laundry,Water_heater_AC,
                           other_areas, Global_active_power, 
                           Submeterings, year, month, week, day), funs(sum))  #grouping by season



# seasonality
seasonality <- clean_df %>% 
  group_by(date(DateTime)) %>% 
  summarise(mean = mean(Global_active_power)) %>%
  ggplot(aes(`date(DateTime)`, mean)) + 
  geom_line(color = "firebrick1") + 
  geom_smooth(se = F) + 
  labs(title = "Mean active energy consumed by day") + 
  ylab("kW/h") + xlab("Time") + theme_light() 

ggplotly(seasonality) 

#using loop to create other groups

agg_df <- list()
plots <- list()

groups <- c("year", "season", "month", "week", "day", "hour")

for(i in groups){
  agg_df[[i]] <- clean_df %>%
    group_by(DateTime=floor_date(DateTime, i)) %>%
    dplyr::summarize_at(vars(
      Kitchen,
      Laundry,
      Water_heater_AC,
      Global_active_power, 
      other_areas,
      Submeterings),
      funs(sum))
  
  plots[[i]] <- ggplot(data = agg_df[[i]], aes(x = DateTime)) +
    geom_line(aes(y = Submeterings, color = "Submetering")) +
    geom_line(aes(y = Global_active_power, color = "Global Power")) +
    geom_line(aes(y = other_areas, color = "Other Areas"))+
    theme_minimal()+
    labs(title = paste("Other areas, global active power, submeterings", i),
         x = "Time",
         y = "Power in kW/hour")
}

year <- plots[["year"]]
season <- plots[["season"]]
month <- plots[["month"]]
week <- plots[["week"]]
day <- plots[["day"]]

#Power consumption by sub-meters

Subs_Montly <- plot_ly(agg_df[["month"]], 
                                x = ~agg_df[["month"]]$DateTime, 
                                y = ~agg_df[["month"]]$Kitchen, 
                                name = 'Kitchen', type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~agg_df[["month"]]$Laundry,
            name = 'Laundry Room', mode = 'lines+markers') %>%
  add_trace(y = ~agg_df[["month"]]$Water_heater_AC,
            name = 'Water Heater & AC', mode = 'lines+markers') %>%
  layout(title = paste("Power Consumption per sub-meters monthly"),
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (kW-hours)"))

Subs_Montly

#---------------------------TIME-SERIES FROM 2007-2010---------------------------------

ts.year <- ts(agg_df[["year"]], start = c(2007,1), frequency = 1)
ts.month <- ts(agg_df[["month"]], start = c(2007,1), frequency = 12)
ts.week <- ts(agg_df[["week"]], start = c(2007,1), frequency = 52)
ts.day <- ts(agg_df[["day"]], start = c(2007,1), frequency = 365)
ts.hour <- ts(agg_df[["hour"]], start = c(2007,1), frequency = 8760)

## Create a time series for each variable in different periods

vars <- list("Kitchen", "Laundry", "Water_heater_AC", "Global_active_power", "other_areas")

ts.year.var <- list(vector(length = 5))
ts.month.var <- list(vector(length = 5))
ts.week.var <- list(vector(length = 5))
ts.day.var <- list(vector(length = 5))
ts.hour.var <- list(vector(length = 5))

for(i in vars){
  ts.year.var[[i]] <- ts.year[,i]
  ts.month.var[[i]] <- ts.month[,i]
  ts.week.var[[i]] <- ts.week[,i]
  ts.day.var[[i]] <- ts.day[,i]
  ts.hour.var[[i]] <- ts.hour[,i] 
}

#Decomposing TS

decomp_ts.month.var <- list()
decomp_ts.week.var <- list()
decomp_ts.day.var <- list()

decomp.month.plot <- list()
decomp.week.plot <- list()
decomp.day.plot <- list()

for (i in vars){
  # By month
  decomp_ts.month.var[[i]] <- ts.month.var[[i]] %>% stl(s.window = "periodic") 
  
  decomp.month.plot[[i]] <- ts.month.var[[i]] %>% stl(s.window = "periodic") %>%
    autoplot() + xlab("Year") +
    ggtitle(paste("Monthly decomposition of", i))
  # By week
  decomp_ts.week.var[[i]] <- ts.week.var[[i]] %>% stl(s.window = "periodic") 
  
  decomp.week.plot[[i]] <- ts.week.var[[i]] %>% stl(s.window = "periodic") %>% 
    autoplot() + xlab("Year") +
    ggtitle(paste("Weekly decomposition of", i)) 
  
  # By day
  decomp_ts.day.var[[i]] <- ts.day.var[[i]] %>% stl(s.window = "periodic") 
  
  decomp.day.plot[[i]] <- ts.day.var[[i]] %>% stl(s.window = "periodic") %>% 
    autoplot() + xlab("Year") +
    ggtitle(paste("Daily decomposition of", i)) 
}

decomp.month.plot
decomp.week.plot
decomp.day.plot

#----------------------DATA PARTITIONING------------------------------


# created data partition for ts.month (2007-2010.11)
train.m <- window(ts.month, start = c(2007,1), end = c(2009,12))
test.m <- window(ts.month, start= c(2010,1))

## created data partition ts.week (2007-2010.11)
train.w <- window(ts.week, start = c(2007,1), end = c(2009,12))
test.w <- window(ts.week, start= c(2010,1))

#---------------------FORECASTING-------------------------------------

#6 MONTH forecasting of: 
                        #"Global Active power", 
                        #"Submeterings", 
                        #"Other Areas" 
                        #using LR

#GLobal Active Power 
lm_month_global <- tslm(train.m[,5] ~ trend + season) # [,5] corresponds to "Global Active Energy" in ts.month
summary(lm_month_global)

predict_lm_month_global <- forecast(lm_month_global, h=6, level=c(80,90))
plot(predict_lm_month_global, ylab= "Watt-Hours", xlab="Time")

#Other Areas
lm_month_other <- tslm(train.m[,6] ~ trend + season) # [,6] corresponds to "other areas" in ts.month
summary(lm_month_other)

predict_lm_month_other <- forecast(lm_month_other, h=6, level=c(80,90))
plot(predict_lm_month_other, ylab= "Watt-Hours", xlab="Time")

#Submeterings
lm_month_subs <- tslm(train.m[,7] ~ trend + season) # [,6] corresponds to "other areas" in ts.month
summary(lm_month_subs)

predict_lm_month_subs <- forecast(lm_month_subs, h=6, level=c(80,90))
plot(predict_lm_month_subs, ylab= "Watt-Hours", xlab="Time")

#6 MONTH forecasting of: 
                        #"Global Active power", 
                        #"Submeterings", 
                        #"Other Areas" 
                        # using ARIMA

##GLobal Active Power 

autoarima_month_global <- auto.arima(train.m[,5])
predict_autoarima_month_global <- forecast(autoarima_month_global, h = 6, level = c(80,90)) #6 month forecasting
predict_autoarima_month_global
plot(predict_autoarima_month_global, col = "blue", fcol = "red")

#Other Areas

autoarima_month_other <- auto.arima(train.m[,6])
predict_autoarima_month_other <- forecast(autoarima_month_other, h = 6, level = c(80,90)) #6 month forecasting
predict_autoarima_month_other
plot(predict_autoarima_month_other, col = "blue", fcol = "green")


#Submeterings
autoarima_month_subs <- auto.arima(train.m[,7])
predict_autoarima_month_subs <- forecast(autoarima_month_subs, h = 6, level = c(80,90)) #6 month forecasting
predict_autoarima_month_subs
plot(predict_autoarima_month_subs, col = "blue", fcol = "black")

#6 MONTH forecasting of: 
                        #"Global Active power", 
                        #"Submeterings", 
                        #"Other Areas" using 
                        # using HOLT WINTERS

#Global Active power

HW_month_global <- HoltWinters(train.m[,5])
predict_HW_month_global <- forecast(HW_month_global, h=6)
plot(predict_HW_month_global)

#Submeterings

HW_month_subs <- HoltWinters(train.m[,6])
predict_HW_month_subs <- forecast(HW_month_subs, h=6)
plot(predict_HW_month_subs)

#Other Areas

HW_month_other <- HoltWinters(train.m[,7])
predict_HW_month_other <- forecast(HW_month_other, h=6)
plot(predict_HW_month_other)


#COMPARING MODELS

#Global Active Power

modCompare.m_global <- autoplot(ts.month[,"Global_active_power"], series = "real", size = 2) + 
  autolayer(predict_autoarima_month_global, series= "ARIMA prediction", PI=FALSE, size = 1) +
  autolayer(predict_HW_month_global, series= "Holt Winters prediction", PI= FALSE, size = 1) +
  autolayer(predict_lm_month_global, series= "Linear Regression", PI= FALSE, size = 1) +
  scale_linetype_manual(labels = c("real", "ARIMA prediction", "Holt Winters prediction", "Linear Regression"),
                        values = c(1, 4, 4, 4)) +
  scale_size_manual(labels = c("real", "ARIMA prediction", "Holt Winters prediction", "Linear Regression"),
                    values = c(1, 4, 4, 4)) +
  ggtitle("Prediction of Global Active power by ARIMA, Holt Winters, Linear Regression")

modCompare.m_global

# Submeterings 

modCompare.m_subs <- autoplot(ts.month[,"Submeterings"], series = "real", size = 2) + 
  autolayer(predict_autoarima_month_subs, series= "ARIMA prediction", PI=FALSE, size = 1) +
  autolayer(predict_HW_month_subs, series= "Holt Winters prediction", PI= FALSE, size = 1) +
  autolayer(predict_lm_month_subs, series= "Linear Regression", PI= FALSE, size = 1) +
  scale_linetype_manual(labels = c("real", "ARIMA prediction", "Holt Winters prediction", "Linear Regression"),
                        values = c(1, 4, 4, 4)) +
  scale_size_manual(labels = c("real", "ARIMA prediction", "Holt Winters prediction", "Linear Regression"),
                    values = c(1, 4, 4, 4)) +
  ggtitle("Prediction submeterings consumption by ARIMA, Holt Winters, Linear Regression")

modCompare.m_subs

# Other Areas

modCompare.m_other <- autoplot(ts.month[,"other_areas"], series = "real", size = 2) + 
  autolayer(predict_autoarima_month_other, series= "ARIMA prediction", PI=FALSE, size = 1) +
  autolayer(predict_HW_month_other, series= "Holt Winters prediction", PI= FALSE, size = 1) +
  autolayer(predict_lm_month_other, series= "Linear Regression", PI= FALSE, size = 1) +
  scale_linetype_manual(labels = c("real", "ARIMA prediction", "Holt Winters prediction", "Linear Regression"),
                        values = c(1, 4, 4, 4)) +
  scale_size_manual(labels = c("real", "ARIMA prediction", "Holt Winters prediction", "Linear Regression"),
                    values = c(1, 4, 4, 4)) +
  ggtitle("Other areas consumption predictions by ARIMA, Holt Winters, Linear Regression")

modCompare.m_other




#-------------------Shiny part------------------------------------

# Created time for rangeslider

d <- tibble::tibble(
  time = seq(as.Date("2007-01-01"), as.Date("2010-11-01"), by = "days"),
  y = rnorm(seq_along(time))
)


#Plotly Submeterings with rangeslider
Subs <- plot_ly(d, 
                x = ~agg_df[["month"]]$DateTime, 
                y = ~agg_df[["month"]]$Kitchen,
                name = 'Kitchen', type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~agg_df[["month"]]$Laundry,
            name = 'Laundry Room', mode = 'lines+markers') %>%
  add_trace(y = ~agg_df[["month"]]$Water_heater_AC,
            name = 'Water Heater & AC', mode = 'lines+markers') %>%
  layout(title = paste("Power Consumption by sub-meters monthly"),
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (kW-hours)")) %>% 
  add_lines() %>%
  rangeslider(d$time[300], d$time[1100])

Subs

#Plotly GLobal Active + Subs + Other Areas with rangeslider
Global_subs_other <- plot_ly(d, 
        x = ~agg_df[["month"]]$DateTime, 
        y = ~agg_df[["month"]]$Submeterings,
        name = 'Submetering', type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~agg_df[["month"]]$Global_active_power,
            name = 'Global Active Power', mode = 'lines+markers') %>%
  add_trace(y = ~agg_df[["month"]]$other_areas,
            name = 'Other Areas', mode = 'lines+markers') %>%
  layout(title = paste("Other areas, global active power, submeterings"),
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (kW-hours)")) %>% 
  add_lines() %>%
  rangeslider(d$time[300], d$time[1100])

Global_subs_other

# Mean Energy consumption by seasons

seasonality <- clean_df %>% 
  group_by(date(DateTime)) %>% 
  summarise(mean = mean(Global_active_power)) %>%
  ggplot(aes(`date(DateTime)`, mean)) + 
  geom_line(color = "firebrick1") + 
  geom_smooth(se = F) + 
  labs(title = "Consumed mean active energy ") + 
  ylab("kW/h") + xlab("Time") + theme_light() 

plot(seasonality)

# Forecast for 6 month GLobal Active Power, Subs, Other

pred_glob <- plot(predict_lm_month_subs, ylab= "Watt-Hours", xlab="Time")
pred_subs <- plot(predict_HW_month_subs, ylab= "Watt-Hours", xlab="Time")
pred_other<- plot(predict_autoarima_month_other, ylab= "Watt-Hours", xlab="Time")

information <- "To prepare this analysis was used a Data set contains more than 2 million measurements gathered in a house located in Sceaux. (7km of Paris, France) 
between December 2006 and November 2010 (47 months)"
