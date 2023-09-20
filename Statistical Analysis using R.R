install.packages("pastecs")
install.packages("caret")
install.packages("car")
install.packages("qqplotr") 
install.packages("ggplot2")
install.packages("TTR") 
install.packages("forecast")
library(pastecs)
library(tidyverse)
library(corrplot)
library(caret)
library(car)
library(dplyr)
library(ggplot2)
library(qqplotr)
library(TTR) 
library(forecast)
library(tseries)

#WE READ THE CVS INTO DATAFRAME
natural_resource <- read.csv('natural resource.csv', header=TRUE)


#THE HEAD FUNCTION RETURNS THE FIRST FEW ROWS
head(natural_resource)


#WE CHECK THE STRUCTURE OF THE DATASET
str(natural_resource)


#WE RENAME THE COLUMN HEADERS
natural_resource <- rename(natural_resource, 
                           Country = "Country.Name",
                           Year = "Time",
                           Resource_GDP = "Total.natural.resources.rents....of.GDP...NY.GDP.TOTL.RT.ZS.",
                           Natural_gas_rents = "Natural.gas.rents....of.GDP...NY.GDP.NGAS.RT.ZS.",
                           Forest_rents = "Forest.rents....of.GDP...NY.GDP.FRST.RT.ZS.",
                           Oil_rents = "Oil.rents....of.GDP...NY.GDP.PETR.RT.ZS.",
                           Mineral_rents = "Mineral.rents....of.GDP...NY.GDP.MINR.RT.ZS.",
                           Coal_rents = "Coal.rents....of.GDP...NY.GDP.COAL.RT.ZS."
                           )

head(natural_resource)


#WE CGROUP THE COUNTRIES INTO NORTH AND SUB SAHARA REGIONS
natural_resource <- natural_resource%>%mutate(Regions=
                                                case_when(`Country` %in% 'Mauritania'~'Sub-Saharan Africa',
                                                          `Country` %in% 'Ghana'~'Sub-Saharan Africa',
                                                          `Country` %in% 'Zimbabwe'~'Sub-Saharan Africa',
                                                          `Country` %in% 'South Africa'~'Sub-Saharan Africa',
                                                          `Country` %in% 'Nigeria'~'Sub-Saharan Africa',
                                                          `Country` %in% 'Libya'~'North Africa',
                                                          `Country` %in% 'Egypt, Arab Rep.'~'North Africa',
                                                          `Country` %in% 'Algeria'~'North Africa',
                                                          `Country` %in% 'Morocco'~'North Africa',
                                                          `Country` %in% 'Tunisia'~'North Africa'))

head(natural_resource)


#WE DROP THE COUNTRY-CODE AND TIME-CODE COLUMN FROM THE DATAFRAME
natural_resource <- subset(natural_resource, select = -c(Country.Code, Time.Code))
head(natural_resource)


#WE CHECK FOR MISSING VALUES
sapply(natural_resource, function(x) sum(is.na(x)))


#BOXPLOT TO DETECT OUTLERS
boxplot(natural_resource[,c(3,4,5,6,7,8)],
        xlab = 'All',
        main = 'Boxplot of All features for Outlier detection')


#WE CREATE A DATAFRAME FOR NORTH AFRICA COUNTRIES
north_africa <- natural_resource %>% 
  filter(Regions %in% 'North Africa')
head(north_africa)


#WE CREATE A DATAFRAME FOR SUB-SAHARA AFRICA COUNTRIES
sub_sahara_africa <- natural_resource %>% 
  filter(Regions %in% 'Sub-Saharan Africa')
head(sub_sahara_africa)


#WE CARRY OUT DESCRIPTIVE STATISTICS ON THE TWO REGION FOR COMPARISION 
stat.desc(sub_sahara_africa[,c(3,4,5,6,7,8)], norm = TRUE)

stat.desc(north_africa[,c(3,4,5,6,7)], norm = TRUE)


##HIST PLOT FOR VARIABLES IN NORTH AFRICA
par("mfcol"=c(3, 1))
hist(north_africa$Mineral_rents, col="yellow")
hist(north_africa$Coal_rents, col="brown")
hist(north_africa$Resource_GDP, col="pink")
par("mfcol"=c(1, 1))

par("mfcol"=c(3, 1))
hist(north_africa$Natural_gas_rents, col="blue")
hist(north_africa$Forest_rents, col="green")
hist(north_africa$Oil_rent, col="red")
par("mfcol"=c(1, 1))

#HIST PLOT FOR VARIABLES IN SUB SAHARAH AFRICA
par("mfcol"=c(3, 1))
hist(sub_sahara_africa$Mineral_rents, col="yellow")
hist(sub_sahara_africa$Coal_rents, col="brown")
hist(sub_sahara_africa$Resource_GDP, col="pink")
par("mfcol"=c(1, 1))

par("mfcol"=c(3, 1))
hist(sub_sahara_africa$Natural_gas_rents, col="blue")
hist(sub_sahara_africa$Forest_rents, col="green")
hist(sub_sahara_africa$Oil_rent, col="red")
par("mfcol"=c(1, 1))


#CORRELATION ANALYSIS FOR NORTH AFRICA
round(cor(north_africa[,c(3,4,5,6,7,8)]),
      digits = 2)

#CORRELATION MATRIX PLOT FOR NORTH AFRICA
corrplot(cor(north_africa[,c(3,4,5,6,7,8)]),
         method = "number",
         digits = 2) 


#CORRELATION ANALYSIS FOR SUB SAHARA AFRICA
round(cor(sub_sahara_africa[,c(3,4,5,6,7,8)]),
      digits = 2)

#CORRELATION MATRIX PLOT FOR SUB SAHARA AFRICA
corrplot(cor(sub_sahara_africa[,c(3,4,5,6,7,8)]),
         method = "number",
         digits = 2) 



#MULTIPLE LINEAR REGRESION MODEL FRO NORTH AFRICA REGION
model_1 <- lm(Resource_GDP ~ Oil_rents + Natural_gas_rents + Forest_rents,
              north_africa)

#SUMMARY OF THE MODEL
summary.lm(model_1)

#SCATTER PLOT MATRIX FOR THE 5 VARIABLES
pairs(north_africa[,c(3,4,5,6)], lower.panel = NULL, pch =19, cex =0.2)

#RESIDUAL INDEPENDENce PLOT
plot(model_1, 1)

#NORMAL PROBABILITY PLOT
plot(model_1, 2)

#THE SCALE-LOCATION PLOT TO CHECK FOR HOMOSCEDASTICITY ASSUMPTION
plot(model_1, 3)

#MULTICOLLINEARITY CHECK.
vif(model_1)


#WE DEFINE THE COEFFICIENTS FROM THE MODEL SUMMARY OUTPUT
intercept <- coef(summary(model_1))["(Intercept)", "Estimate"]
oil_rents <- coef(summary(model_1))["Oil_rents", "Estimate"]
gas_rents <- coef(summary(model_1))["Natural_gas_rents", "Estimate"]
forest_rents <- coef(summary(model_1))["Forest_rents", "Estimate"]


#MULTIPLE LINEAR REGRESION MODEL FOR SUB SAHARA AFRICA REGION
model_2 <- lm(Resource_GDP ~ Oil_rents + Mineral_rents + Forest_rents,
              sub_sahara_africa)

#SUMMARY OF THE MODEL
summary.lm(model_2)

#SCATTER PLOT MATRIX FOR THE 5 VARIABLES
pairs(sub_sahara_africa[,c(3,5,6,7)], lower.panel = NULL, pch =19, cex =0.2)

#RESIDUAL INDEPENDENT PLOT
plot(model_2, 1)

#NORMAL PROBABILITY PLOT
plot(model_2, 2)

#THE SCALE-LOCATION PLOT TO CHECK FOR HOMOSCEDASTICITY ASSUMPTION
plot(model_2, 3)

#MULTICOLLINEARITY CHECK
vif(model_2)

#WE DEFINE THE COEFFICIENTS FROM THE MODEL SUMMARY OUTPUT
intercept <- coef(summary(model_2))["(Intercept)", "Estimate"]
oil_rents <- coef(summary(model_2))["Oil_rents", "Estimate"]
gas_rents <- coef(summary(model_2))["Mineral_rents", "Estimate"]
forest_rents <- coef(summary(model_2))["Forest_rents", "Estimate"]


#HYPOTHESIS TESTING 1
#we use the shapiro test to check the distribution
forent_df <-  natural_resource %>% 
  filter(Year == 2019)%>%
  filter(Regions %in% c('North Africa', 'Sub-Saharan Africa'))
forent_df 

#WE ACCESS THE NORMALITY OF THE VARIABLES
natural_resource %>% 
  filter(Year %in% c(2019)) %>%
  filter(Regions %in% c('North Africa', 'Sub-Saharan Africa')) %>%
  ggplot(aes(sample = Resource_GDP)) +      
  geom_qq() +                         
  geom_qq_line(colour = "blue") +      
  facet_grid(Year ~ Regions)+
  xlab("Theoretical") + ylab("Sample")


#WE PERFORM SHAPIRO-WILK TEST
shapiro.test(forent_df$Resource_GDP) # the p-value is less than 0.05, means the data is not normally distributed


#WE PERFORM A LOG TRANSFORMATION TO MAKE THE DISTRIBUTION NORMAL 
forent_df$logResource_GDP = log10(forent_df$Resource_GDP)
forent_df


#WE PERFORM SHAPIRO-WILK TEST  
shapiro.test(forent_df$logResource_GDP)#the p-value is greater than 0.05, means that the data is normally distributed


#BOX PLOT OF THE RESOURCE GDP OF THE TWO REGION
forent_df %>% 
  filter(Year %in% c(2019)) %>%
  ggplot(aes(x = Regions, y = logResource_GDP)) +
  geom_boxplot() +
  facet_wrap(~ Year)


#WE TEST THE HYPOTHESIS 
ttest_test <- forent_df %>%              
  t.test(logResource_GDP ~ Regions, data = .)      
ttest_test # the p-value is less than 0.05, we fail to accept the hypothesis


#HYPOTHESIS TESTING 2
pair_data <- natural_resource %>%           
  filter(Year %in% c(2015, 2020)) %>%  
  filter(Regions == "North Africa")          
pair_data

pair_data %>%      
  ggplot(aes(x = Year, y = Natural_gas_rents, 
             group = Country)) +       
  geom_line()

pair_table <- pair_data %>%        
  select(Country, Year, Natural_gas_rents) %>%   #WE SELECT VARIABLES OF INTEREST
  pivot_wider(names_from = Year,       
             values_from = Natural_gas_rents) %>% 
  mutate(
    gas_rents = `2015` - `2020`       
  )
pair_table


#MEANS OF THE DIFFERENCE IN YEARS
pair_table %>% summarise( mean(gas_rents) )

#WE PERFORM THE PAIRED TEST HYPOTHESIS.
pair_data %>% 
  t.test(Natural_gas_rents ~ Year, data = ., paired = TRUE)


#TIME SERIES ANALYSIS 
#SUM RESOURCE GDP FOR NORTH AFRICA REGION
north_africa_GDP <- data.frame(
  Resource_GDP = c(with(north_africa, sum(Resource_GDP[Year == '2009'])),
          with(north_africa, sum(Resource_GDP[Year == '2010'])),
          with(north_africa, sum(Resource_GDP[Year == '2011'])),
          with(north_africa, sum(Resource_GDP[Year == '2012'])),
          with(north_africa, sum(Resource_GDP[Year == '2013'])),
          with(north_africa, sum(Resource_GDP[Year == '2014'])),
          with(north_africa, sum(Resource_GDP[Year == '2015'])),
          with(north_africa, sum(Resource_GDP[Year == '2016'])),
          with(north_africa, sum(Resource_GDP[Year == '2017'])),
          with(north_africa, sum(Resource_GDP[Year == '2018'])),
          with(north_africa, sum(Resource_GDP[Year == '2019'])),
          with(north_africa, sum(Resource_GDP[Year == '2020']))
          ))
north_africa_GDP

#SUM RESOURCE GDP FOR SUB SAHARA AFRICA REGION
sub_sahara_GDP <- data.frame(
  Resource_GDP = c(with(sub_sahara_africa, sum(Resource_GDP[Year == '2009'])),
                   with(sub_sahara_africa, sum(Resource_GDP[Year == '2010'])),
                   with(sub_sahara_africa, sum(Resource_GDP[Year == '2011'])),
                   with(sub_sahara_africa, sum(Resource_GDP[Year == '2012'])),
                   with(sub_sahara_africa, sum(Resource_GDP[Year == '2013'])),
                   with(sub_sahara_africa, sum(Resource_GDP[Year == '2014'])),
                   with(sub_sahara_africa, sum(Resource_GDP[Year == '2015'])),
                   with(sub_sahara_africa, sum(Resource_GDP[Year == '2016'])),
                   with(sub_sahara_africa, sum(Resource_GDP[Year == '2017'])),
                   with(sub_sahara_africa, sum(Resource_GDP[Year == '2018'])),
                   with(sub_sahara_africa, sum(Resource_GDP[Year == '2019'])),
                   with(sub_sahara_africa, sum(Resource_GDP[Year == '2020']))
  ))
sub_sahara_GDP

#TIME SERIES AND FORECAST FOR NORTH AFRICA REGION RESOURCE GDP 
North_africa_tseries <- ts(north_africa_GDP, frequency = 1, start = c(2009))
North_africa_tseries
acf(North_africa_tseries, main = 'North Africa Region Resource GDP')
pacf(North_africa_tseries,main = 'North Africa Region Resource GDP')
adf.test(North_africa_tseries)

plot(North_africa_tseries, main='North Africa Region Resource GDP over 12 years')

north_africa_GDP.forecast <-HoltWinters(North_africa_tseries, beta=FALSE, gamma=FALSE)
north_africa_GDP.forecast

north_africa_GDP.forecast$fitted
plot(north_africa_GDP.forecast)
north_africa_GDP.forecast$SSE
HoltWinters(North_africa_tseries, beta=FALSE, gamma=FALSE, l.start = 	
              94.22505)
north_africa_GDP.forecast2 <- forecast(north_africa_GDP.forecast, h=12)
north_africa_GDP.forecast2
plot(north_africa_GDP.forecast2, main = 'North Africa Region Resource GDP Forecast from HoltWinters'
     ,xlab='Year',
     ylab='Resource GDP',
     sub='Current and next 12 years') #North Africa Region Resource GDP Forecast Plot from HoltWinters
acf(north_africa_GDP.forecast2$residuals, lag.max = 20, na.action=na.pass)
Box.test(north_africa_GDP.forecast2$residuals, lag = 8, type='Ljung-Box')
plot.ts(north_africa_GDP.forecast2$residuals)


#TIME SERIES AND FORECAST FOR SUB SAHARA AFRICA REGION RESOURCE GDP 
sub_sahara_tseries <- ts(sub_sahara_GDP, frequency = 1, start = c(2009))
sub_sahara_tseries
acf(sub_sahara_tseries, main = 'Sub Sahara Africa Region Resource GDP')
pacf(sub_sahara_tseries,main = 'Sub Sahara Africa Region Resource GDP')
adf.test(sub_sahara_tseries)

plot(sub_sahara_tseries, main='Sub Sahara Africa Region Resource GDP over 12 years')

sub_sahara_GDP.forecast <-HoltWinters(sub_sahara_tseries, beta=FALSE, gamma=FALSE)
sub_sahara_GDP.forecast

sub_sahara_GDP.forecast$fitted
plot(sub_sahara_GDP.forecast)
sub_sahara_GDP.forecast$SSE
HoltWinters(sub_sahara_tseries, beta=FALSE, gamma=FALSE, l.start = 	
              54.08730)
sub_sahara_GDP.forecast2 <- forecast(sub_sahara_GDP.forecast, h=12)
sub_sahara_GDP.forecast2
plot(sub_sahara_GDP.forecast2, main = 'Sub Sahara Africa Region Resource GDP Forecast from HoltWinters'
     ,xlab='Year',
     ylab='Resource GDP',
     sub='Current and next 12 years') #Sub Sahara Africa Region Resource GDP Forecast Plot from HoltWinters
acf(sub_sahara_GDP.forecast2$residuals, lag.max = 20, na.action=na.pass)
Box.test(sub_sahara_GDP.forecast2$residuals, lag = 8, type='Ljung-Box')
plot.ts(sub_sahara_GDP.forecast2$residuals)