library(dplyr)
library(tidyverse)
library(caret)
library(rpart)
library(rattle)
library(RColorBrewer)
library(sjstats)

setwd('/Users/davidfan/Downloads/Attribution Data')
income = read.csv('london_taxpayer_income.csv') %>% select(year,borough,median_income) %>% filter(year == 2011)
enter = read.csv('enterprise.csv')
ethnic = read.csv('ethnic.csv')
house = read.csv('housing_price.csv') %>% filter(year == 2011) %>% gather("borough","house_price",-year) %>% select(-year)
unemploy = read.csv('unemployment.csv') %>% filter(year == 2011) %>% gather("borough","unemployment",-year) %>% select(-year)
bars = read.csv('bars.csv') %>% filter(year == 2011) %>% gather("borough","bars",-year) %>% select(-year)
cars = read.csv('car_traffic_kilometers.csv') %>% filter(year == 2011) %>% gather("borough","cars",-year) %>% select(-year)
prod = read.csv('productivity_per_hour.csv') %>% filter(year == 2011) %>% gather("borough","prod",-year) %>% select(-year)
jobs = read.csv('productivity_filled_jobs.csv') %>% filter(year == 2011) %>% gather("borough","jobs",-year) %>% select(-year)
motor = read.csv('motor_vehicle_flow.csv') %>% filter(year == 2011) %>% gather("borough","motor",-year) %>% select(-year)
gdp = read.csv('gdp_with_international_cities.csv') %>% filter(year == 2011) %>% gather("borough","gdp",-year) %>% select(-year)
kepler = read.csv('kepler-gl_gdp_london.json.csv')
kepler = kepler %>% select(ctyua16nm,distance_from_olympic_park)
names(kepler) = c('borough','distance')

income[['borough']] = gsub(' ','.',income[['borough']])
enter[['borough']] = gsub(' ','.',enter[['borough']])
ethnic[['borough']] = gsub(' ', '.', ethnic[['borough']])

# ATE = value_aggre %>% filter(year == 2012) %>% gather("borough", "ATE", -year) %>% select(-year)
# ATE[['borough']] = gsub('_', '.',ATE[['borough']])

agg = read_csv("percent_change_gdp.csv")
val = read_csv("percent_change_gdp_year.csv")

val = val %>% select(-X1) %>% mutate(ATE = percent) %>% select(borough, year,ATE)
val[['borough']] = gsub('_', '.',val[['borough']])

agg = agg %>% select(-X1) %>% mutate(ATE = percent) %>% select(borough, ATE)
agg[['borough']] = gsub('_', '.',agg[['borough']])

kepler[['borough']] = gsub('_', '.',kepler[['borough']])
kepler[['borough']] = gsub('and ', '',kepler[['borough']])
kepler[['borough']] = gsub(' ', '.',kepler[['borough']])
kepler[['borough']] = tolower(kepler[['borough']])

feature_df = merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(merge(enter, ethnic, by = "borough"), house, by = "borough"), unemploy, by = "borough"), bars, 
                                                       by = "borough"), cars, by = "borough"), prod, by = "borough"), jobs, by = "borough"), motor, by = "borough"), 
                         gdp,by = 'borough'),kepler, by = "borough"),income,by = 'borough')


feature_df$bars = sapply(FUN = as.integer, feature_df$bars)
names(feature_df)

#Traffic Indexes = motor, cars, Transport.and.Storage.incl.postal, Motor.Trades
#Leisure Indexes = bars, Arts.entertainment.and.recreation
#Consumption Indexes = Retail,Accommodation.and.food.services, Wholesale
#Property Indexes = Property, house_price
#Production Indexes = Production, Construction, prod
#Scientific Indexes = Information.and.Communication, Professional.scientific.and.technical, Education
#Minority Indexes = Asian, Black, Mixed..Other
#Econ Indexes = unemployment, gdp

#1 PC = 60% of variations
pca1 = prcomp(feature_df %>% select("motor", "cars", "Transport.and.Storage.incl.postal", "Motor.Trades"),scale. = T)
summary(pca1)
pca1$rotation

#1 PC = 70% of variations
pca2 = prcomp(feature_df %>% select("bars", "Arts.entertainment.and.recreation"),scale. = T)
summary(pca2)
pca2$rotation

#1 PC = 85% of variations
pca3 = prcomp(feature_df %>% select("Retail","Accommodation.and.food.services"),scale. = T)
summary(pca3)
pca3$rotation

#1 PC = 81% of variations
pca4 = prcomp(feature_df %>% select("Property","house_price"),scale. = T)
summary(pca4)
pca4$rotation

#1 PC = 88% of variations
pca5 = prcomp(feature_df %>% select("Production", "Construction"),scale. = T)
summary(pca5)
pca5$rotation

#1 PC = 60% of variations
pca6 = prcomp(feature_df %>% select("unemployment", "gdp","median_income"),scale. = T)
summary(pca6)
pca6$rotation

df2 = data.frame(pca2$x[,'PC1'],
                 pca6$x[,'PC1'],
                 feature_df %>% select('borough','Asian', 
                                       'Black', 
                                       'Mixed..Other'))

names(df2) = c('Leisure.Indexes',
               'Econ.Indexes'
               ,'borough','Asian', 
               'Black', 
               'Mixed..Other')

# corrplot::corrplot(cor(df2 %>% select(-borough)))

df = merge(df2, agg, by = 'borough')

tree = rpart("ATE ~.-borough", df, control = rpart.control(minsplit = 10))
fancyRpartPlot(tree, palettes = 'Blues')

#Major Indices Per Year
df2 = data.frame(pca2$x[,'PC1'],
                 pca6$x[,'PC1'],
                 feature_df %>% select('borough'))


names(df2) = c('Leisure.Indexes',
               'Econ.Indexes',
               'borough')

df2
df = merge(df2, val, by = 'borough')


#back attribution
z = c()

for (i in c(2012:2016)){
  temp = df %>% filter(year == i) %>% select(-year)
  
  fit = lm(log(ATE + 1) ~ .-borough, temp)
  tempdf = robust(fit)
  tempdf$year = i
  z = rbind(z,tempdf)
}

ggplot(data = z, aes(x = year, y = estimate)) + 
  geom_line(color = 'blue4', size = 1.5)+ 
  geom_line(aes(x = year, y = estimate + std.error), linetype = 'dashed')+ 
  geom_line(aes(x = year, y = estimate - std.error), linetype = 'dashed')+
  geom_ribbon(aes(x = year, ymin = estimate - std.error, ymax =  estimate + std.error), 
              fill = "grey", alpha = 0.5)+
  facet_wrap(~term, scales = 'free_y')



#Minorities Per Year
df2 = data.frame(feature_df %>% select('Asian', 
                                       'Black', 
                                       'Mixed..Other',
                                       'borough'))

df = merge(df2, val, by = 'borough')

#back attribution
z = c()

for (i in c(2012:2016)){
  temp = df %>% filter(year == i) %>% select(-year)
  
  fit = lm(ATE ~ .-borough, temp)
  tempdf = robust(fit)
  tempdf$year = i
  z = rbind(z,tempdf)
}

ggplot(data = z, aes(x = year, y = estimate)) + 
  geom_line(color = 'blue4', size = 1.5)+ 
  geom_line(aes(x = year, y = estimate + std.error), linetype = 'dashed')+ 
  geom_line(aes(x = year, y = estimate - std.error), linetype = 'dashed')+
  geom_ribbon(aes(x = year, ymin = estimate - std.error, ymax =  estimate + std.error), 
              fill = "grey", alpha = 0.5)+
  facet_wrap(~term, scales = 'free_y')
