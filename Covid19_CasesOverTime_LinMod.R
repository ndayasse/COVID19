# Covid19 cases over time
#   Comparing various states/countries 

#### Load packages ##############
#library(tidyr) #for general data wrangling etc
library(tidyverse) #for general data wrangling etc
library(plyr) #for other general data wrangling etc (e.g. join_all)
library(lme4) #for linear models
library(ggplot2) #for graphing
library(Hmisc) #for viewing metadata, rcorr (p-values matrix), and other helpful things
#also can use for some methods of imputing missing data
library(stringr) #for certain functions acting on strings/character vectors
library(tibble) #useful for some dealing with wrangling data
library(sjlabelled) #for labelled data and labelling data
library(VIM) #for plotting missing values
library(viridis) #for color/fill scale for graphing
library(mice) #for viewing and handling missing data
library(dplyr) #for general data wrangling etc (needs to be loaded after certain packages to avoid issues)


#### Read in numbers data, Cumulative Cases Only: #########

international.numbers.time <- read.csv(
  "~/UNCOVER/johns_hopkins_csse/johns-hopkins-covid-19-daily-dashboard-cases-over-time.csv")
us.numbers.time <- read.csv(
  "~/UNCOVER/johns_hopkins_csse/2019-novel-coronavirus-covid-19-2019-ncov-data-repository-confirmed-cases-in-the-us.csv")

### Wrangle data: ######

# select only needed columns from US information:
us.numbers.time <- dplyr::select(us.numbers.time,c(admin2,province_state,date,confirmed))
# Create a version with just state information, summed across counties:
#   Note: dplyr needs to be loaded AFTER plyr for this to work, if an issue detach plyr
us.numbers.time <- us.numbers.time %>% #pip (%>%) says apply all these following functions to this data frame
  dplyr::group_by(province_state, date) %>%
  summarise(confirmed_cases = sum(confirmed,na.rm=TRUE))
us.numbers.time <- as.data.frame(us.numbers.time)

# Filter only selected states to view:
us.numbers.time.sel <- filter(us.numbers.time,province_state=="New York"|province_state=="Massachusetts"|
                            province_state=="Washington"|province_state=="California")

# make date a date:
us.numbers.time.sel$date <- as.Date(us.numbers.time.sel$date)

# Rename some cols from international df to be comparable across df's:
international.numbers.time <- dplyr::rename(international.numbers.time,confirmed_cases=confirmed)
#     ^new name on left, old name on right
international.numbers.time <- dplyr::rename(international.numbers.time,date=last_update)
#     ^new name on left, old name on right

# Filter only selected countries to view:
international.numbers.time.sel <- filter(international.numbers.time,country_region=="Italy"|
                                       country_region=="United Kingdom"|country_region=="Germany"|
                                       country_region=="New Zealand")
# Select only needed columns:
international.numbers.time.sel <- dplyr::select(international.numbers.time.sel,c(country_region,
                                                                                 date,confirmed_cases))
# make date a date:
international.numbers.time.sel$date <- as.Date(international.numbers.time.sel$date)

# Rename cols from US and int'l dataframes to be comparable:
international.numbers.time.sel <- dplyr::rename(international.numbers.time.sel,region=country_region)
#     ^new name on left, old name on right
us.numbers.time.sel <- dplyr::rename(us.numbers.time.sel,region=province_state)
#     ^new name on left, old name on right

# put selected dataframes together:
selected.numbers.time <- rbind(international.numbers.time.sel,us.numbers.time.sel)

## Create a column counting days from first case: ######

# create a df filtered for once cases begin in an area:
selected.numbers.time.nonzero <- filter(selected.numbers.time,confirmed_cases!=0)
selected.numbers.time.nonzero$region <- factor(selected.numbers.time.nonzero$region)
#   ^do this to get rid of extra levels
# get day 1 date for each region:
selected.numbers.time.day0datetbl <- selected.numbers.time.nonzero %>% 
  dplyr::group_by(region) %>% #note: without arranging, region levels in same order as larger df
  summarise(date_day0 = min(date,na.rm=TRUE)) 
# count days from day 1:
selected.numbers.time.nonzero$Days_Count <- NA
for (i in levels(selected.numbers.time.nonzero$region)) { #for each region
  temp <- filter(selected.numbers.time.nonzero,region==i)
  temp <- mutate(temp,Days_Count = difftime(date,selected.numbers.time.day0datetbl[which(
    selected.numbers.time.day0datetbl$region==i),"date_day0"][[1]],
    units=c("days")))
  selected.numbers.time.nonzero[which(selected.numbers.time.nonzero$region==i),] <- temp
}



#### Graph cumulative cases from day 0: #####

ggplot(data=selected.numbers.time.nonzero, aes(x=Days_Count, y=confirmed_cases, color=region, 
                                               fill=region, shape=region)) +
  theme_bw() +
  # stat_summary(fun.data=mean_se, geom="ribbon",  alpha=.25 ) +
  stat_summary(fun.data=mean_se, geom="point",  size = 2.0, alpha=0.9 ) +
  # stat_summary(fun.data=mean_se, geom="pointrange", size = .10) +
  #stat_summary(aes(y=fitted(model.all.v8)), fun.y=mean, geom="line", size = 1) +
  labs(y="Cumulative Cases", x="Days from First Case") +
  theme(text=element_text(color = "black", size=20, family = "Arial")) +
  theme(axis.text = element_text(color = "black", size=10, family = "Arial")) +
  scale_color_viridis(begin=0, end = .8, discrete=TRUE) +
  scale_fill_viridis(begin=0, end = .8, discrete=TRUE) +
  scale_shape_manual(values=c(0:8))
  # scale_linetype_manual(values=c("dotted", "solid")) +
  # coord_cartesian(xlim = c(-500, 2500))
# geom_vline(xintercept=1000)
ggsave("Covid19_CumCasesOverTime_FromDay0_8StatesCountries_28Apr2020_1850.png",width=9,height=7)



#### Read in numbers data, New Cases and Cum Cases: ######

who.new.cum.cases.time <- read.csv(
  "~/UNCOVER/WHO/who-situation-reports-covid-19.csv")

### Wrangle data: #####

# select only countries of interest:
who.new.cum.cases.time.sel <- filter(who.new.cum.cases.time,reporting_country_territory=="Italy"|
                                       reporting_country_territory=="United Kingdom"|
                                       reporting_country_territory=="Germany"|
                                       reporting_country_territory=="Taiwan"|
                                       reporting_country_territory=="New Zealand"|
                                       reporting_country_territory=="United States of America")

# rename United States of America to be United States:
levels(who.new.cum.cases.time.sel$reporting_country_territory)[
  levels(who.new.cum.cases.time.sel$reporting_country_territory)=="United States of America"] <- 
  "United States"

## Create a column counting days from first case: ######

# create a df filtered for once cases begin in an area:
who.new.cum.cases.time.sel.nonzero <- filter(who.new.cum.cases.time.sel,confirmed_cases!=0)
who.new.cum.cases.time.sel.nonzero$reporting_country_territory <- 
  factor(who.new.cum.cases.time.sel.nonzero$reporting_country_territory)
#   ^do this to get rid of extra levels
# make reported_date a date-type variable:
who.new.cum.cases.time.sel.nonzero$reported_date <- as.Date(who.new.cum.cases.time.sel.nonzero$reported_date)
# get day 1 date for each region:
selected.who.day0datetbl <- who.new.cum.cases.time.sel.nonzero %>% 
  dplyr::group_by(reporting_country_territory) %>% #note: without arranging, region levels in same order as larger df
  summarise(date_day0 = min(reported_date,na.rm=TRUE)) 
# count days from day 1:
who.new.cum.cases.time.sel.nonzero$Days_Count <- NA
for (i in levels(who.new.cum.cases.time.sel.nonzero$reporting_country_territory)) { #for each region
  temp <- filter(who.new.cum.cases.time.sel.nonzero,reporting_country_territory==i)
  temp <- mutate(temp,Days_Count = difftime(reported_date,selected.who.day0datetbl[which(
    selected.who.day0datetbl$reporting_country_territory==i),"date_day0"][[1]],
    units=c("days")))
  who.new.cum.cases.time.sel.nonzero[which(
    who.new.cum.cases.time.sel.nonzero$reporting_country_territory==i),] <- temp
}


#### Graph new cases from day 0: #####

ggplot(data=who.new.cum.cases.time.sel.nonzero, aes(x=Days_Count, y=new_confirmed_cases, 
                                                    color=reporting_country_territory, 
                                                    fill=reporting_country_territory, 
                                                    shape=reporting_country_territory)) +
  theme_bw() +
  # stat_summary(fun.data=mean_se, geom="ribbon",  alpha=.25 ) +
  stat_summary(fun.data=mean_se, geom="point",  size = 2.0, alpha=0.9, stroke=1.5 ) +
  # stat_summary(fun.data=mean_se, geom="pointrange", size = .10) +
  #stat_summary(aes(y=fitted(model.all.v8)), fun.y=mean, geom="line", size = 1) +
  labs(y="New Cases", x="Days from First Case") +
  theme(text=element_text(color = "black", size=20, family = "Arial")) +
  theme(axis.text = element_text(color = "black", size=10, family = "Arial")) +
  theme(legend.title = element_blank()) +
  scale_color_viridis(begin=0, end = .8, discrete=TRUE) +
  scale_fill_viridis(begin=0, end = .8, discrete=TRUE) +
  scale_shape_manual(values=c(0:8))
# scale_linetype_manual(values=c("dotted", "solid")) +
# coord_cartesian(xlim = c(-500, 2500))
# geom_vline(xintercept=1000)
ggsave("Covid19_NewCasesOverTime(WHO)_FromDay0_8StatesCountries_29Apr2020_1104.png",width=9,height=7)




#### Model New Cases from Day 0: ######

# make a version (to get rid of NA's on):
who.new.cum.cases.time.sel.nonzero.nona <- dplyr::select(who.new.cum.cases.time.sel.nonzero,
                                                        c(reporting_country_territory,
                                                          new_confirmed_cases,
                                                          Days_Count))
who.new.cum.cases.time.sel.nonzero.nona <- na.omit(who.new.cum.cases.time.sel.nonzero.nona)

model.0 <- lm(new_confirmed_cases ~ 1 , 
              data=who.new.cum.cases.time.sel.nonzero.nona)
model.1 <- lm(new_confirmed_cases ~ Days_Count , 
              data=who.new.cum.cases.time.sel.nonzero.nona)
model.2 <- lm(new_confirmed_cases ~ Days_Count + reporting_country_territory,
              data=who.new.cum.cases.time.sel.nonzero.nona)
model.3 <- lm(new_confirmed_cases ~ Days_Count + reporting_country_territory
                  + Days_Count:reporting_country_territory,
                  data=who.new.cum.cases.time.sel.nonzero.nona)
anova(model.0, model.1, model.2, model.3)
summary(model.0)
summary(model.1)
summary(model.2)
summary(model.3)

#Note: models indicate that the US (among the tested countries)
#      has the highest intercept and the steepest increase

#### Graph Model: #####

# Graph New Cases + Models from Day 0
ggplot(data=who.new.cum.cases.time.sel.nonzero.nona, aes(x=Days_Count, y=new_confirmed_cases, 
                                                    color=reporting_country_territory, 
                                                    fill=reporting_country_territory, 
                                                    shape=reporting_country_territory,
                                                    linetype=reporting_country_territory)) +
  theme_bw() +
  stat_summary(fun.data=mean_se, geom="point",  size = 2.0, alpha=0.9, stroke=1.5 ) +
  stat_summary(aes(y=fitted(model.3)), fun=mean, geom="line", size = 1) +
  labs(y="New Cases", x="Days from First Case") +
  theme(text=element_text(color = "black", size=20, family = "Arial")) +
  theme(axis.text = element_text(color = "black", size=10, family = "Arial")) +
  theme(legend.title = element_blank()) +
  scale_color_viridis(begin=0, end = .8, discrete=TRUE) +
  scale_fill_viridis(begin=0, end = .8, discrete=TRUE) +
  scale_shape_manual(values=c(0:8))
# scale_linetype_manual(values=c("dotted", "solid")) +
# coord_cartesian(xlim = c(-500, 2500))
# geom_vline(xintercept=1000)
ggsave("Covid19_LM_NewCasesOverTime(WHO)_FromDay1_5Countries_29Apr2020_1613.png",width=9,height=7)



