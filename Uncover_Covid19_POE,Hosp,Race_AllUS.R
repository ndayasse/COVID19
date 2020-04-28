# Population at risk for contracting covid19
#   Looking at County-Level information
#   Interested in ports of entry, hospital capacity (per population), racial diversity/% minorities

#### Load packages ##############
#library(tidyr) #for general data wrangling etc
library(tidyverse) #for general data wrangling etc
library(plyr) #for other general data wrangling etc (e.g. join_all)
library(lme4) #for linear models
library(ggplot2) #for graphing
library(Hmisc) #for viewing metadata, rcorr (p-values matrix), and other helpful things
  #also can use for some methods of imputing missing data
library(broom) #for turning summaries of models into tidy tibbles
library(httr) #for reading in excel files off the internet or something
library(stringr) #for certain functions acting on strings/character vectors
library(tibble) #useful for some dealing with wrangling data
library(sjlabelled) #for labelled data and labelling data
library(VIM) #for plotting missing values
library(viridis) #for color/fill scale for graphing
library(mice) #for viewing and handling missing data
#library(DMwR) #for KNN imputation
library(mi) #has multiple imputation methods
library(betareg) #need for mi imputation methods
library(dplyr) #for general data wrangling etc (needs to be loaded after certain packages to avoid issues)


#### US Predictors of Contracting C19: ######


# US ports of entry:
hifld.poe.df <- read.csv("~/Documents/Rdirectory/UNCOVER/hifld/hifld/us-ports-of-entry.csv")
#^ can give # of different ports of entry per state (if transformed into that)
#the following two sections are switching on which works properly...
poe.num.df <- hifld.poe.df %>%
count(x_state)
poe.num.df <- as.data.frame(poe.num.df) #turn to dataframe
poe.num.df <- dplyr::rename(poe.num.df,state_code=x_state) #rename variable
poe.num.df <- dplyr::rename(poe.num.df,num_ports_of_entry=n) #rename variable
# poe.num.df <- count(hifld.poe.df$x_state) #get counts of ports of entry (water or airport) #NOTE: code stopped working later on?
# poe.num.df <- as.data.frame(poe.num.df) #turn to dataframe
# poe.num.df <- dplyr::rename(poe.num.df,state_code=x) #rename variable
# poe.num.df <- dplyr::rename(poe.num.df,num_ports_of_entry=freq) #rename variable


# facility information/capacity
esri.fac.df <- read.csv("~/Documents/Rdirectory/UNCOVER/esri_covid-19/esri_covid-19/definitive-healthcare-usa-hospital-beds.csv",
                        na.strings=c("","****","NA"))
#^ def healthcare usa hospital beds -- gives hospitals and # of staff and ICU beds for city, state, county, 
#   with fips (county, state, general fips)
# Get counts/numbers of health facilities:
esri.numfac.df <- count(esri.fac.df$fips) #get counts of health facilities
esri.numfac.df <- as.data.frame(esri.numfac.df) #turn to dataframe
esri.numfac.df <- dplyr::rename(esri.numfac.df,fips=x) #rename variable
esri.numfac.df <- dplyr::rename(esri.numfac.df,num_health_fac=freq) #rename variable
esri.numfac.df <- as.data.frame(esri.numfac.df)
# Get sum/numbers of staff members (across facilities):
#   Note: dplyr needs to be loaded AFTER plyr for this to work, if an issue detach plyr
esri.fac.staff.df <- esri.fac.df %>% #pip (%>%) says apply all these following functions to this data frame
  dplyr::group_by(fips) %>%
  summarise(num.staff = sum(num_staffe,na.rm=TRUE))
esri.fac.staff.df <- as.data.frame(esri.fac.staff.df)
# Get sum/numbers of ICU beds (across facilities):
esri.fac.icubeds.df <- esri.fac.df %>% #pip (%>%) says apply all these following functions to this data frame
  dplyr::group_by(fips) %>%
  summarise(num.icubeds = sum(num_icu_be,na.rm=TRUE))
esri.fac.icubeds.df <- as.data.frame(esri.fac.icubeds.df)
# Now merge those new columns:
#   First select the needed cols from the original facility df:
esri.fac.df <- dplyr::select(esri.fac.df,one_of(c("county_nam","state_name","state_fips","cnty_fips","fips")))
esri.fac.list <- list(esri.fac.df,esri.numfac.df,esri.fac.staff.df,esri.fac.icubeds.df)
#   join all df's:
esri.fac.df <- join_all(esri.fac.list, by = NULL, type = "full", match = "all")
#by=NULL makes it join by all common vars, type=full makes it do a full_join, match=all makes it compatible to merge


# county demographics:
us.county.demog <- read.csv("~/Documents/Rdirectory/UNCOVER/2016-us-election/county_facts.csv")
col.labels <- base::t(read.csv("~/Documents/Rdirectory/UNCOVER/2016-us-election/county_facts_dictionary.csv"))
#   ^gives the information of what each column is
col.labels <- base::t(col.labels[2,])
col.labels.first3 <- c("full fips","name of county or state or country","state 2-letter code")
#   ^must give these first 3 since these aren't given in the dictionary
col.labels <- cbind(base::t(col.labels.first3),col.labels)
sjlabelled::set_label(us.county.demog) <- col.labels #supply labels/info
# select columns of interest:
us.county.demog.ofinterest <- dplyr::select(us.county.demog,one_of(c("fips","area_name","state_abbreviation",
                                                                     "PST045214","AGE135214","AGE775214",
                                                                     "SEX255214","RHI125214","RHI225214",
                                                                     "RHI325214","RHI425214","RHI525214",
                                                                     "RHI625214","RHI725214","EDU635213",
                                                                     "EDU685213","LFE305213","HSG445213",
                                                                     "HSD310213","INC910213","INC110213",
                                                                     "PVY020213","POP060210")))
us.county.demog.ofinterest <- sjlabelled::copy_labels(us.county.demog.ofinterest,us.county.demog) 
#       ^supply labels/info
# Rename some vars etc. to be comparable:
us.county.demog.ofinterest <- dplyr::rename(us.county.demog.ofinterest,state_code=state_abbreviation)
#     ^new name on left, old name on right


## Start joining these df's, careful of order ######

us.county.state.info <- dplyr::full_join(us.county.demog.ofinterest,poe.num.df,by=NULL)
#by=NULL makes it join by whatever is in common
# joining these two dataframes first because they share state_code information
# Note: will coerce to character vector since us county demographics does not include places like Puerto Rico, 
#   but ports of entry does
us.county.state.info <- dplyr::full_join(us.county.state.info,esri.fac.df,by=NULL)
#   ^This will now join by fips (full fips) code
# Copy labels back over:
us.county.state.info <- sjlabelled::copy_labels(us.county.state.info,us.county.demog.ofinterest)
# Get rid of duplicate rows:
us.county.state.info <- unique(us.county.state.info)

#### US numbers (tests, cases, deaths, recovered, etc.): #######

# cases and deaths by state and county US:
nyt.cases.deaths.list <- "~/Documents/Rdirectory/UNCOVER/New_York_Times/covid-19-county-level-data.csv"
#NOTE: includes multiple dates - info from former days as well as current
#called "date" (#1 in df list)
# confirmed and recovered cases, US and global:
cases.recov.pop.list <- c("~/Documents/Rdirectory/UNCOVER/johns_hopkins_csse/johns-hopkins-covid-19-daily-dashboard-cases.csv",
                          "~/Documents/Rdirectory/UNCOVER/johns_hopkins_csse/2019-novel-coronavirus-covid-19-2019-ncov-data-repository-confirmed-deaths-in-the-us.csv")
#***^ daily dashboard cases gives US states AND county/city for confirmed / deaths / recovered / active 
#NOTE: just has CURRENT info, not former days' info
#called "last_update" and includes timestamp as well (#2 in df list)
#***^ ncov data repository confirmed deaths in the US gives for US state/territory and some cities/counties (col: admin2) deaths due to COVID19 AND population # info
#^note: fips here seems to mean either county or state fips, and admin2 indicates whether county or city info is given or it is "Unassigned" and just state info
#Note: date called "date" but is MISSING for all (#3 in df list) <- just want population info though
# for just NY in USA:
ny.tests.cases.list <- "~/Documents/Rdirectory/UNCOVER/ny_dept_of_health/new-york-state-statewide-covid-19-testing.csv"
#^ the only US testing info I could find at a state/county level (could find whole-US info)
#NOTE: includes multiple dates - info from former days as well as current
#called "test_date" (#4 in df list)


# full list of all:
us.tests.cases.deaths.recov.list <- c(nyt.cases.deaths.list, cases.recov.pop.list, ny.tests.cases.list)
# read in as df's:
us.tests.cases.deaths.recov.df <- lapply(us.tests.cases.deaths.recov.list, read.csv)


## Keep only US-parts of international ones:
us.tests.cases.deaths.recov.df[[2]] <- dplyr::filter(us.tests.cases.deaths.recov.df[[2]],country_region=="US")


## Keep only needed parts to supply population info from #4:
us.tests.cases.deaths.recov.df[[3]] <- dplyr::select(us.tests.cases.deaths.recov.df[[3]],one_of(c("fips","admin2","province_state","population")))


## Rename some vars etc. to be comparable:
us.tests.cases.deaths.recov.df[[1]] <- dplyr::rename(us.tests.cases.deaths.recov.df[[1]],county_fips=fips) #rename var, new name on left old name on right
us.tests.cases.deaths.recov.df[[1]] <- dplyr::rename(us.tests.cases.deaths.recov.df[[1]],confirmed_cases=cases) #rename var, new name on left old name on right
us.tests.cases.deaths.recov.df[[2]] <- dplyr::rename(us.tests.cases.deaths.recov.df[[2]],confirmed_cases=confirmed) #rename var, new name on left old name on right
us.tests.cases.deaths.recov.df[[2]] <- dplyr::rename(us.tests.cases.deaths.recov.df[[2]],state=province_state) #rename var, new name on left old name on right
us.tests.cases.deaths.recov.df[[2]] <- dplyr::rename(us.tests.cases.deaths.recov.df[[2]],county=admin2) #rename var, new name on left old name on right
us.tests.cases.deaths.recov.df[[3]] <- dplyr::rename(us.tests.cases.deaths.recov.df[[3]],state=province_state) #rename var, new name on left old name on right
us.tests.cases.deaths.recov.df[[3]] <- dplyr::rename(us.tests.cases.deaths.recov.df[[3]],county=admin2) #rename var, new name on left old name on right


## Fix up "date" variables:
us.tests.cases.deaths.recov.df[[1]]$date <- as.Date(us.tests.cases.deaths.recov.df[[1]]$date)
us.tests.cases.deaths.recov.df[[2]]$last_update <- as.Date(us.tests.cases.deaths.recov.df[[2]]$last_update)
us.tests.cases.deaths.recov.df[[4]]$test_date <- as.Date(us.tests.cases.deaths.recov.df[[4]]$test_date)


## For those with multiple dates, keep only the most current one (max for dates):
#keep in mind this may be different for different counties, hence the group_by
us.tests.cases.deaths.recov.df[[1]] <- us.tests.cases.deaths.recov.df[[1]] %>% 
  group_by(county) %>%
  filter(date == max(date)) 
us.tests.cases.deaths.recov.df[[4]] <- us.tests.cases.deaths.recov.df[[4]] %>% 
  group_by(county) %>%
  filter(test_date == max(test_date)) 


## Now that dates are all current, can make version where get rid of date columns:
nodate.us.tests.cases.deaths.recov.df <- us.tests.cases.deaths.recov.df
nodate.us.tests.cases.deaths.recov.df[[1]] <- dplyr::select(us.tests.cases.deaths.recov.df[[1]], -date)
nodate.us.tests.cases.deaths.recov.df[[2]] <- dplyr::select(us.tests.cases.deaths.recov.df[[2]], -last_update)
nodate.us.tests.cases.deaths.recov.df[[4]] <- dplyr::select(us.tests.cases.deaths.recov.df[[4]], -test_date)

# make sure are dataframes:
for (i in 1:length(nodate.us.tests.cases.deaths.recov.df)) {
  nodate.us.tests.cases.deaths.recov.df[[i]] <- as.data.frame(nodate.us.tests.cases.deaths.recov.df[[i]])
}


## Combine df's (join):
master.nodate.us.tests.cases.deaths.recov.df <- join_all(nodate.us.tests.cases.deaths.recov.df, by = NULL, type = "full", match = "all")
#by=NULL makes it join by all common vars, type=full makes it do a full_join, match=all makes it compatible to merge
#NOTE: state indicated with "state" variable that gives full state name; also "county" gives full county name, and has county_fips and fips

# if fips = NA, then take county_fips (if it exists, if it is NA it won't matter):
master.nodate.us.tests.cases.deaths.recov.df <- mutate(master.nodate.us.tests.cases.deaths.recov.df,
                                                          fips=case_when(is.na(fips)==1~county_fips,
                                                                         is.na(fips)==0~fips))


#### Combine US predictors with US numbers: ########

# get rid of unneeded columns (latitude and longitude):
master.nodate.us.tests.cases.deaths.recov.df <- dplyr::select(master.nodate.us.tests.cases.deaths.recov.df,
                                                              -c(lat,long))

# Join dataframes:
us.county.state.info.numbers.nodate.df <- dplyr::full_join(us.county.state.info,
                                                           master.nodate.us.tests.cases.deaths.recov.df,
                                                           by=NULL)
#by=NULL makes it join by whatever is in common
#   Note: don't want to rename some other columns that are in common (e.g. names) 
#     since the names are not all in the same format etc., just relying on fips codes

# Copy labels back over:
us.county.state.info.numbers.nodate.df <- sjlabelled::copy_labels(us.county.state.info.numbers.nodate.df,
                                                                  us.county.demog)


### Examine missing data: #####

# make a copy to trim down to just NY:
ny.county.state.info.numbers.nodate.df <- us.county.state.info.numbers.nodate.df
ny.county.state.info.numbers.nodate.df <- filter(ny.county.state.info.numbers.nodate.df,
                                                 state_code=="NY")
# now take out the last four cols that are only there for NY:
us.county.state.info.numbers.nodate.df <- dplyr::select(us.county.state.info.numbers.nodate.df,
                                                        -c(new_positives,cumulative_number_of_positives,
                                                           total_number_of_tests_performed,
                                                           cumulative_number_of_tests_performed))
# Copy labels back over, since select always takes them out:
us.county.state.info.numbers.nodate.df <- sjlabelled::copy_labels(us.county.state.info.numbers.nodate.df,
                                                                  us.county.demog)


# initial checks:
total_cases <- nrow(us.county.state.info.numbers.nodate.df) #number of people responded total
total_nona <- sum(complete.cases(us.county.state.info.numbers.nodate.df)) #count how many complete cases
num_missing <- total_cases - total_nona #calc number missing
prop_missing <- num_missing/total_cases #calc proportion missing

# Check number missing in each column:
num_miss_col_tbl <- us.county.state.info.numbers.nodate.df %>% #save to work with/view easier
  summarise_all(funs(sum(is.na(.))))
num_miss_col_nonzero <- num_miss_col_tbl %>% select_if(~ sum(.) != 0) #select only cols with NA's (for easier viewing)
prop_miss_col_nonzero <- num_miss_col_nonzero / nrow(us.county.state.info.numbers.nodate.df) #calc proportion missing in each col (nonzero only)
prop_miss_col_tbl <- num_miss_col_tbl / nrow(us.county.state.info.numbers.nodate.df) #calc proportion missing in each col
prop_miss_col_tbl_trans <- as.data.frame(t(prop_miss_col_tbl)) #for easier viewing (as a column)

# now take out additional unneeded columns that have missing data:
us.county.state.info.numbers.nodate.df <- dplyr::select(us.county.state.info.numbers.nodate.df,
                                                        -c(county_nam,state_name,state_fips,
                                                           cnty_fips,county_fips,country_region,
                                                           combined_key))
# Copy labels back over, since select always takes them out:
us.county.state.info.numbers.nodate.df <- sjlabelled::copy_labels(us.county.state.info.numbers.nodate.df,
                                                                  us.county.demog)

# Check number missing again:
total_cases <- nrow(us.county.state.info.numbers.nodate.df) #number of people responded total
total_nona <- sum(complete.cases(us.county.state.info.numbers.nodate.df)) #count how many complete cases
num_missing <- total_cases - total_nona #calc number missing
prop_missing <- num_missing/total_cases #calc proportion missing

# make sure no duplicate rows again:
us.county.state.info.numbers.nodate.df <- unique(us.county.state.info.numbers.nodate.df)



#### Examine patterns, predictions, etc.: ########

# Drop extra population estimate (older):
us.county.state.info.numbers.nodate.df <- dplyr::select(us.county.state.info.numbers.nodate.df,-PST045214)
# Copy labels back over, since select always takes them out:
us.county.state.info.numbers.nodate.df <- sjlabelled::copy_labels(us.county.state.info.numbers.nodate.df,
                                                                  us.county.demog)
# make those variables without labels have their names be the label:
us.county.state.info.numbers.nodate.df$state_code <- set_label(
  us.county.state.info.numbers.nodate.df$state_code,"state code")
us.county.state.info.numbers.nodate.df["num_ports_of_entry":"population"] <- set_label(
  us.county.state.info.numbers.nodate.df["num_ports_of_entry":"population"],
  colnames(us.county.state.info.numbers.nodate.df["num_ports_of_entry":"population"]))


## Predict confirmed cases: ######

# Create a df with just confirmed cases as outcome variable, only needed county info vars
us.county.state.info.numbers.nodate.df.confcase <- dplyr::select(us.county.state.info.numbers.nodate.df,
                                                                 -c(fips,area_name,state_code,county,
                                                                    state,deaths,recovered,active,
                                                                    population))
# Copy labels back over, since select always takes them out:
us.county.state.info.numbers.nodate.df.confcase <- sjlabelled::copy_labels(
  us.county.state.info.numbers.nodate.df.confcase,us.county.state.info.numbers.nodate.df)

# Calc the model:
mdl.us.countydemogs.confcase.nums <- 
  lm(confirmed_cases ~ .,label_to_colnames(us.county.state.info.numbers.nodate.df.confcase)) 
#   ^ used label_to_colnames to make the more-informative labels the names for this model
# save as a tibble:
mdl.tbbl.us.countydemogs.confcase.nums <- tidy(mdl.us.countydemogs.confcase.nums)

## Predict confirmed cases proportion: ######

# calculate proportion of confirmed cases:
us.county.state.info.numbers.nodate.df.confcaseprop <- mutate(us.county.state.info.numbers.nodate.df,
                                                              confirmed_cases_prop = 
                                                                confirmed_cases/population)
# Create a df with just confirmed cases as outcome variable, only needed county info vars
us.county.state.info.numbers.nodate.df.confcaseprop <- dplyr::select(
  us.county.state.info.numbers.nodate.df.confcaseprop,-c(fips,area_name,state_code,county,
                                                         state,confirmed_cases,deaths,recovered,
                                                         active,population))
# Copy labels back over, since select always takes them out:
us.county.state.info.numbers.nodate.df.confcaseprop <- sjlabelled::copy_labels(
  us.county.state.info.numbers.nodate.df.confcaseprop,us.county.state.info.numbers.nodate.df)

# Calc the model:
mdl.us.countydemogs.confcaseprop.nums <- 
  lm(confirmed_cases_prop ~ .,label_to_colnames(us.county.state.info.numbers.nodate.df.confcaseprop)) 
#   ^ used label_to_colnames to make the more-informative labels the names for this model
# save as a tibble:
mdl.tbbl.us.countydemogs.confcaseprop.nums <- tidy(mdl.us.countydemogs.confcaseprop.nums)


## Predict proportion of deaths from confirmed cases: ######

# calculate proportion of deaths:
us.county.state.info.numbers.nodate.df.deathsprop <- mutate(us.county.state.info.numbers.nodate.df,
                                                              deaths_prop = 
                                                                deaths/confirmed_cases)
# Create a df with just confirmed cases as outcome variable, only needed county info vars
us.county.state.info.numbers.nodate.df.deathsprop <- dplyr::select(
  us.county.state.info.numbers.nodate.df.deathsprop,-c(fips,area_name,state_code,county,
                                                         state,confirmed_cases,deaths,recovered,
                                                         active,population))
# Copy labels back over, since select always takes them out:
us.county.state.info.numbers.nodate.df.deathsprop <- sjlabelled::copy_labels(
  us.county.state.info.numbers.nodate.df.deathsprop,us.county.state.info.numbers.nodate.df)

# Calc the model:
mdl.us.countydemogs.deathsprop.nums <- 
  lm(deaths_prop ~ .,label_to_colnames(us.county.state.info.numbers.nodate.df.deathsprop)) 
#   ^ used label_to_colnames to make the more-informative labels the names for this model
# save as a tibble:
mdl.tbbl.us.countydemogs.deathsprop.nums <- tidy(mdl.us.countydemogs.deathsprop.nums)





## Predict proportion of deaths from confirmed cases, minus active: ######

# calculate proportion of deaths:
us.county.state.info.numbers.nodate.df.deathspropnoact <- mutate(us.county.state.info.numbers.nodate.df,
                                                            deaths_prop_noact = 
                                                              deaths/(confirmed_cases-active))
# Create a df with just confirmed cases as outcome variable, only needed county info vars
us.county.state.info.numbers.nodate.df.deathspropnoact <- dplyr::select(
  us.county.state.info.numbers.nodate.df.deathspropnoact,-c(fips,area_name,state_code,county,
                                                       state,confirmed_cases,deaths,recovered,
                                                       active,population))
# Copy labels back over, since select always takes them out:
us.county.state.info.numbers.nodate.df.deathspropnoact <- sjlabelled::copy_labels(
  us.county.state.info.numbers.nodate.df.deathspropnoact,us.county.state.info.numbers.nodate.df)

# Calc the model:
mdl.us.countydemogs.deathspropnoact.nums <- 
  lm(deaths_prop_noact ~ .,label_to_colnames(us.county.state.info.numbers.nodate.df.deathspropnoact)) 
#   ^ used label_to_colnames to make the more-informative labels the names for this model
# save as a tibble:
mdl.tbbl.us.countydemogs.deathspropnoact.nums <- tidy(mdl.us.countydemogs.deathspropnoact.nums)





