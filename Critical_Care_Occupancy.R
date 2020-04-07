# Critical care bed -  winter sitreps

library(easypackages)

libraries(c("readxl", "readr", "plyr", "dplyr", "ggplot2", "png", "tidyverse", "reshape2", "scales", "viridis", "rgdal", "officer", "flextable", "tmaptools", "lemon", "fingertipsR", "PHEindicatormethods", 'jsonlite', 'readODS', 'zoo', 'gridExtra'))

github_repo_dir <- "~/Documents/Repositories/critical_care_sitrep"

capwords = function(s, strict = FALSE) {
  cap = function(s) paste(toupper(substring(s, 1, 1)),
                          {s = substring(s, 2); if(strict) tolower(s) else s},sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))}

options(scipen = 999)

ph_theme = function(){
  theme( 
    plot.title = element_text(colour = "#000000", face = "bold", size = 10),    
    plot.subtitle = element_text(colour = "#000000", size = 10),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    panel.background = element_rect(fill = "#FFFFFF"), 
    panel.grid.major.y = element_line(colour = "#E7E7E7", size = .3),
    panel.grid.minor.y = element_blank(), 
    strip.text = element_text(colour = "#000000", size = 10, face = "bold"), 
    strip.background = element_blank(), 
    axis.ticks = element_line(colour = "#dbdbdb"), 
    legend.position = "bottom", 
    legend.title = element_text(colour = "#000000", size = 9, face = "bold"), 
    legend.background = element_rect(fill = "#ffffff"), 
    legend.key = element_rect(fill = "#ffffff", colour = "#ffffff"), 
    legend.text = element_text(colour = "#000000", size = 9), 
    axis.text.y = element_text(colour = "#000000", size = 8), 
    axis.text.x = element_text(colour = "#000000", angle = 0, hjust = 1, vjust = .5, size = 8), 
    axis.title =  element_text(colour = "#000000", size = 9, face = "bold"),   
    axis.line = element_line(colour = "#dbdbdb")
  ) 
}

if(!file.exists(paste0(github_repo_dir, '/etr.zip'))){
  download.file('https://files.digital.nhs.uk/assets/ods/current/etr.zip', paste0(github_repo_dir, '/etr.zip'), mode = 'wb')
  unzip(paste0(github_repo_dir, '/etr.zip'), exdir = github_repo_dir)
}

etr <- read_csv(paste0(github_repo_dir, '/etr.csv'),col_names = c('Code', 'Name', 'National_grouping', 'Health_geography', 'Address_1', 'Address_2', 'Address_3', 'Address_4', 'Address_5', 'Postcode', 'Open_date', 'Close_date', 'Null_1', 'Null_2', 'Null_3', 'Null_4', 'Null_5', 'Contact', 'Null_6', 'Null_7', 'Null_8', 'Amended_record_indicator', 'Null_9', 'GOR', 'Null_10', 'Null_11', 'Null_12')) %>%
  select(Code, Name, National_grouping) %>% 
  mutate(Name = capwords(Name, strict = TRUE)) %>% 
  mutate(Name = gsub(' And ', ' and ', Name)) %>% 
  mutate(Name = gsub(' Of ', ' of ', Name)) %>% 
  mutate(Name = gsub(' Nhs ', ' NHS ', Name)) %>% 
  add_row( Code = '-', Name = 'England', National_grouping = '-')

# Hospital provider trusts do not have geographically defined boundaries for their population nor do they have complete lists of registered patients. However, modelled estimates of the catchment populations for hospital provider trusts in England are provided by Public Health England (PHE). These experimental statistics estimates the number of people who are using each hospital trust or have the potential to do so. Individual acute trusts sometimes use varying methods to define the population they serve, such as patient flow, CCG derived or travel time based estimates. PHE published modelled estimates use the patient flow method.

catchment_pop <- read_excel(paste0(github_repo_dir, "/2020 Trust Catchment Populations Worksheet.xlsx"), sheet = "Trust Analysis", col_types = c("text", "text", "text", "text", "text", "numeric", "text", "text", "numeric", "numeric", "numeric", "numeric","numeric")) %>% 
  group_by(CatchmentYear, TrustCode, TrustName, AdmissionType) %>% 
  summarise(Catchment = sum(Catchment, na.rm = TRUE)) %>% 
  filter(CatchmentYear == 2018) %>% 
  filter(AdmissionType == 'Emergency') %>% 
  rename(Emergency_catchment_pop_2018 = Catchment) %>% 
  rename(Code = TrustCode) %>% 
  ungroup() %>% 
  select(Code, Emergency_catchment_pop_2018)

catchment_pop %>% 
  write.csv(., paste0(github_repo_dir, '/trust_catchment_population_estimates.csv'), row.names = FALSE)

filepaths_1 = c('https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/03/Monthly-SITREPSs-CC-and-UOC-Extracts-JANUARY-2020-oa9U1.csv', 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/02/Monthly-SITREPSs-CC-and-UOC-Extracts-DECEMBER-2019.csv', 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/01/Monthly-SITREPSs-CC-and-UOC-Extracts-November-2019-d63j8.csv', 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/12/Monthly-SITREPSs-CC-and-UOC-Extracts-October-2019-3jd8y.csv', 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/11/Monthly-SITREPSs-CC-and-UOC-Extracts-September-2019-J4d35.csv', 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/10/Monthly-SITREPs-CC-and-UOC-Extracts-August-2019-L4h2U.csv', 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/09/Monthly-SITREPs-CC-and-UOC-Extracts-July-2019_yuGz0j.csv', 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/08/Monthly-SITREPs-CC-and-UOC-Extracts-June-2019_yPGz0j.csv', 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/07/Monthly-SITREPs-CC-and-UOC-Extracts-May-2019_K4nMsW.csv', 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/12/Monthly-SITREPs-CC-and-UOC-Extracts-April-2019-CSV-2nb90.csv', 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/06/MSitRep-MARCH-2019.csv', 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/04/Monthly-SITREPs-CC-and-UOC-Extracts-Feb-2019-CSV-2lTm7.csv', 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/06/MSitRep-JANUARY-2019.csv', 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/02/Monthly-SITREPs-CC-and-UOC-Extracts-Dec-2018-CSV-2dSa5.csv', 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/01/Monthly-SITREPs-CC-and-UOC-Extracts-Nov-2018-CSV-dsaW2.csv', 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/06/MSitRep-OCTOBER-2018.csv', 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2018/11/Monthly-SITREPs-CC-and-UOC-Extracts-Sep-2018-CSV-USf2f2.csv', 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2018/11/MSitRep-AUGUST-2018-revisions.csv','https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2018/11/MSitRep-JULY-2018-revisions.csv','https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2018/11/MSitRep-June-2018-revisions.csv','https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2018/11/MSitRep-May-2018-revisions.csv','https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2018/11/MSitRep-April-2018-revisions-1.csv','https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2018/11/MSitRep-March-2018-revisions.csv','https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2018/11/MSitRep-February-2018-revisions.csv', 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2018/11/MSitRep-January-2018-revisions.csv')

Region <- filepaths_1 %>% 
  map_df(~read_csv(.)) %>% 
  select(`Region Code`, `Region name`) %>% 
  filter(`Region Code` %in% etr$National_grouping) %>% 
  unique() %>% 
  mutate(`Region name` = capwords(`Region name`, strict = TRUE)) %>% 
  mutate(`Region name` = gsub(' And ', ' and ', `Region name`)) %>% 
  mutate(`Region name` = gsub(' Of ', ' of ', `Region name`))

Monthly_cc_sitrep_1 <- filepaths_1 %>% 
  map_df(~read_csv(.)) %>% 
  mutate(Period = paste0('01-', capwords(gsub('MSitRep-', '' , Period), strict = TRUE))) %>% 
  mutate(Date = as.Date(Period, format = '%d-%B-%Y')) %>% 
  mutate(`Org Code` = ifelse(`Org Code` == 'Totals', '-', `Org Code`)) %>% 
  left_join(etr, by = c('Org Code' = 'Code')) %>% 
  select(-c(`Org name`, `Region Code`, `Region name`)) %>% 
  rename(Region_code = National_grouping) %>% 
  rename(Code = 'Org Code') %>% 
  left_join(Region, by = c('Region_code' = 'Region Code')) %>% 
  rename(Region_name = `Region name`) %>% 
  mutate(Period = gsub('01-', '', Period)) %>% 
  # select(Code, Name, Region_code, Region_name, Date, Period, `Number of adult critical care beds open`, `Number of adult critical care beds occupied`, `Number of paediatric intensive care beds open`,`Number of paediatric intensive care beds occupied`,`Number of neonatal critical care cots (or beds) open`,`Number of neonatal critical care cots (or beds) occupied`) %>% 
  select(Code, Name, Region_code, Region_name, Date, Period, `Number of adult critical care beds open`, `Number of adult critical care beds occupied`) %>% 
  rename(adult_beds_open = `Number of adult critical care beds open`,
         adult_beds_occupied = `Number of adult critical care beds occupied`) 

filepaths_2 <- c('https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2018/04/MSitReps-December-CC-Revised-27.04.18.csv', 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2018/04/MSitReps-November-CC-Revised-27.04.18.csv', 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2018/04/MSitReps-October-CC-Revised-27.04.18.csv','https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2018/04/MSitReps-September-CC-Revised-27.04.18.csv','https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2018/04/MSitReps-August-CC-Revised-27.04.18.csv','https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2018/04/MSitReps-July-CC-Revised-27.04.18.csv','https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2018/04/MSitReps-June-CC-Revised-27.04.18.csv','https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2018/04/MSitReps-May-CC-Revised-27.04.18.csv','https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2018/04/MSitReps-April-CC-Revised-27.04.18.csv', 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2016/05/Monthly-SITREPs-CC-Extracts-March-2017-Revised-27.10.2017-s65Bu.csv','https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2016/05/Monthly-SITREPs-CC-Extracts-February-2017-Revised-27.10.2017-8v6Hy.csv', 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2016/05/Monthly-SITREPs-CC-Extracts-January-2017-Revised-27.10.2017-Un45M.csv', 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2016/05/Monthly-SITREPs-CC-Extracts-December-2016-n09YJ.csv','https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2016/05/Monthly-SITREPs-CC-Extracts-November-2016-2B447.csv',  'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2016/05/Monthly-SITREPs-CC-Extracts-October-2016-JkigS.csv','https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2016/05/Monthly-SITREPs-CC-Extracts-September-2016-3gThp.csv','https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2016/05/Monthly-SITREPs-CC-Extracts-August-2016-76Sk0.csv','https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2016/05/Monthly-SITREPs-CC-Extracts-July-2016-Revised-SqGaU.csv','https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2016/05/Monthly-SITREPs-CC-Extracts-June-2016-Revised-g02SK.csv','https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2016/05/Monthly-SITREPs-CC-Extracts-May-2016-Revised-a3i7N.csv','https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2016/05/Monthly-SITREPs-CC-Extracts-April-2016-Revised-lxIXm.csv','https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2015/05/Monthly-SITREPs-CC-Extracts-fdjs.csv','https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2015/05/Monthly-SITREPs-CC-Extracts-February-2016-Revised-nVt3S.csv','https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2015/05/Monthly-SITREPs-CC-Extracts-jgsdd1.csv','https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2015/05/Monthly-SITREPs-CC-Extracts-jgsdd.csv','https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2015/05/Nov-csv-CC-fevcm.csv','https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2015/05/Oct-csv-cc-fevcm.csv','https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2015/05/Sept-csv-cc-fevcm.csv','https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2015/05/Monthly-SITREPs-CC-Extracts-fjkiu.csv','https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2015/05/Monthly-SITREPs-CC-Extracts-djery.csv','https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2015/05/Monthly-SITREPs-CC-Extracts.csv','https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2015/05/Monthly-SITREPS-CC-Extracts-May.csv','https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2015/05/Monthly-SITREPS-CC-Extracts-April.csv')

Monthly_cc_sitrep_2 = data.frame(Code = character(), adult_beds_open = numeric(), adult_beds_occupied = numeric(), `Number of paediatric intensive care beds open` = numeric(), `Number of paediatric intensive care beds occupied` = numeric(), `Number of neonatal critical care cots (or beds) open` = numeric(), `Number of neonatal critical care cots (or beds) occupied` = numeric(), Period = character(), check.names = FALSE)      

for(i in 1:length(filepaths_2)){
  Period_x <- read_csv(filepaths_2[i], skip = 1, n_max = 1, col_names = c('Year', 'Period', 'SHA', 'Org')) %>% 
    mutate(Year = gsub('Year:', '', Year)) %>% 
    mutate(Year = paste0('20', ifelse(Period %in% c('Period Name:JANUARY','Period Name:FEBRUARY','Period Name:MARCH'), substr(Year, 6,7), substr(Year, 3,4)))) %>% 
    mutate(Period = paste0(capwords(gsub('Period Name:', '', Period), strict = TRUE), '-', Year)) %>% 
    select(Period)
  
  Monthly_cc_sitrep_x <- read_csv(filepaths_2[i], skip = 6, col_names = c('Code', 'adult_beds_open', 'adult_beds_occupied', 'Number of paediatric intensive care beds open', 'Number of paediatric intensive care beds occupied', 'Number of neonatal critical care cots (or beds) open', 'Number of neonatal critical care cots (or beds) occupied')) %>% 
    mutate(Period = Period_x$Period)
  
  Monthly_cc_sitrep_2 = Monthly_cc_sitrep_2 %>% 
    bind_rows(Monthly_cc_sitrep_x)
}
 

March_15 <- read_csv('https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2014/05/Monthly-SITREPS-CC-Extracts-March.csv', skip = 6, col_names = c('Code', 'adult_beds_open', 'adult_beds_occupied', 'Number of paediatric intensive care beds open', 'Number of paediatric intensive care beds occupied', 'Number of neonatal critical care cots (or beds) open', 'Number of neonatal critical care cots (or beds) occupied')) %>% 
  mutate(Period = 'March-2015')

Feb_15 <- read_csv('https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2014/05/Monthly-SITREPS-CC-Extracts-February.csv', skip = 6, col_names = c('Code', 'adult_beds_open', 'adult_beds_occupied', 'Number of paediatric intensive care beds open', 'Number of paediatric intensive care beds occupied', 'Number of neonatal critical care cots (or beds) open', 'Number of neonatal critical care cots (or beds) occupied')) %>% 
  mutate(Period = 'February-2015')

Jan_15 <- read_csv('https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2014/05/Monthly-SITREPs-CC-Extracts9g5ud.csv', skip = 6, col_names = c('Code', 'adult_beds_open', 'adult_beds_occupied', 'Number of paediatric intensive care beds open', 'Number of paediatric intensive care beds occupied', 'Number of neonatal critical care cots (or beds) open', 'Number of neonatal critical care cots (or beds) occupied')) %>% 
  mutate(Period = 'January-2015')

Monthly_cc_sitrep_2 <- Monthly_cc_sitrep_2 %>% 
  bind_rows(March_15) %>% 
  bind_rows(Feb_15) %>% 
  bind_rows(Jan_15) %>% 
  mutate(Code = ifelse(Code == 'ENGLAND', '-', Code)) %>% 
  left_join(etr, by = 'Code') %>% 
  rename(Region_code = National_grouping) %>% 
  left_join(Region, by = c('Region_code' = 'Region Code')) %>% 
  rename(Region_name = `Region name`) %>% 
  mutate(Period = paste0('01-', Period)) %>% 
  mutate(Date = as.Date(Period, format = '%d-%B-%Y')) %>% 
  mutate(Period = gsub('01-', '', Period)) %>% 
  # select(Code, Name, Region_code, Region_name, Date, Period, `Number of adult critical care beds open`, `Number of adult critical care beds occupied`, `Number of paediatric intensive care beds open`,`Number of paediatric intensive care beds occupied`,`Number of neonatal critical care cots (or beds) open`,`Number of neonatal critical care cots (or beds) occupied`) %>% 
  select(Code, Name, Region_code, Region_name, Date, Period, adult_beds_open, adult_beds_occupied)

# Not all trusts have names in the etr (possible due to mergers/closures)
#setdiff(Monthly_cc_sitrep_2$Code, etr$Code)

# But it is only the old files (pre 2017)
Monthly_cc_sitrep <- Monthly_cc_sitrep_1 %>%
  bind_rows(Monthly_cc_sitrep_2) %>% 
  mutate(adult_beds_available = adult_beds_open - adult_beds_occupied) %>% 
  mutate(Percentage_adult_beds_available = ifelse(adult_beds_open == 0, NA, adult_beds_available / adult_beds_open)) %>% 
  mutate(Percentage_adult_cc_beds_occupied = ifelse(adult_beds_open == 0, NA, adult_beds_occupied / adult_beds_open))

rm(Monthly_cc_sitrep_1, Monthly_cc_sitrep_2, Monthly_cc_sitrep_x, Period_x, filepaths_1, filepaths_2, March_15, Feb_15, Jan_15)

# Not all trusts have names in the etr (possible due to mergers/closures)
#setdiff(Monthly_cc_sitrep$Code, etr$Code)

last_thursdays <- data.frame(DATE = seq(min(Monthly_cc_sitrep$Date), as.Date('2020-03-01'), "day")) %>% 
  mutate(month = months(DATE), 
         year = format(DATE, '%Y'),
         weekday = weekdays(DATE)) %>%
  group_by(month, year) %>%
  filter(weekday == 'Thursday') %>%
  filter(DATE == max(DATE)) %>% 
  ungroup() %>% 
  mutate(Period = paste0(month, '-', year)) %>% 
  rename(last_thursday = DATE) %>% 
  select(Period, last_thursday)

# closed organisations have been removed from the dataset
Monthly_cc_sitrep<- Monthly_cc_sitrep %>% 
  mutate(Name = ifelse(Code == 'ENGLAND', 'England', Name)) %>% 
  mutate(Code = ifelse(Code == 'ENGLAND', '-', Code)) %>% 
  filter(!is.na(Name)) %>% 
  left_join(last_thursdays, by = 'Period') %>% 
  left_join(catchment_pop, by = 'Code') %>% 
  mutate(cc_beds_per_100000 = (adult_beds_open / Emergency_catchment_pop_2018) * 100000) %>% 
  mutate(occupancy_above_85 = ifelse(Percentage_adult_cc_beds_occupied >= .85, '>= 85%', 'below 85%'))

Monthly_cc_sitrep %>% 
  write.csv(., paste0(github_repo_dir, '/Monthly_adult_cc_sitrep.csv'), row.names = FALSE)

Providers_dec <- Monthly_cc_sitrep %>% 
  filter(Period == 'December-2019') %>% 
  select(Code, Name,  Region_code, Region_name) %>% 
  unique() 

Providers <- Monthly_cc_sitrep %>% 
  select(Code, Name,  Region_code, Region_name) %>% 
  unique() 

# What does normal look like? ####

# write a report detailing key metrics

Eng_monthly_sitrep <- Monthly_cc_sitrep %>%
  filter(Name == 'England')

se_sitrep <- Monthly_cc_sitrep %>%
  filter(Region_name == 'South East Commissioning Region') %>%
  arrange(Name, Date) %>%
  group_by(Date, Period, last_thursday) %>%
  summarise(adult_beds_open = sum(adult_beds_open, na.rm = TRUE),
            adult_beds_occupied = sum(adult_beds_occupied, na.rm = TRUE),
            adult_beds_available = sum(adult_beds_available, na.rm = TRUE)) %>%
  mutate(Percentage_adult_cc_beds_occupied = adult_beds_occupied / adult_beds_open) %>%
  mutate(Name = 'South East Commissioning Region',
         Code = '-')

n_se_providers <- Providers %>% 
  filter(Region_code == 'Y59') %>% 
  unique() %>% 
  nrow()

stp_sitrep <- Monthly_cc_sitrep %>%
  filter(Name %in% c('Brighton and Sussex University Hospitals NHS Trust', 'East Sussex Healthcare NHS Trust', 'Queen Victoria Hospital NHS Foundation Trust', 'Surrey and Sussex Healthcare NHS Trust', 'Western Sussex Hospitals NHS Foundation Trust')) %>%
  group_by(Date, Period, last_thursday) %>%
  summarise(adult_beds_open = sum(adult_beds_open, na.rm = TRUE),
            adult_beds_occupied = sum(adult_beds_occupied, na.rm = TRUE),
            adult_beds_available = sum(adult_beds_available, na.rm = TRUE)) %>%
  mutate(Percentage_adult_cc_beds_occupied = adult_beds_occupied / adult_beds_open) %>%
  mutate(Name = 'Sussex and East Surrey STP acute trusts',
         Code = '-')

sitrep_beds <- Eng_monthly_sitrep %>% 
  bind_rows(se_sitrep) %>% 
  bind_rows(stp_sitrep) %>% 
  arrange(Date) %>% 
  mutate(Period = factor(Period, levels = unique(Period))) %>% 
  mutate(last_thursday = as.character(format(last_thursday, '%d/%m/%Y'))) %>% 
  mutate(Percentage_adult_cc_beds_available = adult_beds_available / adult_beds_open) %>% 
  mutate(Percentage_adult_cc_beds_occupied = adult_beds_occupied / adult_beds_open) %>% 
  select(Code, Name, Date, Period, last_thursday, adult_beds_open, adult_beds_available, adult_beds_occupied, Percentage_adult_cc_beds_available, Percentage_adult_cc_beds_occupied) %>% 
  as.data.frame() %>% 
  mutate(occupancy_above_85 = ifelse(Percentage_adult_cc_beds_occupied >= .85, '>= 85%', 'below 85%'))

latest_sitrep_beds <- sitrep_beds %>% 
  filter(Date == max(Date))

jan_19_sitrep_beds <- sitrep_beds %>% 
  filter(Period == 'January-2019')

paste0('The latest monthly data for critical care bed occupancy is for ', unique(latest_sitrep_beds$Period), ' and this indicates that nationally as at midnight on the ', unique(latest_sitrep_beds$last_thursday), ' there were ', format(subset(latest_sitrep_beds, Name == 'England', select = 'adult_beds_open'), big.mark = ','), ' open critical care beds (adult beds). Of these, ', format(subset(latest_sitrep_beds, Name == 'England', select = 'adult_beds_occupied'), big.mark = ','), ' were occupied (', round(subset(latest_sitrep_beds, Name == 'England', select = 'Percentage_adult_cc_beds_occupied') * 100, 1), '%). In January 2019, the occupancy rate was ', round(subset(jan_19_sitrep_beds, Name == 'England', select = 'Percentage_adult_cc_beds_occupied') * 100, 1), '% with ', format(subset(jan_19_sitrep_beds, Name == 'England', select = 'adult_beds_occupied'), big.mark = ','), ' beds out of ', format(subset(jan_19_sitrep_beds, Name == 'England', select = 'adult_beds_open'), big.mark = ','),' occupied.')

paste0('Across England at midnight on ', unique(latest_sitrep_beds$last_thursday), ' there were ', format(subset(latest_sitrep_beds, Name == 'England', select = 'adult_beds_available'), big.mark = ','), ' beds available for new patients.')

paste0('There are ', n_se_providers, ' NHS organisations providing critical care beds in the South East Commissioning Region. Across the commissioning region, at midnight on ', unique(latest_sitrep_beds$last_thursday), ' there were ', format(subset(latest_sitrep_beds, Name == 'South East Commissioning Region', select = 'adult_beds_open'), big.mark = ','), ' open critical care beds (adult beds) with ', format(subset(latest_sitrep_beds, Name == 'South East Commissioning Region', select = 'adult_beds_occupied'), big.mark = ','), ' occupied (', round(subset(latest_sitrep_beds, Name == 'South East Commissioning Region', select = 'Percentage_adult_cc_beds_occupied') * 100, 1), '%). This occupancy rate is lower than in England overall in the latest available data.')

paste0('Across the South East Commissioning Region at midnight on ', unique(latest_sitrep_beds$last_thursday), ' there were ', format(subset(latest_sitrep_beds, Name == 'South East Commissioning Region', select = 'adult_beds_available'), big.mark = ','), ' beds available for new patients.')

paste0('More locally, the Sussex and East Surrey Sustainability and Transformation Partnership (STP) comprises five acute hospital trusts which provider critical care beds (Brighton and Sussex University Hospitals NHS Trust, East Sussex Healthcare NHS Trust, Queen Victoria Hospital NHS Foundation Trust, Surrey and Sussex Healthcare NHS Trust, and Western Sussex Hospitals NHS Foundation Trust). The January 2020 data indicates that across the STP, hospital trusts have ', format(subset(latest_sitrep_beds, Name == 'Sussex and East Surrey STP acute trusts', select = 'adult_beds_open'), big.mark = ','), ' open critical care beds (adult beds) with ', format(subset(latest_sitrep_beds, Name == 'Sussex and East Surrey STP acute trusts', select = 'adult_beds_occupied'), big.mark = ','), ' occupied (', round(subset(latest_sitrep_beds, Name == 'Sussex and East Surrey STP acute trusts', select = 'Percentage_adult_cc_beds_occupied') * 100, 1), '%). This occupancy rate is marginally lower than in England overall in the latest available data and higher than in the South East Commissioning region.')

paste0('Across the Sussex and East Surrey STP at midnight on ', unique(latest_sitrep_beds$last_thursday), ' there were ', format(subset(latest_sitrep_beds, Name == 'Sussex and East Surrey STP acute trusts', select = 'adult_beds_available'), big.mark = ','), ' beds available for new patients.')

paste0('A critical care capacity research study conducted in 2018 by The Faculty of Intensive Care Medicine reports that the highest rate recommended for safe and efficient patient care is 85%. see - https://www.ficm.ac.uk/sites/default/files/ficm_critical_capacity_-_a_short_research_survey_on_critical_care_bed_capacity.pdf')

paste0('Operating above this capacity means providers can struggle to cope with variation, respond to crises, and this leads to cancelling operations, or needing to transfer very ill patients to other hospitals.')

paste0('The Guidelines for the Provision of Intensive Care Services (GPICS) in 2019 recommends a ratio of one critical care bed per 35 acute hospital beds.')

sitrep_beds_1 <- sitrep_beds %>% 
  rename(Area = Name)

ggplot(sitrep_beds, aes(x = Period, y = Percentage_adult_cc_beds_occupied)) +
  geom_line(data = sitrep_beds_1, aes(x = Period, y = Percentage_adult_cc_beds_occupied, group = Area), colour = '#c5c5c5') +
  geom_line(aes(group = Name),
            colour = '#000000') +
  geom_point(aes(fill = occupancy_above_85),
             size = 2,
             colour = '#ffffff',
             shape = 21) +
  scale_fill_manual(values = c('#660066', '#FA6900'),
                    name = 'Occupancy (%)') +
  scale_y_continuous(limits = c(0.6,1),
                     breaks = seq(0,1,.1),
                     labels = percent_format(accuracy = 1),
                     expand = c(0,0.01)) +
  labs(title = 'Critical care beds (adults) occupied as a proportion of open critical care beds',
       caption = 'Note: y axis does not start at zero.\nThe dashed line represents the 85% occupancy recommended limit for safe and efficient patient care.',
       x = 'Last Thursday of each month',
       y = 'Proportion of critical care beds occupied') +
  geom_hline(aes(yintercept = .85),
             colour = "#3d2b65",
             linetype="dashed",
             lwd = .5) +
  facet_rep_grid(Name ~., repeat.tick.labels = FALSE) +
  ph_theme() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(strip.text = element_blank()) +
  annotate(geom = "text", label = c('England','South East Commissioning Region','Sussex and East Surrey STP'), x = 1, y = .97, size = 3, fontface = "bold", hjust = 0)

sitrep_beds %>% 
  filter(Date >= '2019-02-01') %>% 
  group_by(Name, occupancy_above_85) %>% 
  # summarise(n()) %>% 
  filter(occupancy_above_85 == '>= 85%')

paste0('In the past 12 months (February 2019 - January 2020), occupancy of critical care beds did not exceed 85% in England overall or in South East Commissioning Region. However, in Sussex and East Surrey STP, the occupancy exceeded the safe level once, in May 2019 when occupancy reached 91.2% (103 out of 113 beds).')

# Occupancy over time, peaks and average occupancy

sitrep_beds %>% 
  select(Name, Period, Percentage_adult_cc_beds_occupied) %>% 
  spread(Period, Percentage_adult_cc_beds_occupied) %>% 
  select(Name, `January-2019`, `January-2020`)

sitrep_beds %>% 
  select(Name, Period, adult_beds_occupied) %>% 
  spread(Period, adult_beds_occupied) %>% 
  select(Name, `January-2019`, `January-2020`)

sitrep_beds %>% 
  select(Name, Period, adult_beds_open) %>% 
  spread(Period, adult_beds_open) %>% 
  select(Name, `January-2019`, `January-2020`)

sitrep_beds %>% 
  select(Name, Period, adult_beds_open) %>% 
  group_by(Name) %>% 
  filter(adult_beds_open == max(adult_beds_open))

sitrep_beds %>% 
  select(Name, Period, adult_beds_available) %>% 
  group_by(Name) %>% 
  filter(adult_beds_available == max(adult_beds_available))

sitrep_beds %>% 
  select(Name, Date, adult_beds_available) %>% 
  # filter(Date > '2019-01-01') %>%
  group_by(Name) %>% 
  summarise(available = mean(adult_beds_available))

# They have forecast the percentage of COVID-19 bed requirements in isolation. In reality, all ICUs will need to continue to provide ‘business as usual’ care for other types of patients. Since UK bed occupancy is typically greater than 80% and may frequently exceed 100%, the researchers say it is clearly not the case that all open beds can simply be re-allocated for COVID-19 patients.

# They have also assumed that all adult critical care beds can be used for level 3 or mechanically ventilated ICU patients, which may not be possible; some specialist ICUs may not be able to reconfigure at all.

# sitrep_beds <- sitrep_beds %>% 
#   mutate()

# Trust variation ####

trust_sitrep <- Monthly_cc_sitrep %>%
  filter(Name %in% c('Brighton and Sussex University Hospitals NHS Trust', 'East Sussex Healthcare NHS Trust', 'Queen Victoria Hospital NHS Foundation Trust', 'Surrey and Sussex Healthcare NHS Trust', 'Western Sussex Hospitals NHS Foundation Trust')) %>% 
  arrange(Name, Date) %>% 
  mutate(Period = factor(Period, levels = unique(Period))) %>% 
  arrange(Period) %>% 
  select(Code, Name, Period, adult_beds_occupied, adult_beds_available, adult_beds_open) %>% 
  gather(key = "Status", value = "Beds", c(adult_beds_occupied, adult_beds_available))

ggplot(trust_sitrep, aes(x = Period, y = Beds, group = Status, fill = Status)) +
  geom_bar(stat = "identity", width = 0.8) +
  scale_fill_manual(values = c('#f9ac26', '#DBDBDB'),
                    name = 'Bed status',
                    labels = c('Available', 'Occupied')) +
  scale_y_continuous(breaks = seq(0,120, 20)) +
  ph_theme() +
  labs(title = 'Critical care beds (adults); Sussex and East Surrey STP Trusts; January 2015-January 2020;',
       x = 'Last Thursday of each month',
       y = 'Number of critical care beds occupied') +
  theme(axis.text.x = element_text(angle = 90))

Jan_2020_ses_stp <- trust_sitrep %>% 
  filter(Period == 'January-2020') %>% 
  arrange(adult_beds_open) %>% 
  mutate(Name = ifelse(Name == 'Brighton and Sussex University Hospitals NHS Trust', 'Brighton and Sussex\nUniversity Hospitals\nNHS Trust', ifelse(Name == 'Western Sussex Hospitals NHS Foundation Trust', 'Western Sussex\nHospitals NHS\nFoundation Trust', ifelse(Name == 'East Sussex Healthcare NHS Trust', 'East Sussex\nHealthcare\nNHS Trust', ifelse(Name == 'Surrey and Sussex Healthcare NHS Trust','Surrey and Sussex\nHealthcare\nNHS Trust', ifelse(Name == 'Queen Victoria Hospital NHS Foundation Trust', 'Queen Victoria\nHospital NHS\nFoundation Trust', Name)))))) %>% 
  mutate(Name = factor(Name, levels = unique(Name))) %>% 
  mutate(Percentage_beds = Beds / adult_beds_open)

n_beds_occupied_jan_trust <- ggplot(Jan_2020_ses_stp, aes(x = Name, y = Beds, group = Status, fill = Status)) +
  geom_bar(stat = "identity", width = 0.8) +
  scale_fill_manual(values = c('#f9ac26', '#DBDBDB'),
                    name = 'Bed status',
                    labels = c('Available', 'Occupied')) +
  coord_flip() +
  labs(x = NULL,
       y = 'Beds') +
  ph_theme() 

p_beds_occupied_jab_trust <- ggplot(Jan_2020_ses_stp, aes(x = Name, y = Percentage_beds, group = Status, fill = Status)) +
  geom_bar(stat = "identity", width = 0.8) +
  scale_fill_manual(values = c('#f9ac26', '#DBDBDB'),
                    name = 'Bed status',
                    labels = c('Available', 'Occupied'),
                    expand = c(0,0.01)) +
  labs(x = NULL,
       y = 'Percentage') +
  scale_y_continuous(breaks = seq(0,1,.1),
                     labels = percent) +
  coord_flip() +
  ph_theme() +
  theme(axis.text.y = element_blank())

grid.arrange(n_beds_occupied_jan_trust, p_beds_occupied_jab_trust, ncol=2, top = 'Critical care beds by bed status; Sussex and East Surrey STP; January 2020; Number of beds and percentage', widths = c(2.1,1.8))

trust_sitrep <- Monthly_cc_sitrep %>%
  filter(Name %in% c('Brighton and Sussex University Hospitals NHS Trust', 'East Sussex Healthcare NHS Trust', 'Queen Victoria Hospital NHS Foundation Trust', 'Surrey and Sussex Healthcare NHS Trust', 'Western Sussex Hospitals NHS Foundation Trust')) %>% 
  arrange(Name, Date) %>% 
  mutate(Period = factor(Period, levels = unique(Period))) %>% 
  arrange(Period) 

trust_sitrep_1 <- trust_sitrep %>% 
  rename(Area = Name)

ggplot(trust_sitrep, aes(x = Period, y = Percentage_adult_cc_beds_occupied)) +
  geom_line(data = trust_sitrep_1, aes(x = Period, y = Percentage_adult_cc_beds_occupied, group = Area), colour = '#c5c5c5') +
  geom_line(aes(group = Name),
            colour = '#000000') +
  geom_point(aes(fill = occupancy_above_85),
             size = 2,
             colour = '#ffffff',
             shape = 21) +
  scale_fill_manual(values = c('#660066', '#FA6900'),
                    name = 'Occupancy (%)') +
  scale_y_continuous(limits = c(0,1.2),
                     breaks = seq(0,1,.2),
                     labels = percent_format(accuracy = 1),
                     expand = c(0,0.01)) +
  labs(title = 'Critical care beds (adults) occupied as a proportion of open critical care beds; January 2015-January 2020;',
       x = 'Last Thursday of each month',
       y = 'Proportion of critical care beds occupied') +
  geom_hline(aes(yintercept = .85),
             colour = "#3d2b65",
             linetype="dashed",
             lwd = .5) +
  facet_rep_grid(Name ~., repeat.tick.labels = FALSE) +
  ph_theme() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(strip.text = element_blank()) +
  annotate(geom = "text", label = c('Brighton and Sussex University Hospitals NHS Trust', 'East Sussex Healthcare NHS Trust', 'Queen Victoria Hospital NHS Foundation Trust', 'Surrey and Sussex Healthcare NHS Trust', 'Western Sussex Hospitals NHS Foundation Trust'), x = 1, y = 1.1, size = 3, fontface = "bold", hjust = 0)

# Occupancy over time, peaks and average occupancy

trust_sitrep %>% 
  select(Name, Period, Percentage_adult_cc_beds_occupied) %>% 
  spread(Period, Percentage_adult_cc_beds_occupied) %>% 
  select(Name, `January-2015`,`January-2019`, `January-2020`)

trust_sitrep %>% 
  select(Name, Period, adult_beds_occupied) %>% 
  spread(Period, adult_beds_occupied) %>% 
  select(Name, `January-2019`, `January-2020`)

trust_sitrep %>% 
  select(Name, Period, adult_beds_available) %>% 
  spread(Period, adult_beds_available) %>% 
  select(Name, `January-2019`, `January-2020`)

trust_sitrep %>% 
  select(Name, Period, adult_beds_open) %>% 
  spread(Period, adult_beds_open) %>% 
  select(Name, `January-2019`, `January-2020`)

trust_sitrep %>% 
  select(Name, Date, occupancy_above_85, Percentage_adult_cc_beds_occupied) %>% 
  filter(Date > '2019-01-01') %>%
  filter(occupancy_above_85 == '>= 85%') %>% 
  arrange(Name) 
  
trust_sitrep %>% 
  select(Name, Date, Percentage_adult_cc_beds_occupied) %>% 
  filter(Date > '2019-01-01') %>%
  group_by(Name) %>% 
  summarise(Percentage_adult_cc_beds_occupied = mean(Percentage_adult_cc_beds_occupied))

trust_sitrep %>% 
  select(Name, Period, Percentage_adult_cc_beds_occupied) %>% 
  group_by(Name) %>% 
  filter(Percentage_adult_cc_beds_occupied == max(Percentage_adult_cc_beds_occupied))

trust_sitrep %>% 
  select(Name, adult_beds_open) %>% 
  group_by(Name, adult_beds_open) %>% 
  summarise(n()) %>% 
  View()

trust_sitrep %>% 
  select(Name, Period, adult_beds_available) %>% 
  group_by(Name) %>% 
  filter(adult_beds_available == max(adult_beds_available))

trust_sitrep %>% 
  select(Name, Date, adult_beds_available) %>% 
  # filter(Date > '2019-01-01') %>%
  group_by(Name) %>% 
  summarise(available = mean(adult_beds_available))

# MSitRep data are published to a monthly timetable with data being published around 6 to 7 weeks after the end of the reference period

# Organisation names and codes change all the time, and nhs regions change too.    
  
# Critical care capacity, including adult, paediatric and neonatal available and occupied critical care beds, as a snapshot at midnight on the last Thursday of the month.

# Count all adult critical care (ITU, HDU or other) beds that are funded and available for critical care patients (Levels 2 and 3) at midnight on the last Thursday of the reporting period. Note that this should be the actual number of beds at that time and not the planned number of beds. Beds funded but not available due to staff vacancies should not be counted unless the vacancies have been filled by bank or agency staff. Beds that are not funded, but are occupied should be counted.

# This count should be consistent with that provided for the KH03a return. The definitions of critical care levels are:
  # Level 1 – Patients at risk of their condition deteriorating or those recently relocated from higher levels of care, whose needs can be met on an acute ward with additional advice and support from the critical care team. (NB These patients are NOT included in SitRep returns).

  # Level 2 – Patients requiring more detailed observation or intervention including support for a single failing organ system or post operative care and those “stepping down” from higher levels of care. Also known as High Dependency.

  # Level 3 – Patients requiring advanced respiratory support alone or basic respiratory support together with support of at least two organ systems. This level includes all complex patients requiring support for multi-organ failure. Also known as Intensive Care.

# Paediatric intensive care at level 3, also known as paediatric advanced critical care (provided by Trusts commissioned to deliver this care). In order to provide the appropriate level of care for paediatric intensive care (level 3), a minimum nurse to patient ratio of 1:1 is required. There are 20 Trusts who are commissioned to provide Paediatric Intensive care (Level 3) across England (equating to 22 units) This collection aims to look at capacity levels. Therefore, all open level 3 beds should be counted and figures for occupied beds should include all patients in level 3 beds (regardless of patient characteristics and the nature of care they receive). PICANet 2018 Annual Report and Paediatric Intensive Care (PICS) Nurse Workforce Planning for level 3 Paediatric Critical Care Units Paediatric Intensive Care Surge Standard Operating Procedure

# F3i) The total number of neonatal intensive care cots (or beds) open at midnight on the last Thursday of the reporting period
# F3ii) The total number of occupied neonatal intensive care cots (or beds) at midnight on the last Thursday of the reporting period
# Defined as:
  # High dependency care
# Higher levels of clinical care including babies recovering from intensive care. This includes babies receiving oxygen for immature lungs as they breathe on their own, sometimes assisted by higher pressure given via nasal prongs; babies on intravenous nutrition or treated with chest drains or for convulsions, infections or metabolic problems.
# Neonatal Critical Care
# Babies born prematurely, simply to support organ systems until they have matured; and babies who are ill or who have congenital disorders. This includes support in breathing (often with mechanical ventilation), to protect from infection and to achieve growth equivalent to that which occurs in the womb. Even “well” very premature babies require intensive care simply to support their life until their organ systems undergo maturity. Short term intensive care may also be provided for less immature babies who need mechanical assistance from a ventilator to breathe and for some this may only be for 1 to 2 days as the effect of artificial substances (surfactant) given through the breathing tube located in their lungs takes effect and they can move to high dependency care.
# Sophisticated mechanical ventilation with oxygen, intravenous feeding, and the use of incubators to control body temperature and protect from infection. Care may also involve treatment of illnesses that are more common in vulnerable babies. NIC is also required for a small number of larger, more mature, babies who become ill from complications of delivery, from infection or metabolic disorders or when surgical or other treatment is required for congenital anomalies such as congenital heart disease, disorders of the lung or gut, or of other organs.
  

# df1 <-Monthly_cc_sitrep %>%
#   group_by(Name) %>% 
#   mutate(process_mean = ifelse(is.na(Percentage_adult_beds_available), NA, mean(Percentage_adult_beds_available, na.rm = TRUE))) %>% 
#   mutate(mR = abs(Percentage_adult_beds_available - lag(Percentage_adult_beds_available))) %>% 
#   mutate(mean_mR = ifelse(is.na(Percentage_adult_beds_available), NA, mean(mR, na.rm = TRUE))) %>% 
#   mutate(seq_dev = mean_mR / 1.128) %>% 
#   mutate(lower_control_limit = process_mean - (seq_dev * 3),
#          upper_control_limit = process_mean + (seq_dev * 3)) %>% 
#   mutate(one_sigma_lci = process_mean - seq_dev,
#          one_sigma_uci = process_mean + seq_dev,
#          two_sigma_lci = process_mean - (seq_dev * 2),
#          two_sigma_uci = process_mean + (seq_dev * 2)) %>% 
#   mutate(zone = ifelse(Percentage_adult_beds_available > upper_control_limit, 'Outside +/- 3sigma', ifelse(Percentage_adult_beds_available < lower_control_limit, 'Outside +/- 3sigma', ifelse(Percentage_adult_beds_available > two_sigma_uci, 'Between +/- 2sigma and 3sigma', ifelse(Percentage_adult_beds_available < two_sigma_lci, 'Between +/- 2sigma and 3sigma', ifelse(Percentage_adult_beds_available > one_sigma_uci, 'Between +/- 1sigma and 2sigma', ifelse(Percentage_adult_beds_available < one_sigma_lci, 'Between +/- 1sigma and 2sigma', 'Within +/- 1sigma'))))))) %>% 
#   mutate(rule_1 = ifelse(Percentage_adult_beds_available > upper_control_limit, 'Special cause concern', ifelse(Percentage_adult_beds_available < lower_control_limit, 'Special cause concern', 'Common cause variation'))) %>% # This highlights any values outside the control limits
#   mutate(above_mean = ifelse(Percentage_adult_beds_available > process_mean, 1, 0)) 
# 
# df2 <- df1 %>% 
#   filter(Name == 'Portsmouth Hospitals NHS Trust')
# 
# ggplot(data = df2, aes(x = Date, y = Percentage_adult_beds_available, group = 1)) +
#   geom_hline(aes(yintercept = process_mean),
#              colour = "#264852",
#              lwd = .8) +
#   geom_line(colour = '#999999') +
#   geom_hline(aes(yintercept = lower_control_limit),
#              colour = "#A8423F",
#              linetype="dotted",
#              lwd = .7) +
#   geom_hline(aes(yintercept = upper_control_limit),
#              colour = "#A8423F",
#              linetype="dotted",
#              lwd = .7) +
#   geom_hline(aes(yintercept = two_sigma_uci),
#              colour = "#3d2b65",
#              linetype="dashed",
#              lwd = .7) +
#   geom_hline(aes(yintercept = two_sigma_lci),
#              colour = "#3d2b65",
#              linetype="dashed",
#              lwd = .7) +
#   geom_hline(aes(yintercept = one_sigma_uci),
#              colour = "#45748d",
#              linetype="solid",
#              lwd = .4) +
#   geom_hline(aes(yintercept = one_sigma_lci),
#              colour = "#45748d",
#              linetype="solid",
#              lwd = .4) +
#   labs(caption = "Note: Y axis does not start at zero.\nThe red dotted lines represent 99% control limits (3σ, moving range) control limits respectively.\nThe thick solid line represents the long term average.") +
#   ph_theme() +
#   scale_y_continuous(labels = percent_format(accuracy = 1)) +
#   theme(legend.position = "bottom", 
#         # axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, size = 8), 
#         plot.background = element_rect(fill = "white", colour = "#E2E2E3"), 
#         panel.background = element_rect(fill = '#ffffff'),
#         axis.line = element_line(colour = "#E7E7E7", size = .3),
#         axis.text = element_text(colour = "#000000", size = 8), 
#         plot.title = element_text(colour = "#000000", face = "bold", size = 10, vjust = -.5), 
#         axis.title = element_text(colour = "#000000", face = "bold", size = 8),     
#         panel.grid = element_blank(), 
#         strip.background = element_rect(fill = "#327d9c"),
#         axis.ticks = element_line(colour = "#9e9e9e"),
#         legend.key = element_rect(fill = '#ffffff'),
#         legend.text = element_text(colour = "#000000", size = 8),
#         legend.title = element_text(face = 'bold', size = 8))


trust_sitrep_per_capita <- trust_sitrep %>% 
  filter(Date == max(Date)) %>% 
  mutate(Name = ifelse(Name == 'Brighton and Sussex University Hospitals NHS Trust', 'Brighton and Sussex\nUniversity Hospitals\nNHS Trust', ifelse(Name == 'Western Sussex Hospitals NHS Foundation Trust', 'Western Sussex\nHospitals NHS\nFoundation Trust', ifelse(Name == 'East Sussex Healthcare NHS Trust', 'East Sussex\nHealthcare\nNHS Trust', ifelse(Name == 'Surrey and Sussex Healthcare NHS Trust','Surrey and Sussex\nHealthcare\nNHS Trust', ifelse(Name == 'Queen Victoria Hospital NHS Foundation Trust', 'Queen Victoria\nHospital NHS\nFoundation Trust', Name)))))) %>% 
  mutate(Name = factor(Name, levels = c("Queen Victoria\nHospital NHS\nFoundation Trust","Surrey and Sussex\nHealthcare\nNHS Trust", "East Sussex\nHealthcare\nNHS Trust","Western Sussex\nHospitals NHS\nFoundation Trust","Brighton and Sussex\nUniversity Hospitals\nNHS Trust"))) %>% 
  mutate(cc_beds_available_per_100000 = (adult_beds_available / Emergency_catchment_pop_2018) *100000) %>% 
  mutate(cc_beds_occupied_per_100000 = (adult_beds_occupied / Emergency_catchment_pop_2018) *100000) %>% 
  select(Name, cc_beds_per_100000, cc_beds_available_per_100000, cc_beds_occupied_per_100000) %>% 
  gather(key = "Status", value = "Beds", c(cc_beds_occupied_per_100000, cc_beds_available_per_100000))

n_beds_occupied_jan_trust <- ggplot(Jan_2020_ses_stp, aes(x = Name, y = Beds, group = Status, fill = Status)) +
  geom_bar(stat = "identity", width = 0.8) +
  scale_fill_manual(values = c('#f9ac26', '#DBDBDB'),
                    name = 'Bed status',
                    labels = c('Available', 'Occupied')) +
  coord_flip() +
  labs(x = NULL,
       y = 'Beds') +
  ph_theme() 

n_beds_occupied_jan_trust_per_capita <- ggplot(trust_sitrep_per_capita, aes(x = Name, y = Beds, group = Status, fill = Status)) +
  geom_bar(stat = "identity", width = 0.8) +
  scale_fill_manual(values = c('#f9ac26', '#DBDBDB'),
                    name = 'Bed status',
                    labels = c('Available', 'Occupied')) +
  scale_y_continuous(limits = c(0,10),
                     breaks = seq(0,10, 1),
                     expand = c(0,0.01)) +
  coord_flip() +
  labs(x = NULL,
       y = 'Beds per 100,000 population') +
  ph_theme() +
  theme(axis.text.y = element_blank())

grid.arrange(n_beds_occupied_jan_trust, n_beds_occupied_jan_trust_per_capita, ncol=2, top = 'Critical care beds by bed status; Sussex and East Surrey STP; January 2020; Actual beds and beds per 100,000', widths = c(2.2,1.8))


beds_open_per_1000000 <- ggplot(trust_sitrep_per_capita, aes(x = Name, y = cc_beds_per_100000)) +
  geom_bar(stat = "identity", 
           width = 0.8,
           fill = '#0093c0') +
  scale_y_continuous(limits = c(0,10),
                     breaks = seq(0,10, 1),
                     expand = c(0,0.01)) +
  coord_flip() +
  labs(x = NULL,
       y = 'Beds per 100,000 population') +
  ph_theme() 


ggplot(trust_sitrep_per_capita, aes(x = Name, y = cc_beds_available_per_100000)) +
  geom_bar(stat = "identity", 
           width = 0.8,
           fill = '#f9ac26') +
  scale_y_continuous(limits = c(0,10),
                     breaks = seq(0,10, 1),
                     expand = c(0,0.01)) +
  coord_flip() +
  labs(x = NULL,
       y = 'Beds available per 100,000 population') +
  ph_theme() 

# 
# Daily_ga_sitrep <- read_csv(paste0(github_repo_dir, '/general_acute_bed_occupancy_daily_sitrep.csv')) %>% 
#   gather(key = 'Metric', value = 'Beds', `Core Beds Open_43801`:`Occupancy rate_43891`) %>% 
#   mutate(Date = as.Date(as.numeric(substr(Metric, nchar(Metric) - 4 , nchar(Metric))), origin = "1899-12-30")) %>% 
#   mutate(Metric = substr(Metric, 1, nchar(Metric) - 6)) %>% 
#   spread(Metric, Beds) %>% 
#   mutate(Occupied = as.numeric(`Total beds occ'd`)) %>% 
#   mutate(Open = as.numeric(`Total Beds Open`)) %>% 
#   mutate(Available = Open - Occupied) %>% 
#   mutate(Occupied_rate = Occupied / Open) %>% 
#   select(Code, Name, Date, Open, Available, Occupied, Occupied_rate)
# 
# Daily_cc_sitrep %>% 
#   write.csv(., paste0(github_repo_dir, '/Daily_adult_cc_sitrep.csv'), row.names = FALSE)
# 
# Daily_ga_sitrep %>% 
#   write.csv(., paste0(github_repo_dir, '/Daily_adult_ga_sitrep.csv'), row.names = FALSE)

Daily_cc_sitrep_1920 <- read_csv(paste0(github_repo_dir, '/Daily_adult_cc_sitrep_201920.csv')) %>% 
  gather(key = "Metric", value = "Beds", `CC Adult Open_43801`:`Occupancy rate_43891`) %>% 
  mutate(Date = as.Date(as.numeric(substr(Metric, nchar(Metric) - 4 , nchar(Metric))), origin = "1899-12-30")) %>% 
  mutate(Metric = substr(Metric, 1, nchar(Metric) - 6)) %>% 
  spread(Metric, Beds) %>% 
  mutate(adult_beds_occupied = as.numeric(`CC Adult Occ`)) %>% 
  mutate(adult_beds_open = as.numeric(`CC Adult Open`)) %>% 
  mutate(adult_beds_available = adult_beds_open - adult_beds_occupied) %>% 
  mutate(Occupied_rate = adult_beds_occupied / adult_beds_open) %>% 
  select(Code, Date, adult_beds_open, adult_beds_available, adult_beds_occupied, Occupied_rate) %>% 
  left_join(etr, by = 'Code') %>% 
  rename(`Region Code` = National_grouping) %>% 
  left_join(Region, by = 'Region Code') %>% 
  left_join(catchment_pop, by = 'Code') %>%
  mutate(cc_beds_per_100000 = (adult_beds_open / Emergency_catchment_pop_2018) * 100000) %>% 
  rename(Region_code = `Region Code`,
         Region_name = `Region name`) %>% 
  select(Code, Name, Region_code, Region_name, Date, adult_beds_open, adult_beds_occupied, adult_beds_available, Occupied_rate, Emergency_catchment_pop_2018, cc_beds_per_100000)

# monthly_return_from_daily <- Daily_cc_sitrep_1920 %>% 
#   filter(Date %in% last_thursdays$last_thursday)  
# 
# Providers_dec_daily <- Daily_cc_sitrep_1920 %>% 
#   filter(Date == '2019-12-26') %>% 
#   unique()

# setdiff(Providers_dec$Name, Providers_dec_daily$Name)

local_trust_cc <- Daily_cc_sitrep %>% 
  filter(Name == 'ENGLAND')

ggplot(data = local_trust_cc, aes(x = Date, y = Occupied_rate, group = 1)) +
  geom_line(colour = '#999999') +
  geom_point(size = 2, 
             shape = 21) +
  labs(title = 'Daily Critical Care bed (Adults) occupancy as a proportion of open Critical Care beds',
       subtitle = paste0(unique(local_trust$Name), ' (', unique(local_trust$Code), ')'),
       x = 'Day',
       y = 'Critical care bed occupancy proportion') +
  scale_y_continuous(limit = c(.75, 1), 
                     label = percent) +
  scale_x_date(date_labels = "%b %d (%A)",
               date_breaks = '1 week',
               date_minor_breaks = '1 day') +
  ph_theme() +
  theme(axis.text.x = element_text(angle = 90))

local_trust_ga <- Daily_ga_sitrep %>% 
  filter(Name == 'ENGLAND')

ggplot(data = local_trust_ga, aes(x = Date, y = Occupied_rate, group = 1)) +
  geom_line(colour = '#999999') +
  geom_point(size = 2, 
             shape = 21) +
  labs(title = 'Daily general and acute bed occupancy as a proportion of all open beds',
       subtitle = paste0(unique(local_trust$Name), ' (', unique(local_trust$Code), ')'),
       x = 'Day',
       y = 'G&A bed occupancy proportion') +
  scale_y_continuous(limit = c(.75, 1), 
                     label = percent) +
  scale_x_date(date_labels = "%b %d (%A)",
               date_breaks = '1 week',
               date_minor_breaks = '1 day') +
  ph_theme() +
  theme(axis.text.x = element_text(angle = 90))


