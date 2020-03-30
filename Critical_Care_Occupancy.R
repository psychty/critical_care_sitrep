# Critical care bed -  winter sitreps

library(easypackages)

libraries(c("readxl", "readr", "plyr", "dplyr", "ggplot2", "png", "tidyverse", "reshape2", "scales", "viridis", "rgdal", "officer", "flextable", "tmaptools", "lemon", "fingertipsR", "PHEindicatormethods", 'jsonlite', 'readODS', 'zoo'))

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

catchment_pop <- read_excel(paste0(github_repo_dir, "/2019 Trust Catchment Populations Worksheet.xlsx"), sheet = "Trust Analysis", col_types = c("text", "text", "text", "text", "text", "numeric", "text", "text", "numeric", "numeric", "numeric", "numeric","numeric")) %>% 
  group_by(CatchmentYear, TrustCode, TrustName, AdmissionType) %>% 
  summarise(Catchment = sum(Catchment, na.rm = TRUE)) %>% 
  filter(CatchmentYear == 2017) %>% 
  filter(AdmissionType == 'Emergency') %>% 
  rename(Emergency_catchment_pop_2017 = Catchment) %>% 
  rename(Code = TrustCode) %>% 
  ungroup() %>% 
  select(Code, Emergency_catchment_pop_2017)

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

Monthly_cc_sitrep_2 <- Monthly_cc_sitrep_2 %>% 
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
setdiff(Monthly_cc_sitrep_2$Code, etr$Code)

Monthly_cc_sitrep <- Monthly_cc_sitrep_1 %>%
  bind_rows(Monthly_cc_sitrep_2) %>% 
  mutate(adult_beds_available = adult_beds_open - adult_beds_occupied) %>% 
  mutate(Percentage_adult_beds_available = ifelse(adult_beds_open == 0, NA, adult_beds_available / adult_beds_open)) %>% 
  mutate(Percentage_adult_cc_beds_occupied = ifelse(adult_beds_open == 0, NA, adult_beds_occupied / adult_beds_open))

rm(Monthly_cc_sitrep_1, Monthly_cc_sitrep_2, Monthly_cc_sitrep_x, Period_x, filepaths_1, filepaths_2)

# Not all trusts have names in the etr (possible due to mergers/closures)
#setdiff(Monthly_cc_sitrep$Code, etr$Code)

# closed organisations have been remived from the dataset
Monthly_cc_sitrep <- Monthly_cc_sitrep %>% 
  mutate(Name = ifelse(Code == 'ENGLAND', 'England', Name)) %>% 
  mutate(Code = ifelse(Code == 'ENGLAND', '-', Code)) %>% 
  filter(!is.na(Name)) %>% 
  left_join(catchment_pop, by = 'Code') %>% 
  mutate(cc_beds_per_100000 = (adult_beds_open / Emergency_catchment_pop_2017) * 100000)

Monthly_cc_sitrep %>% 
  write.csv(., paste0(github_repo_dir, '/Monthly_adult_cc_sitrep.csv'), row.names = FALSE)

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


Daily_cc_sitrep <- read_csv(paste0(github_repo_dir, '/daily_cc_adult_bed_occupancy.csv')) %>% 
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
  mutate(cc_beds_per_100000 = (adult_beds_open / Emergency_catchment_pop_2017) * 100000) %>% 
  rename(Region_code = `Region Code`,
         Region_name = `Region name`) %>% 
  select(Code, Name, Region_code, Region_name, Date, adult_beds_open, adult_beds_occupied, adult_beds_available, Occupied_rate, Emergency_catchment_pop_2017, cc_beds_per_100000)

Daily_ga_sitrep <- read_csv(paste0(github_repo_dir, '/general_acute_bed_occupancy_daily_sitrep.csv')) %>% 
  gather(key = 'Metric', value = 'Beds', `Core Beds Open_43801`:`Occupancy rate_43891`) %>% 
  mutate(Date = as.Date(as.numeric(substr(Metric, nchar(Metric) - 4 , nchar(Metric))), origin = "1899-12-30")) %>% 
  mutate(Metric = substr(Metric, 1, nchar(Metric) - 6)) %>% 
  spread(Metric, Beds) %>% 
  mutate(Occupied = as.numeric(`Total beds occ'd`)) %>% 
  mutate(Open = as.numeric(`Total Beds Open`)) %>% 
  mutate(Available = Open - Occupied) %>% 
  mutate(Occupied_rate = Occupied / Open) %>% 
  select(Code, Name, Date, Open, Available, Occupied, Occupied_rate)

Daily_cc_sitrep %>% 
  write.csv(., paste0(github_repo_dir, '/Daily_adult_cc_sitrep.csv'), row.names = FALSE)

Daily_ga_sitrep %>% 
  write.csv(., paste0(github_repo_dir, '/Daily_adult_ga_sitrep.csv'), row.names = FALSE)

Providers <- Monthly_cc_sitrep %>% 
  select(Code, Name,  Region_code, Region_name) %>% 
  unique() 

SE_sitrep <- Monthly_cc_sitrep %>% 
  filter(Region_name == 'South East Commissioning Region') %>% 
  arrange(Name, Date) %>% 
  mutate(Highlight = ifelse(Name == 'Western Sussex Hospitals NHS Foundation Trust', 'Western Sussex Hospitals NHS Foundation Trust', '')) %>% 
  mutate(Period = factor(Period, levels = unique(Period)))

ggplot(SE_sitrep, aes(x = Period, y = Percentage_adult_beds_available, group = Name, colour = Highlight)) +
  geom_line() +
  scale_colour_manual(values = c('#DBDBDB', '#660066')) +
  scale_y_continuous(labels = percent) +
  # geom_point(size = 2, 
  #            shape = 21) +
  labs(title = 'Critical Care bed (Adults) availability as a proportion of open Critical Care beds',
       x = 'Last Thursday of each month',
       y = 'Proportion of critical care beds available') +
  ph_theme() +
  theme(axis.text.x = element_text(angle = 90))

# Sussex and East Surrey STP
ses_stp <- SE_sitrep %>% 
  filter(Name %in% c('Brighton and Sussex University Hospitals NHS Trust', 'East Sussex Healthcare NHS Trust', 'Queen Victoria Hospital NHS Foundation Trust', 'Surrey and Sussex Healthcare NHS Trust', 'Western Sussex Hospitals NHS Foundation Trust')) %>% 
  arrange(Period) %>% 
  select(Code, Name, Period, adult_beds_occupied, adult_beds_available, adult_beds_open) %>% 
  gather(key = "Status", value = "Beds", c(adult_beds_occupied, adult_beds_available))

ggplot(ses_stp, aes(x = Period, y = Beds, group = Status, fill = Status)) +
  geom_bar(stat = "identity", width = 0.8) +
  scale_fill_manual(values = c('#f9ac26', '#DBDBDB')) +
  scale_y_continuous() +
  ph_theme() +
  theme(axis.text.x = element_text(angle = 90))

Jan_2020_ses_stp <- ses_stp %>% 
  filter(Period == 'January-2020') %>% 
  arrange(adult_beds_open) %>% 
  mutate(Name = factor(Name, levels = unique(Name))) %>% 
  mutate(Percentage_beds = Beds / adult_beds_open)

ggplot(Jan_2020_ses_stp, aes(x = Name, y = Beds, group = Status, fill = Status)) +
  geom_bar(stat = "identity", width = 0.8) +
  scale_fill_manual(values = c('#f9ac26', '#DBDBDB')) +
  coord_flip() +
  ph_theme() 

ggplot(Jan_2020_ses_stp, aes(x = Name, y = Percentage_beds, group = Status, fill = Status)) +
  geom_bar(stat = "identity", width = 0.8) +
  scale_fill_manual(values = c('#f9ac26', '#DBDBDB')) +
  scale_y_continuous(breaks = seq(0,1,.1),
                     labels = percent) +
  coord_flip() +
  ph_theme() 

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
  