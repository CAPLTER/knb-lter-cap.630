
# README ----

# reml slots ----
getSlots("dataset")
  getSlots("distribution")
  getSlots("keywordSet")
    getSlots("keyword")
getSlots("dataTable")
getSlots("physical")
  getSlots("dataFormat")
    getSlots("textFormat")
  getSlots("size")
  getSlots("distribution")
    getSlots("online")
      getSlots("url")
getSlots("additionalInfo")
  getSlots("section")
  getSlots("para")
getSlots("metadataProvider")
  getSlots("individualName")
  getSlots("userId")
getSlots("creator")
  getSlots("individualName")
  getSlots("userId")

# libraries ----
library(EML)
library(RPostgreSQL)
library(RMySQL)
library(tidyverse)
library(tools)
library(readxl)
library(aws.s3)
library(capeml)

# reml-helper-functions ----
source('~/localRepos/reml-helper-tools/writeAttributesFn.R')
source('~/localRepos/reml-helper-tools/createDataTableFromFileFn.R')
source('~/localRepos/reml-helper-tools/createKMLFn.R')
source('~/localRepos/reml-helper-tools/address_publisher_contact_language_rights.R')
source('~/localRepos/reml-helper-tools/createOtherEntityFn.R')
source('~/localRepos/reml-helper-tools/createPeople.R')
source('~/localRepos/reml-helper-tools/createFactorsDataframe.R')

# connections ----

# Amazon
source('~/Documents/localSettings/aws.s3')
  
# postgres
source('~/Documents/localSettings/pg_prod.R')
source('~/Documents/localSettings/pg_local.R')
  
pg <- pg_prod
pg <- pg_local

# mysql
source('~/Documents/localSettings/mysql_prod.R')
prod <- mysql_prod

# dataset details to set first ----
projectid <- 630
packageIdent <- 'knb-lter-cap.630.1'
pubDate <- '2018-01-18'



# data processing ---------------------------------------------------------

ttl_water_quality <- read_excel('TTL data summary_for STEVAN-Jan2018.xlsx',
                                skip = 1,
                                guess_max = 700) %>% 
  select(-contains("monsoon"))

# import errors: number one refers to a decimal date in the Excel file but it
# seems to have been converted acceptably
# Warning messages:
# 1: In read_fun(path = path, sheet = sheet, limits = limits, shim = shim,  :
#    Coercing numeric to date A801 / R801C1
# 2: In read_fun(path = path, sheet = sheet, limits = limits, shim = shim,  :
#    Expecting numeric in K985 / R985C11: got 'no meters today'
# 3: In read_fun(path = path, sheet = sheet, limits = limits, shim = shim,  :
#    Expecting numeric in K986 / R986C11: got 'no meters today'

# empty to NA
ttl_water_quality[,-which(grepl("date", names(ttl_water_quality)))][ttl_water_quality[,-which(grepl("date", names(ttl_water_quality)))] == ''] <- NA


# field notes -------------------------------------------------------------

field_notes <- ttl_water_quality %>% 
  select(sampling_date = date, `field notes`) %>% 
  filter(!is.na(sampling_date)) %>% 
  filter(!is.na(`field notes`)) %>% 
  arrange(sampling_date)


# meter data --------------------------------------------------------------

ttl_meter_data <- ttl_water_quality %>% 
  select(`sampling date`:`dissolved O2, mg/L`) %>%
  rename(
    sampling_date = `sampling date`,
    temperature = `Temp, degC`,
    conductance = `Conductivity, microSiemens/cm`,
    alkalinity = `Alkalinity, mg/L`,
    bicarbonate = `Bicarbonate, mmolar`,
    turbidity = `Turbidity, NTU`,
    sediment_load = `Sediment Load, g/L`,
    dissolved_oxygen = `dissolved O2, mg/L`
  ) %>% 
  filter(!is.na(sampling_date)) %>% 
  full_join(field_notes, by = "sampling_date") %>% 
  rename(field_notes = `field notes`) %>% 
  arrange(sampling_date) %>% 
  as.data.frame()

# filter where all target cols are NA
ttl_meter_data <- ttl_meter_data[rowSums(!is.na(ttl_meter_data[,c("temperature", "pH", "conductance", "alkalinity", "bicarbonate", "turbidity", "sediment_load", "dissolved_oxygen", "field_notes")])) > 0,]

writeAttributes(ttl_meter_data) # write data frame attributes to a csv in current dir to edit metadata

ttl_meter_data_desc <- "Tempe Town Lake water-quality measurements collected with hand-held field meters and by titration"

# factorsToFrame(ttl_meter_data) # no factors

# create data table based on metadata provided in the companion csv
# use createdataTableFn() if attributes and classes are to be passed directly
ttl_meter_data_DT <- createDTFF(dfname = ttl_meter_data,
                                description = ttl_meter_data_desc,
                                dateRangeField = 'sampling_date')


# carbon_nitrogen ---------------------------------------------------------

ttl_carbon_nitrogen <- ttl_water_quality %>% 
  select(`DOC date`:`TN error`) %>%
  rename(
    sampling_date = `DOC date`,
    DOC = `DOC, mg/L`,
    DOC_error = `DOC error`,
    TN = `TN, mg/L`,
    TN_error = `TN error`
  ) %>% 
  filter(!is.na(sampling_date)) %>% 
  full_join(field_notes, by = "sampling_date") %>% 
  rename(field_notes = `field notes`) %>% 
  arrange(sampling_date) %>% 
  as.data.frame()

# filter where all target cols are NA
ttl_carbon_nitrogen <- ttl_carbon_nitrogen[rowSums(!is.na(ttl_carbon_nitrogen[,c("DOC", "DOC_error", "TN", "TN_error", "field_notes")])) > 0,]

writeAttributes(ttl_carbon_nitrogen) # write data frame attributes to a csv in current dir to edit metadata

ttl_carbon_nitrogen_desc <- "Tempe Town Lake carbon and nitrogen concentrations"

# factorsToFrame(ttl_meter_data) # no factors

# create data table based on metadata provided in the companion csv
# use createdataTableFn() if attributes and classes are to be passed directly
ttl_carbon_nitrogen_DT <- createDTFF(dfname = ttl_carbon_nitrogen,
                                     description = ttl_carbon_nitrogen_desc,
                                     dateRangeField = 'sampling_date')


# title and abstract ----
title <- 'Water-quality monitoring in Tempe Town Lake, Tempe, AZ, ongoing since 2005'

# abstract from file or directly as text
abstract <- as(set_TextType("abstract.md"), "abstract") 


# people ----

# creators
hilairyHartnett <- addCreator('h', 'hartnett')
danChilders <- addCreator('d', 'childers')

creators <- c(as(hilairyHartnett, 'creator'),
              as(danChilders, 'creator'))

# metadata providers
hilairyHartnett <- addMetadataProvider('h', 'hartnett')

metadataProvider <-c(as(hilairyHartnett, 'metadataProvider'))


# keywords ----

# CAP IRTs for reference: https://sustainability.asu.edu/caplter/research/
# be sure to include these as appropriate

keywordSet <-
  c(new("keywordSet",
        keywordThesaurus = "LTER controlled vocabulary",
        keyword =  c("urban",
                     "long term",
                     "long term monitoring",
                     "dissolved organic carbon",
                     "dissolved oxygen",
                     "conductivity",
                     "water quality",
                     "water temperature",
                     "lakes",
                     "carbon",
                     "alkalinity",
                     "bicarbonate",
                     "turbidity",
                     "sediments",
                     "ph",
                     "total dissolved nitrogen")),
    new("keywordSet",
        keywordThesaurus = "Creator Defined Keyword Set",
        keyword =  c("time-series")),
    new("keywordSet",
        keywordThesaurus = "LTER core areas",
        keyword =  c("disturbance patterns",
                     "movement of organic matter",
                     "movement of inorganic matter",
                     "water and fluxes",
                     "parks and rivers")),
    new("keywordSet",
        keywordThesaurus = "CAPLTER Keyword Set List",
        keyword =  c("cap lter",
                     "cap",
                     "caplter",
                     "central arizona phoenix long term ecological research",
                     "arizona",
                     "az",
                     "arid land",
                     "tempe")))

# methods and coverages ----
methods <- set_methods("ttl_methods.md")

# if relevant, pulling dates from a DB is nice
# begindate <- dbGetQuery(con, "SELECT MIN(sample_date) AS date FROM database.table;")
# begindate <- begindate$date

begindate <- "2005-01-05"
enddate <- "2017-11-22"
geographicDescription <- "Tempe Town Lake, Tempe, Arizona, USA"
coverage <- set_coverage(begin = begindate,
                         end = enddate,
                         geographicDescription = geographicDescription,
                         west = -111.949, east = -111.910,
                         north = +33.437, south = +33.430)


# project ----

# projectDetails <- new("project",
#                       title = "project title",
#                       personnel = projectPersonnel,
#                       funding = "This project was supported by...")

# construct the dataset ----

# from sourced file:
  # address
  # publisher
  # contact
  # rights
  # distribution

# DATASET
dataset <- new("dataset",
               title = title,
               creator = creators,
               pubDate = pubDate,
               metadataProvider = metadataProvider,
               # associatedParty = associatedParty,
               intellectualRights = rights,
               abstract = abstract,
               keywordSet = keywordSet,
               coverage = coverage,
               contact = contact,
               methods = methods,
               distribution = metadata_dist,
               dataTable = c(ttl_meter_data_DT,
                             ttl_carbon_nitrogen_DT))
               # otherEntity = c(core_arthropod_locations)) # if other entity is relevant

# ls(pattern= "_DT") # can help to pull out DTs

# assembly line flow that would be good to incorporate - build the list of DTs at creation
# data_tables_stored <- list()
# data_tables_stored[[i]] <- data_table
# dataset@dataTable <- new("ListOfdataTable",
#                      data_tables_stored)

# construct the eml ----

# CUSTOM UNITS
# standardUnits <- get_unitList()
# unique(standardUnits$unitTypes$id) # unique unit types

custom_units <- rbind(
  data.frame(id = "microsiemenPerCentimeter",
             unitType = "conductance",
             parentSI = "siemen",
             multiplierToSI = 0.000001,
             description = "electric conductance of lake water in the units of microsiemenPerCentimeter"),
data.frame(id = "nephelometricTurbidityUnit",
           unitType = "unknown",
           parentSI = "unknown",
           multiplierToSI = 1,
           description = "(NTU) ratio of the amount of light transmitted straight through a water sample with the amount scattered at an angle of 90 degrees to one side"))
unitList <- set_unitList(custom_units)


eml <- new("eml",
           schemaLocation = "eml://ecoinformatics.org/eml-2.1.1  http://nis.lternet.edu/schemas/EML/eml-2.1.1/eml.xsd",
           packageId = packageIdent,
           scope = "system",
           system = "knb",
           access = lter_access,
           dataset = dataset,
           additionalMetadata = as(unitList, "additionalMetadata"))


# assembly line code to incorporate next round!

# if (custom_units == "yes"){
#   eml <- new("eml",
#              schemaLocation = "eml://ecoinformatics.org/eml-2.1.1  http://nis.lternet.edu/schemas/EML/eml-2.1.1/eml.xsd",
#              packageId = data_package_id,
#              system = root_system,
#              access = access,
#              dataset = dataset,
#              additionalMetadata = as(unitsList, "additionalMetadata"))
# } else {
#   eml <- new("eml",
#              schemaLocation = "eml://ecoinformatics.org/eml-2.1.1  http://nis.lternet.edu/schemas/EML/eml-2.1.1/eml.xsd",
#              packageId = data_package_id,
#              system = root_system,
#              access = access,
#              dataset = dataset)
# }
# 
# # Write EML
# 
# print("Writing EML ...")
# 
# write_eml(eml, paste(path, "/", data_package_id, ".xml", sep = ""))
# 
# # Validate EML
# 
# print("Validating EML ...")
# 
# validation_result <- eml_validate(eml)
# 
# if (validation_result == "TRUE"){
#   
#   print("EML passed validation!")
#   
# } else {
#   
#   print("EML validaton failed. See warnings for details.")
#   
# }

# write the xml to file ----
write_eml(eml, "knb-lter-cap.630.1.xml")


# S3 functions ----

# misc commands
 
  # get list of buckets
  # bucketlist()
  # 
  # add an object to S3 - datasets
  # put_object(file = '649_maintenance_log_dd68e293482738ac6f05303d473687a2.csv',
  #            object = '/datasets/cap/649_maintenance_log_dd68e293482738ac6f05303d473687a2.csv',
  #            bucket = 'gios-data')
  # 
  # add an object to S3 - metadata
  # put_object(file = '~/Dropbox/development/knb-lter-cap.650.1/knb-lter-cap.650.1.xml',
  #            object = '/metadata/knb-lter-cap.650.1.xml',
  #            bucket = 'gios-data')
  # 
  # get files in the gios-data bucket with the prefix datasets/cap/650
  # get_bucket(bucket = 'gios-data',
  #            prefix = 'datasets/cap/650')

# data file to S3
dataToAmz <- function(fileToUpload) {
  
  put_object(file = fileToUpload,
             object = paste0('/datasets/cap/', basename(fileToUpload)),
             bucket = 'gios-data') 
  
}

# example
dataToAmz('630_ttl_meter_data_a8d5bff9348d1fba59a96af822289c46.csv')
dataToAmz('630_ttl_carbon_nitrogen_19728b17c228953f8318200e8da8dc30.csv')


# metadata file to S3
emlToAmz <- function(fileToUpload) {
  
  put_object(file = fileToUpload,
             object = paste0('/metadata/', basename(fileToUpload)),
             bucket = 'gios-data') 
  
}

# example
# emlToAmz('~/localRepos/cap-data/cap-data-eml/knb-lter-cap.650.1.xml')