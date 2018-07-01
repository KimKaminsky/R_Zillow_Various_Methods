#install.packages("xgboost")
library(dplyr)
library(forecast)
#library(fma)
library(rattle)
library(stringr)
library("ggmap")
library(corrplot)
library(tidyr)
library(gbm)
library(Metrics)
library(relaimpo)
library(data.table) 
library(caret)
library(xgboost)

#############
# Functions #
#############

# Get a list of variables with missing values for Training data
tmv <- function() {
  train.missing.values <- data.frame(matrix(ncol = 2, nrow = 0))
  cols <- c("variable", "numMissing")
  colnames(train.missing.values) <- cols
  
  for (i in 1:length(train)) {
    
    if (sum(is.na(train[i])) > 0) {
      train.missing.values[nrow(train.missing.values) + 1,] <-
        c(noquote(names(train[i])), sum(is.na(train[i])))
    }
  }
  
  train.missing.values
}

# Get a list of variables with missing values for Training data - 2nd set from 2017
tmv2 <- function() {
  train.missing.values <- data.frame(matrix(ncol = 2, nrow = 0))
  cols <- c("variable", "numMissing")
  colnames(train.missing.values) <- cols
  
  for (i in 1:length(train17)) {
    
    if (sum(is.na(train17[i])) > 0) {
      train.missing.values[nrow(train.missing.values) + 1,] <-
        c(noquote(names(train17[i])), sum(is.na(train17[i])))
    }
  }
  
  train.missing.values
}

# Get a list of variables with missing values for properties_2016 data
tmv2016 <- function() {
  train.miss.values <- data.frame(matrix(ncol = 2, nrow = 0))
  cols <- c("variable", "numMissing")
  colnames(train.miss.values) <- cols
  
  for (i in 1:length(properties_2016)) {
    
    if (sum(is.na(properties_2016[i])) > 0) {
      train.miss.values[nrow(train.miss.values) + 1,] <-
        c(noquote(names(properties_2016[i])), sum(is.na(properties_2016[i])))
    }
  }
  
  train.miss.values
}


#################
#  Get the data #
#################

train_2016 <- read.csv("D:/Kim MSPA/Predict 413/Midterm - Zillow Kaggle/train_2016.csv ", header = T)
properties_2016 <- read.csv("D:/Kim MSPA/Predict 413/Midterm - Zillow Kaggle/properties_2016.csv", header = T)
sample_submission <- read.csv("D:/Kim MSPA/Predict 413/Midterm - Zillow Kaggle/sample_submission.csv", header = T)

train_2017 <- read.csv("D:/Kim MSPA/Predict 413/Midterm - Zillow Kaggle/train_2017.csv ", header = T)
properties_2017 <- read.csv("D:/Kim MSPA/Predict 413/Midterm - Zillow Kaggle/properties_2017.csv", header = T)

# 2985217
nrow(properties_2016)

# 2985217
nrow(sample_submission)

# 2985217
nrow(properties_2017)

# 90275
nrow(train_2016)

# 77613
nrow(train_2017)


#############################################
# Rename variables for more intuitive names #
#############################################

properties_2016 <- properties_2016 %>% rename(
  id_parcel = parcelid,
  build_year = yearbuilt,
  area_basement = basementsqft,
  area_patio = yardbuildingsqft17,
  area_shed = yardbuildingsqft26, 
  area_pool = poolsizesum,  
  area_lot = lotsizesquarefeet, 
  area_garage = garagetotalsqft,
  area_firstfloor_finished = finishedfloor1squarefeet,
  area_total_calc = calculatedfinishedsquarefeet,
  area_base = finishedsquarefeet6,
  area_live_finished = finishedsquarefeet12,
  area_liveperi_finished = finishedsquarefeet13,
  area_total_finished = finishedsquarefeet15,  
  area_unknown = finishedsquarefeet50,
  num_unit = unitcnt, 
  num_story = numberofstories,  
  num_room = roomcnt,
  num_bathroom = bathroomcnt,
  num_bedroom = bedroomcnt,
  num_bathroom_calc = calculatedbathnbr,
  num_bath = fullbathcnt,  
  num_75_bath = threequarterbathnbr, 
  num_fireplace = fireplacecnt,
  num_pool = poolcnt,  
  num_garage = garagecarcnt,  
  region_county = regionidcounty,
  region_city = regionidcity,
  region_zip = regionidzip,
  region_neighbor = regionidneighborhood,  
  tax_total = taxvaluedollarcnt,
  tax_building = structuretaxvaluedollarcnt,
  tax_land = landtaxvaluedollarcnt,
  tax_property = taxamount,
  tax_year = assessmentyear,
  tax_delinquency = taxdelinquencyflag,
  tax_delinquency_year = taxdelinquencyyear,
  zoning_property = propertyzoningdesc,
  zoning_landuse = propertylandusetypeid,
  zoning_landuse_county = propertycountylandusecode,
  flag_fireplace = fireplaceflag, 
  flag_tub = hashottuborspa,
  quality = buildingqualitytypeid,
  framing = buildingclasstypeid,
  material = typeconstructiontypeid,
  deck = decktypeid,
  story = storytypeid,
  heating = heatingorsystemtypeid,
  aircon = airconditioningtypeid,
  architectural_style= architecturalstyletypeid
)
train_2016 <- train_2016 %>% rename(
  id_parcel = parcelid,
  date = transactiondate
)

properties_2016 <- properties_2016 %>% 
  mutate(tax_delinquency = ifelse(tax_delinquency=="Y",1,0),
         flag_fireplace = ifelse(flag_fireplace=="Y",1,0),
         flag_tub = ifelse(flag_tub=="Y",1,0))

train_2016 <- train_2016 %>% mutate(abs_logerror = abs(logerror))


properties_2017 <- properties_2017 %>% rename(
  id_parcel = parcelid,
  build_year = yearbuilt,
  area_basement = basementsqft,
  area_patio = yardbuildingsqft17,
  area_shed = yardbuildingsqft26, 
  area_pool = poolsizesum,  
  area_lot = lotsizesquarefeet, 
  area_garage = garagetotalsqft,
  area_firstfloor_finished = finishedfloor1squarefeet,
  area_total_calc = calculatedfinishedsquarefeet,
  area_base = finishedsquarefeet6,
  area_live_finished = finishedsquarefeet12,
  area_liveperi_finished = finishedsquarefeet13,
  area_total_finished = finishedsquarefeet15,  
  area_unknown = finishedsquarefeet50,
  num_unit = unitcnt, 
  num_story = numberofstories,  
  num_room = roomcnt,
  num_bathroom = bathroomcnt,
  num_bedroom = bedroomcnt,
  num_bathroom_calc = calculatedbathnbr,
  num_bath = fullbathcnt,  
  num_75_bath = threequarterbathnbr, 
  num_fireplace = fireplacecnt,
  num_pool = poolcnt,  
  num_garage = garagecarcnt,  
  region_county = regionidcounty,
  region_city = regionidcity,
  region_zip = regionidzip,
  region_neighbor = regionidneighborhood,  
  tax_total = taxvaluedollarcnt,
  tax_building = structuretaxvaluedollarcnt,
  tax_land = landtaxvaluedollarcnt,
  tax_property = taxamount,
  tax_year = assessmentyear,
  tax_delinquency = taxdelinquencyflag,
  tax_delinquency_year = taxdelinquencyyear,
  zoning_property = propertyzoningdesc,
  zoning_landuse = propertylandusetypeid,
  zoning_landuse_county = propertycountylandusecode,
  flag_fireplace = fireplaceflag, 
  flag_tub = hashottuborspa,
  quality = buildingqualitytypeid,
  framing = buildingclasstypeid,
  material = typeconstructiontypeid,
  deck = decktypeid,
  story = storytypeid,
  heating = heatingorsystemtypeid,
  aircon = airconditioningtypeid,
  architectural_style= architecturalstyletypeid
)
train_2017 <- train_2017 %>% rename(
  id_parcel = parcelid,
  date = transactiondate
)

properties_2017 <- properties_2017 %>% 
  mutate(tax_delinquency = ifelse(tax_delinquency=="Y",1,0),
         flag_fireplace = ifelse(flag_fireplace=="Y",1,0),
         flag_tub = ifelse(flag_tub=="Y",1,0))

train_2017 <- train_2017 %>% mutate(abs_logerror = abs(logerror))


################################
# Examine data and merge files #
################################

head(train_2016)
head(properties_2016)
train <- train_2016 %>% left_join(properties_2016, by = "id_parcel")
train17 <- train_2017 %>% left_join(properties_2017, by = "id_parcel")
head(train)
head(train17)

tmv()
tmv2()


#########################################################
# Some variables can be removed as they don't add value #
#########################################################

# There is only one value
train$tax_year <- NULL
train$flag_fireplace <- NULL
train$flag_tub <- NULL

# There is only one value
train17$tax_year <- NULL
train17$flag_fireplace <- NULL
train17$flag_tub <- NULL


####################################################
# Initial correlation plots for numeric variables  #
####################################################

# Pull out the variables with too many missing values 
nums <- sapply(Train, is.numeric)
train <- Train[ , nums]

missing_values <- train %>% summarize_each(funs(sum(is.na(.))/n()))
missing_values <- gather(missing_values, key="feature", value="missing_pct")
good_features <- filter(missing_values, missing_pct<0.75)
# START HERE
vars <- good_features$feature[str_detect(good_features$feature,'num_')]
tmp <- train %>% dplyr::select(one_of(good_features))

# Good correlations: num_baththroom_calc, num_bath and num_garage
# Weaker correlations: num_room and num_unit
corrplot(cor(tmp, use="complete.obs"),type="lower")

# Correlation plot of variables starting with tax
vars <- good_features$feature[str_detect(good_features$feature,'tax_')]
tmp <- train %>% dplyr::select(one_of(c(vars,"abs_logerror")))

# No correlations
corrplot(cor(tmp, use="complete.obs"),type="lower")

# Correlation plot of variables starting with area
vars <- good_features$feature[str_detect(good_features$feature,'area_')]
tmp <- train %>% dplyr::select(one_of(c(vars,"abs_logerror")))

# No correlations
corrplot(cor(tmp, use="complete.obs"),type="lower")

# list of missing values 
tmv()

# number of rows to see if some data points should just be dropped
nrow(train) #90275


# Do the same as above for 2017
missing_values <- train17 %>% summarize_each(funs(sum(is.na(.))/n()))
missing_values <- gather(missing_values, key="feature", value="missing_pct")
good_features <- filter(missing_values, missing_pct<0.75)
vars <- good_features$feature[str_detect(good_features$feature,'num_')]
tmp <- train17 %>% select(one_of(c(vars,"abs_logerror")))

# Good correlations: num_baththroom_calc, num_bath and num_garage
# Weaker correlations: num_room and num_unit
corrplot(cor(tmp, use="complete.obs"),type="lower")

# Correlation plot of variables starting with tax
vars <- good_features$feature[str_detect(good_features$feature,'tax_')]
tmp <- train17 %>% select(one_of(c(vars,"abs_logerror")))

# No correlations
corrplot(cor(tmp, use="complete.obs"),type="lower")

# Correlation plot of variables starting with area
vars <- good_features$feature[str_detect(good_features$feature,'area_')]
tmp <- train17 %>% select(one_of(c(vars,"abs_logerror")))

# No correlations
corrplot(cor(tmp, use="complete.obs"),type="lower")

#rattle()

####################################################
# Impute Missing Values using decision tree rules  #
####################################################

# Impute tax_total
train$m_tax_total <- 0
train$m_tax_total[is.na(train$tax_total)] <- 1
train$tax_total[is.na(train$tax_total)] <- 168709

train17$m_tax_total <- 0
train17$m_tax_total[is.na(train17$tax_total)] <- 1
train17$tax_total[is.na(train17$tax_total)] <- 0

# Impute tax_land
train$m_tax_land <- 0
train$m_tax_land[is.na(train$tax_land)] <- 1
train$tax_land[is.na(train$tax_land)] <- 74070

train17$m_tax_land <- 0
train17$m_tax_land[is.na(train17$tax_land)] <- 1
train17$tax_land[is.na(train17$tax_land)] <- median(train17$tax_land[!is.na(train17$tax_land)])

# Impute tax_property
train$m_tax_property <- 0
train$m_tax_property[is.na(train$tax_property)] <- 1
train$tax_property[is.na(train$tax_property) & train$tax_total < 5.131e+05] <- 3745
train$tax_property[is.na(train$tax_property) & train$tax_total < 1.409e+06
                   & train$tax_total >= 5.131e+05] <- 9356
train$tax_property[is.na(train$tax_property) & train$tax_total < 4.761e+06
                   & train$tax_total >= 1.409e+06] <- 25800
train$tax_property[is.na(train$tax_property) & train$tax_total >= 4.761e+06] <- 85855

train17$m_tax_property <- 0
train17$m_tax_property[is.na(train17$tax_property)] <- 1
train17$tax_property[is.na(train17$tax_property) & train17$tax_total < 6.38e+05] <- 3844
train17$tax_property[is.na(train17$tax_property) & train17$tax_total < 1.924e+06
                     & train17$tax_total >= 6.38e+05] <- 11368
train17$tax_property[is.na(train17$tax_property) & train17$tax_total < 8.258e+06
                     & train17$tax_total >= 1.924e+06] <- 35482
train17$tax_property[is.na(train17$tax_property) & train17$tax_total >= 8.258e+06] <- 158447



# Impute tax_building
train$m_tax_building <- 0
train$m_tax_building[is.na(train$tax_building)] <- 1
train$tax_building[is.na(train$tax_building)
                   & train$tax_total < 4.82e+05] <- 114668
train$tax_building[is.na(train$tax_building) &
                     train$tax_total >= 4.82e+05 & train$tax_total < 1.24e+06] <- 255698
train$tax_building[is.na(train$tax_building) &
                     train$tax_total >= 1.24e+06 & train$tax_property < 6.355e+04] <- 685596
train$tax_building[is.na(train$tax_building) &
                     train$tax_total >= 1.24e+06 & train$tax_property >= 6.355e+04] <- 2455593

train17$m_tax_building <- 0
train17$m_tax_building[is.na(train17$tax_building)] <- 1
train17$tax_building[is.na(train17$tax_building)
                     & train17$tax_property< 6771] <- 122900
train17$tax_building[is.na(train17$tax_building) &
                       train17$tax_property>=6771 & train17$tax_property< 1.701e+04] <- 291689
train17$tax_building[is.na(train17$tax_building) &
                       train17$tax_property>=1.701e+04 & train17$tax_property< 5.933e+04] <- 749209
train17$tax_building[is.na(train17$tax_building) &
                       train17$tax_property>=5.933e+04] <- 2411452


# Impute censustractandblock - this is censustractandblock with another number appended
# have no way to get that number so pad with zeros
train$m_censustractandblock <- 0
train$m_censustractandblock[is.na(train$censustractandblock)] <- 1
train$censustractandblock[is.na(train$censustractandblock)] <-
  as.numeric(str_pad(train$rawcensustractandblock[is.na(train$censustractandblock)], width = 14, side = "right", pad = "0"))

train17$m_censustractandblock <- 0
train17$m_censustractandblock[is.na(train17$censustractandblock)] <- 1
train17$censustractandblock[is.na(train17$censustractandblock)] <-
  as.numeric(str_pad(train17$rawcensustractandblock[is.na(train17$censustractandblock)], width = 14, side = "right", pad = "0"))


# num_bathroom has a number of zero values which doesn't make sense - clean this up
train$num_bathroom[train$num_bathroom == 0 & train$area_total_calc < 904.5] <- 1
train$num_bathroom[train$num_bathroom == 0 & train$area_total_calc >= 904.5
                   & train$area_total_calc < 1174] <- 1.5
train$num_bathroom[train$num_bathroom == 0 & train$area_total_calc < 1654 
                   & train$area_total_calc >= 1174] <- 2
train$num_bathroom[train$num_bathroom == 0 & train$area_total_calc >= 1654 
                   & train$area_total_calc < 2238] <- 2.5
train$num_bathroom[train$num_bathroom == 0 & train$area_total_calc >= 2238 
                   & train$area_total_calc < 3030] <- 3
train$num_bathroom[train$num_bathroom == 0 & train$area_total_calc >= 3030 
                   & train$area_total_calc < 3530] <- 3.5
train$num_bathroom[train$num_bathroom == 0 & train$area_total_calc >= 3530 
                   & train$area_total_calc < 4602] <- 4
train$num_bathroom[train$num_bathroom == 0 & train$area_total_calc >= 4602] <- 5.5

train17$m_num_bathroom <- 0
train17$m_num_bathroom[is.na(train17$num_bathroom)] <- 1
train17$num_bathroom[is.na(train17$num_bathroom)] <- median(train17$num_bathroom[!is.na(train17$num_bathroom)])

train17$num_bathroom[train17$num_bathroom == 0 & train17$area_total_calc < 904.5] <- 1
train17$num_bathroom[train17$num_bathroom == 0 & train17$area_total_calc >= 904.5
                     & train17$area_total_calc < 1174] <- 1.5
train17$num_bathroom[train17$num_bathroom == 0 & train17$area_total_calc < 1654 
                     & train17$area_total_calc >= 1174] <- 2
train17$num_bathroom[train17$num_bathroom == 0 & train17$area_total_calc >= 1654 
                     & train17$area_total_calc < 2238] <- 2.5
train17$num_bathroom[train17$num_bathroom == 0 & train17$area_total_calc >= 2238 
                     & train17$area_total_calc < 3030] <- 3
train17$num_bathroom[train17$num_bathroom == 0 & train17$area_total_calc >= 3030 
                     & train17$area_total_calc < 3530] <- 3.5
train17$num_bathroom[train17$num_bathroom == 0 & train17$area_total_calc >= 3530 
                     & train17$area_total_calc < 4602] <- 4
train17$num_bathroom[train17$num_bathroom == 0 & train17$area_total_calc >= 4602] <- 5.5


# Impute num_bathroom_calc -- This is actually always the same as num_bathroom, so I'll delete this variable
#  However it might be helpful to know which records were missing this data so I'll keep an indicator
train$m_num_bathroom_calc <- 0
train$m_num_bathroom_calc[is.na(train$num_bathroom_calc)] <- 1
train$num_bathroom_calc[is.na(train$num_bathroom_calc)] <- train$num_bathroom[is.na(train$num_bathroom_calc)]

train17$m_num_bathroom_calc <- 0
train17$m_num_bathroom_calc[is.na(train17$num_bathroom_calc)] <- 1
train17$num_bathroom_calc[is.na(train17$num_bathroom_calc)] <- train17$num_bathroom[is.na(train17$num_bathroom_calc)]


# Impute num_bath - this is the bathroom count rounded down to the nearest whole number
train$m_num_bath <- 0
train$m_num_bath[is.na(train$num_bath)] <- 1
train$num_bath[is.na(train$num_bath)] <- floor(train$num_bathroom[is.na(train$num_bath)])

train17$m_num_bath <- 0
train17$m_num_bath[is.na(train17$num_bath)] <- 1
train17$num_bath[is.na(train17$num_bath)] <- floor(train17$num_bathroom[is.na(train17$num_bath)])


# num_bedroom has a number of zero values which doesn't make sense - clean this up
train$num_bedroom[train$num_bedroom == 0 & train$area_total_calc < 781.5] <- 1
train$num_bedroom[train$num_bedroom == 0 & train$area_total_calc > 781.5
                  & train$area_total_calc < 1024] <- 2
train$num_bedroom[train$num_bedroom == 0 & train$area_total_calc >= 1024 
                  & train$area_total_calc < 1724] <- 3
train$num_bedroom[train$num_bedroom == 0 & train$area_total_calc >= 1724
                  & train$num_bathroom < 3.75 & train$area_total_calc < 2184] <- 3
train$num_bedroom[train$num_bedroom == 0 & train$area_total_calc >= 1724
                  & train$num_bathroom < 3.75 & train$area_total_calc >= 2184] <- 4
train$num_bedroom[train$num_bedroom == 0 & train$area_total_calc >= 1724
                  & train$num_bathroom >= 3.75 &  train$num_bathroom < 5.75] <- 5
train$num_bedroom[train$num_bedroom == 0 & train$area_total_calc >= 1724
                  & train$num_bathroom >= 3.75 &  train$num_bathroom >= 5.75] <- 6

train17$m_num_bedroom<- 0
train17$m_num_bedroom[is.na(train17$num_bedroom)] <- 1
train17$num_bedroom[is.na(train17$num_bedroom)] <- median(train17$num_bedroom[!is.na(train17$num_bedroom)])

train17$num_bedroom[train17$num_bedroom == 0 & train17$area_total_calc < 781.5] <- 1
train17$num_bedroom[train17$num_bedroom == 0 & train17$area_total_calc > 781.5
                    & train17$area_total_calc < 1024] <- 2
train17$num_bedroom[train17$num_bedroom == 0 & train17$area_total_calc >= 1024 
                    & train17$area_total_calc < 1724] <- 3
train17$num_bedroom[train17$num_bedroom == 0 & train17$area_total_calc >= 1724
                    & train17$num_bathroom < 3.75 & train17$area_total_calc < 2184] <- 3
train17$num_bedroom[train17$num_bedroom == 0 & train17$area_total_calc >= 1724
                    & train17$num_bathroom < 3.75 & train17$area_total_calc >= 2184] <- 4
train17$num_bedroom[train17$num_bedroom == 0 & train17$area_total_calc >= 1724
                    & train17$num_bathroom >= 3.75 &  train17$num_bathroom < 5.75] <- 5
train17$num_bedroom[train17$num_bedroom == 0 & train17$area_total_calc >= 1724
                    & train17$num_bathroom >= 3.75 &  train17$num_bathroom >= 5.75] <- 6


# Impute area_total_calc
train$m_area_total_calc <- 0
train$m_area_total_calc[is.na(train$area_total_calc)] <- 1
train$area_total_calc[is.na(train$area_total_calc)
                      & train$num_bathroom < 2.25] <- 1338
train$area_total_calc[is.na(train$area_total_calc) &
                        train$num_bathroom > 2.25 & train$num_bathroom < 3.25] <- 2068
train$area_total_calc[is.na(train$area_total_calc) &
                        train$num_bathroom < 5.25 & train$num_bathroom >= 3.25] <- 3391
train$area_total_calc[is.na(train$area_total_calc) &
                        train$num_bathroom >= 5.25] <- 5514

train17$m_area_total_calc <- 0
train17$m_area_total_calc[is.na(train17$area_total_calc)] <- 1
train17$area_total_calc[is.na(train17$area_total_calc)
                        & train17$num_bathroom < 2.25] <- 1338
train17$area_total_calc[is.na(train17$area_total_calc) &
                          train17$num_bathroom > 2.25 & train17$num_bathroom < 3.25] <- 2068
train17$area_total_calc[is.na(train17$area_total_calc) &
                          train17$num_bathroom < 5.25 & train17$num_bathroom >= 3.25] <- 3391
train17$area_total_calc[is.na(train17$area_total_calc) &
                          train17$num_bathroom >= 5.25] <- 5514


train17$m_latitude <- 0
train17$m_latitude[is.na(train17$latitude)] <- 1
train17$latitude[is.na(train17$latitude)] <- as.numeric(names(which.max(table(train17$latitude))))

train17$m_longitude <- 0
train17$m_longitude[is.na(train17$longitude)] <- 1
train17$longitude[is.na(train17$longitude)] <-  -118444000


# Impute region_zip 
train$m_region_zip <- 0
train$m_region_zip[is.na(train$region_zip)] <- 1

# Create subset dataset that incluldes all region_zip fields that are missing
zipFinder <- subset(train, is.na(train$region_zip), select = c(region_zip, longitude, latitude))

# Loop through subset of data- get longitude and latitude data and use ggmap library to look 
#  up the zip code using the longitude and latitude. Then set the zip code to the missing value
for (i in 1:nrow(zipFinder)) {
  
  lonlat_sample <- c(round(zipFinder$longitude[i] / 986931, 2),
                     round(zipFinder$latitude[i] / 867022, 2))
  res <- ggmap::revgeocode(lonlat_sample, output = "more")
  zipcode <- res$postal_code
  train$region_zip[is.na(train$region_zip) & train$longitude == zipFinder$longitude[i]
                   & train$latitude == zipFinder$latitude[i]] <- zipcode
}

tmv()

train17$m_region_zip <- 0
train17$m_region_zip[is.na(train17$region_zip)] <- 1

# Create subset dataset that incluldes all region_zip fields that are missing
zipFinder <- subset(train17, is.na(train17$region_zip), select = c(region_zip, longitude, latitude))

# Loop through subset of data- get longitude and latitude data and use ggmap library to look 
#  up the zip code using the longitude and latitude. Then set the zip code to the missing value
for (i in 1:nrow(zipFinder)) {
  
  lonlat_sample <- c(round(zipFinder$longitude[i] / 986931, 2),
                     round(zipFinder$latitude[i] / 867022, 2))
  res <- ggmap::revgeocode(lonlat_sample, output = "more")
  zipcode <- res$postal_code
  if(is.integer(zipcode)) 
    train17$region_zip[is.na(train17$region_zip) & train17$longitude == zipFinder$longitude[i]
                       & train17$latitude == zipFinder$latitude[i]] <- zipcode
  else
    train17$region_zip[is.na(train17$region_zip) & train17$longitude == zipFinder$longitude[i]
                       & train17$latitude == zipFinder$latitude[i]] <- 96126
  
}

tmv2()

# Impute build_year
train$m_build_year <- 0
train$m_build_year[is.na(train$build_year)] <- 1
train$build_year[is.na(train$build_year) & train$latitude < 3.436e+07 & !(train$zoning_landuse_county %in%
                                                                            c("010", "0102", "010C", "010D", "010M", "0114", "012C", "012D", "0131", "01DC", "01HC", "020M",
                                                                              "0700", "070D", "1", "1111", "1116", "1117", "1128", "1129", "1200", "122", "1310", "1432",
                                                                              "34"))] <- 1951
train$build_year[is.na(train$build_year) & train$tax_building >= 1.237e+05
                 & train$zoning_landuse_county %in% c("010", "0102", "010C", "010D", "010M", "0114", "012C",
                                                      "012D", "0131", "01DC", "01HC", "020M", "0700", "070D", "1", "1111", "1116", "1117", "1128", "1129",
                                                      "1200", "122", "1310", "1432", "34")] <- 1988
train$build_year[is.na(train$build_year) & train$tax_building < 1.237e+05
                 & train$zoning_landuse_county %in% c("010", "0102", "010C", "010D", "010M", "0114", "012C",
                                                      "012D", "0131", "01DC", "01HC", "020M", "0700", "070D", "1", "1111", "1116", "1117", "1128", "1129",
                                                      "1200", "122", "1310", "1432", "34")] <- 1971
train$build_year[is.na(train$build_year) & train$latitude >= 3.436e+07 & !(train$zoning_landuse_county %in%
                                                                             c("010", "0102", "010C", "010D", "010M", "0114", "012C", "012D", "0131", "01DC", "01HC", "020M",
                                                                               "0700", "070D", "1", "1111", "1116", "1117", "1128", "1129", "1200", "122", "1310", "1432",
                                                                               "34"))] <- 1985

train17$m_build_year <- 0
train17$m_build_year[is.na(train17$build_year)] <- 1
train17$build_year[is.na(train17$build_year) & train17$latitude < 3.436e+07 & !(train17$zoning_landuse_county %in%
                                                                                  c("010", "0102", "010C", "010D", "010M", "0114", "012C", "012D", "0131", "01DC", "01HC", "020M",
                                                                                    "0700", "070D", "1", "1111", "1116", "1117", "1128", "1129", "1200", "122", "1310", "1432",
                                                                                    "34"))] <- 1951
train17$build_year[is.na(train17$build_year) & train17$tax_building >= 1.237e+05
                   & train17$zoning_landuse_county %in% c("010", "0102", "010C", "010D", "010M", "0114", "012C",
                                                          "012D", "0131", "01DC", "01HC", "020M", "0700", "070D", "1", "1111", "1116", "1117", "1128", "1129",
                                                          "1200", "122", "1310", "1432", "34")] <- 1988
train17$build_year[is.na(train17$build_year) & train17$tax_building < 1.237e+05
                   & train17$zoning_landuse_county %in% c("010", "0102", "010C", "010D", "010M", "0114", "012C",
                                                          "012D", "0131", "01DC", "01HC", "020M", "0700", "070D", "1", "1111", "1116", "1117", "1128", "1129",
                                                          "1200", "122", "1310", "1432", "34")] <- 1971
train17$build_year[is.na(train17$build_year) & train17$latitude >= 3.436e+07 & !(train17$zoning_landuse_county %in%
                                                                                   c("010", "0102", "010C", "010D", "010M", "0114", "012C", "012D", "0131", "01DC", "01HC", "020M",
                                                                                     "0700", "070D", "1", "1111", "1116", "1117", "1128", "1129", "1200", "122", "1310", "1432",
                                                                                     "34"))] <- 1985

# Impute region_city - base this off the zip code and just select the first unique 
#                       city id found
train$m_region_city <- 0
train$m_region_city[is.na(train$region_city)] <- 1
city <- subset(train, is.na(train$region_city), select =region_zip)

for (i in 1:nrow(city)) {
  
  zip <- city$region_zip[i]
  zipcities <- subset(train, !is.na(train$region_city) & train$region_zip == zip,
                      select = region_city)
  cities <- unique(zipcities)
  train$region_city[is.na(train$region_city) & train$region_zip == zip] <- cities[1,]
}

train$region_city[is.na(train$region_city) & train$region_zip == 96395] <- 11111
train$region_city[is.na(train$region_city) & train$region_zip == 96500] <- 22222


train17$m_region_city <- 0
train17$m_region_city[is.na(train17$region_city)] <- 1
city <- subset(train17, is.na(train17$region_city), select =region_zip)

for (i in 1:nrow(city)) {
  
  zip <- city$region_zip[i]
  zipcities <- subset(train17, !is.na(train17$region_city) & train17$region_zip == zip,
                      select = region_city)
  cities <- unique(zipcities)
  train17$region_city[is.na(train17$region_city) & train17$region_zip == zip] <- zipcities[1,]
}

train17$region_city[is.na(train17$region_city) & train17$region_zip == 96395] <- 11111
train17$region_city[is.na(train17$region_city) & train17$region_zip == 96500] <- 22222

#################################################################
# Ran out of geocode allowance so just use most frequent value  # 
#################################################################

train$region_city[is.na(train$region_city)] <- as.numeric(names(which.max(table(train$region_city))))
train17$region_city[is.na(train17$region_city)] <- as.numeric(names(which.max(table(train17$region_city))))

train$region_zip[is.na(train$region_zip)] <- as.numeric(names(which.max(table(train$region_zip))))
train17$region_zip[is.na(train17$region_zip)] <- as.numeric(names(which.max(table(train17$region_zip))))

tmv2()

# Impute area_live_finished
train$m_area_live_finished <- 0
train$m_area_live_finished[is.na(train$area_live_finished)] <- 1
train$area_live_finished[is.na(train$area_live_finished)
                         & train$area_total_calc < 1466] <- 1120
train$area_live_finished[is.na(train$area_live_finished) & train$area_total_calc >= 1466
                         & train$area_total_calc < 2316] <- 1810
train$area_live_finished[is.na(train$area_live_finished) & train$area_total_calc < 4101
                         & train$area_total_calc >= 2316] <- 2905
train$area_live_finished[is.na(train$area_live_finished)
                         & train$area_total_calc >= 4101] <- 5296

train17$m_area_live_finished <- 0
train17$m_area_live_finished[is.na(train17$area_live_finished)] <- 1
train17$area_live_finished[is.na(train17$area_live_finished)
                           & train17$area_total_calc < 1466] <- 1120
train17$area_live_finished[is.na(train17$area_live_finished) & train17$area_total_calc >= 1466
                           & train17$area_total_calc < 2316] <- 1810
train17$area_live_finished[is.na(train17$area_live_finished) & train17$area_total_calc < 4101
                           & train17$area_total_calc >= 2316] <- 2905
train17$area_live_finished[is.na(train17$area_live_finished)
                           & train17$area_total_calc >= 4101] <- 5296

# Impute area_lot
train$m_area_lot <- 0
train$m_area_lot[is.na(train$area_lot)] <- 1
train$area_lot[is.na(train$area_lot) & !(train$zoning_landuse_county %in%
                                           c("0109", "010C", "010F", "010G", "010M", "012C", "01DC", "020M", "0700", 
                                             "1112", "1432"))] <- 10779
train$area_lot[is.na(train$area_lot) & train$latitude < 3.431e+07
               & train$zoning_landuse_county %in% c("0109", "010C", "010F", "010G", "010M", 
                                                    "012C", "01DC", "020M", "0700", "1112", "1432")] <- 104180
train$area_lot[is.na(train$area_lot) & train$latitude >= 3.431e+07
               & train$zoning_landuse_county %in% c("0109", "010C", "010F", "010G", "010M",
                                                    "012C", "01DC", "020M", "0700", "1112", "1432")] <- 354662

train17$m_area_lot <- 0
train17$m_area_lot[is.na(train17$area_lot)] <- 1
train17$area_lot[is.na(train17$area_lot) & !(train17$zoning_landuse_county %in%
                                               c("0109", "010C", "010F", "010G", "010M", "012C", "01DC", "020M", "0700", 
                                                 "1112", "1432"))] <- 10779
train17$area_lot[is.na(train17$area_lot) & train17$latitude < 3.431e+07
                 & train17$zoning_landuse_county %in% c("0109", "010C", "010F", "010G", "010M", 
                                                        "012C", "01DC", "020M", "0700", "1112", "1432")] <- 104180
train17$area_lot[is.na(train17$area_lot) & train17$latitude >= 3.431e+07
                 & train17$zoning_landuse_county %in% c("0109", "010C", "010F", "010G", "010M",
                                                        "012C", "01DC", "020M", "0700", "1112", "1432")] <- 354662

# Impute num_unit
train$m_num_unit <- 0
train$m_num_unit[is.na(train$num_unit)] <- 1
train$num_unit[is.na(train$num_unit) & train$num_bedroom < 7.5] <- 1
train$num_unit[is.na(train$num_unit) & train$num_bedroom >= 7.5] <-4

train17$m_num_unit <- 0
train17$m_num_unit[is.na(train17$num_unit)] <- 1
train17$num_unit[is.na(train17$num_unit) & train17$num_bedroom < 7.5] <- 1
train17$num_unit[is.na(train17$num_unit) & train17$num_bedroom >= 7.5] <-4


# Zero units doesn't make sense, so set this equal to 1
train$num_unit[train$num_unit == 0] <- 1
train17$num_unit[train$num_unit == 0] <- 1


# Create houseAge variable
train$houseAge <- as.numeric(substring(train$date , 1, 4)) - train$build_year
train$build_year <- NULL

train17$houseAge <- as.numeric(substring(train17$date , 1, 4)) - train17$build_year
train17$build_year <- NULL

# Create "roll your own" data
train$mo=as.factor(substr(train$date,6,7))
train$day=as.factor(substr(train$date,9,10)) 

train17$mo=as.factor(substr(train17$date,6,7))
train17$day=as.factor(substr(train17$date,9,10)) 


# Impute quality
train$m_quality <- 0
train$m_quality[is.na(train$quality)] <- 1
train$quality[is.na(train$quality) & train$houseAge >= 58.5] <- 7
train$quality[is.na(train$quality) & train$houseAge < 58.5] <-4

train17$m_quality <- 0
train17$m_quality[is.na(train17$quality)] <- 1
train17$quality[is.na(train17$quality) & train17$houseAge >= 58.5] <- 7
train17$quality[is.na(train17$quality) & train17$houseAge < 58.5] <-4


# Impute heating
train$m_heating <- 0
train$m_heating[is.na(train$heating)] <- 1
train$heating[is.na(train$heating) & train$fips< 6048] <- 2
train$heating[is.na(train$heating) & train$fips >= 6048 & train$num_room <6.5] <- 24
train$heating[is.na(train$heating) & train$fips >= 6048 & train$num_room >=6.5] <- 6

train17$m_heating <- 0
train17$m_heating[is.na(train17$heating)] <- 1
train17$heating[is.na(train17$heating) & train17$fips< 6048] <- 2
train17$heating[is.na(train17$heating) & train17$fips >= 6048 & train17$num_room <6.5] <- 24
train17$heating[is.na(train17$heating) & train17$fips >= 6048 & train17$num_room >=6.5] <- 6

# Impute region_neighbor
train$m_region_neighbor <- 0
train$m_region_neighbor[is.na(train$region_neighbor)] <- 1
train$region_neighbor[is.na(train$region_neighbor) & train$latitude>=3.419e+07 & 
                        train$latitude< 3.427e+07 & train$longitude>=-1.185e+08] <- 41131
train$region_neighbor[is.na(train$region_neighbor) & train$latitude>=3.419e+07 & 
                        train$latitude>=3.422e+07 & train$longitude>=-1.186e+08 & train$longitudelongitude< -1.185e+08] <- 33183
train$region_neighbor[is.na(train$region_neighbor) & train$latitude< 3.408e+07 & train$longitude>=-1.182e+08] <- 113455
train$region_neighbor[is.na(train$region_neighbor) & train$latitude< 3.392e+07 & 
                        train$longitude< -1.182e+08 & train$longitude>=-1.184e+08] <- 54300
train$region_neighbor[is.na(train$region_neighbor) & train$latitude< 3.408e+07 & train$latitude>=3.403e+07 &
                        train$longitude< -1.182e+08 & train$longitude>=-1.184e+08] <- 274514
train$region_neighbor[is.na(train$region_neighbor) & train$latitude< 3.403e+07 & train$latitude>=3.392e+07 &
                        train$longitude< -1.182e+08 & train$longitude>=-1.184e+08] <- 268496
train$region_neighbor[is.na(train$region_neighbor) & train$latitude< 3.403e+07 & train$latitude>=3.392e+07 &
                        train$longitude< -1.183e+08 & train$longitude>=-1.184e+08] <- 118208
train$region_neighbor[is.na(train$region_neighbor) & train$latitude< 3.408e+07 & train$longitude< -1.184e+08] <- 118920
train$region_neighbor[is.na(train$region_neighbor) & train$latitude>=3.408e+07 & train$latitude< 3.419e+07
                      & train$longitude>=-1.183e+08] <- 275405
train$region_neighbor[is.na(train$region_neighbor) & train$latitude< 3.413e+07 & train$latitude>=3.408e+07 &
                        train$longitude< -1.183e+08 & train$longitude>=-1.185e+08] <- 274049
train$region_neighbor[is.na(train$region_neighbor) & train$latitude>=3.413e+07 & train$latitude< 3.419e+07 &
                        train$longitude>=-1.184e+08 & train$longitude< -1.183e+08] <- 47880
train$region_neighbor[is.na(train$region_neighbor) & train$latitude>=3.413e+07 & train$latitude< 3.419e+07 &
                        train$longitude< -1.184e+08 & train$longitude>=-1.185e+08] <- 27080
train$region_neighbor[is.na(train$region_neighbor) & train$latitude>=3.413e+07 & train$latitude< 3.419e+07 &
                        train$longitude< -1.187e+08] <- 46736
train$region_neighbor[is.na(train$region_neighbor) & train$latitude>=3.413e+07 & train$latitude< 3.419e+07 &
                        train$longitude>= -1.187e+08 & train$longitude< -1.186e+08] <- 48570
train$region_neighbor[is.na(train$region_neighbor) & train$latitude>=3.408e+07 & train$latitude< 3.419e+07 &
                        train$longitude>= -1.185e+08 & train$longitude< -1.185e+08] <- 51906
train$region_neighbor[is.na(train$region_neighbor) & train$latitude>=3.408e+07 & train$latitude< 3.419e+07 &
                        train$longitude>= -1.186e+08 & train$longitude< -1.185e+08] <- 47950
train$region_neighbor[is.na(train$region_neighbor) & train$latitude>=3.419e+07 & train$latitude< 3.427e+07 &
                        train$longitude< -1.186e+08] <- 268588
train$region_neighbor[is.na(train$region_neighbor) & train$latitude>=3.419e+07 & train$latitude< 3.427e+07 &
                        train$longitude< -1.185e+08 & train$longitude>=-1.186e+08] <- 40548
train$region_neighbor[is.na(train$region_neighbor) & train$latitude>=3.427e+07 & train$latitude< 3.435e+07] <- 34213
train$region_neighbor[is.na(train$region_neighbor) & train$latitude>=3.435e+07 & train$longitude< -1.185e+08] <- 48200
train$region_neighbor[is.na(train$region_neighbor) & train$latitude>=3.435e+07 & train$longitude>=-1.185e+08] <- 37739
train$region_neighbor[is.na(train$region_neighbor) & train$latitude>=3.435e+07 & train$longitude>=-1.185e+08
                      & train$longitude < -1.185e+08] <- 6952
# There were 70 left over that didn't fit into any of the rules. Setup a dummy neighborhood for these
train$region_neighbor[is.na(train$region_neighbor)] <- 11111

train17$m_region_neighbor <- 0
train17$m_region_neighbor[is.na(train17$region_neighbor)] <- 1
train17$region_neighbor[is.na(train17$region_neighbor) & train17$latitude>=3.419e+07 & 
                          train17$latitude< 3.427e+07 & train17$longitude>=-1.185e+08] <- 41131
train17$region_neighbor[is.na(train17$region_neighbor) & train17$latitude>=3.419e+07 & 
                          train17$latitude>=3.422e+07 & train17$longitude>=-1.186e+08 & train17$longitudelongitude< -1.185e+08] <- 33183
train17$region_neighbor[is.na(train17$region_neighbor) & train17$latitude< 3.408e+07 & train17$longitude>=-1.182e+08] <- 113455
train17$region_neighbor[is.na(train17$region_neighbor) & train17$latitude< 3.392e+07 & 
                          train17$longitude< -1.182e+08 & train17$longitude>=-1.184e+08] <- 54300
train17$region_neighbor[is.na(train17$region_neighbor) & train17$latitude< 3.408e+07 & train17$latitude>=3.403e+07 &
                          train17$longitude< -1.182e+08 & train17$longitude>=-1.184e+08] <- 274514
train17$region_neighbor[is.na(train17$region_neighbor) & train17$latitude< 3.403e+07 & train17$latitude>=3.392e+07 &
                          train17$longitude< -1.182e+08 & train17$longitude>=-1.184e+08] <- 268496
train17$region_neighbor[is.na(train17$region_neighbor) & train17$latitude< 3.403e+07 & train17$latitude>=3.392e+07 &
                          train17$longitude< -1.183e+08 & train17$longitude>=-1.184e+08] <- 118208
train17$region_neighbor[is.na(train17$region_neighbor) & train17$latitude< 3.408e+07 & train17$longitude< -1.184e+08] <- 118920
train17$region_neighbor[is.na(train17$region_neighbor) & train17$latitude>=3.408e+07 & train17$latitude< 3.419e+07
                        & train17$longitude>=-1.183e+08] <- 275405
train17$region_neighbor[is.na(train17$region_neighbor) & train17$latitude< 3.413e+07 & train17$latitude>=3.408e+07 &
                          train17$longitude< -1.183e+08 & train17$longitude>=-1.185e+08] <- 274049
train17$region_neighbor[is.na(train17$region_neighbor) & train17$latitude>=3.413e+07 & train17$latitude< 3.419e+07 &
                          train17$longitude>=-1.184e+08 & train17$longitude< -1.183e+08] <- 47880
train17$region_neighbor[is.na(train17$region_neighbor) & train17$latitude>=3.413e+07 & train17$latitude< 3.419e+07 &
                          train17$longitude< -1.184e+08 & train17$longitude>=-1.185e+08] <- 27080
train17$region_neighbor[is.na(train17$region_neighbor) & train17$latitude>=3.413e+07 & train17$latitude< 3.419e+07 &
                          train17$longitude< -1.187e+08] <- 46736
train17$region_neighbor[is.na(train17$region_neighbor) & train17$latitude>=3.413e+07 & train17$latitude< 3.419e+07 &
                          train17$longitude>= -1.187e+08 & train17$longitude< -1.186e+08] <- 48570
train17$region_neighbor[is.na(train17$region_neighbor) & train17$latitude>=3.408e+07 & train17$latitude< 3.419e+07 &
                          train17$longitude>= -1.185e+08 & train17$longitude< -1.185e+08] <- 51906
train17$region_neighbor[is.na(train17$region_neighbor) & train17$latitude>=3.408e+07 & train17$latitude< 3.419e+07 &
                          train17$longitude>= -1.186e+08 & train17$longitude< -1.185e+08] <- 47950
train17$region_neighbor[is.na(train17$region_neighbor) & train17$latitude>=3.419e+07 & train17$latitude< 3.427e+07 &
                          train17$longitude< -1.186e+08] <- 268588
train17$region_neighbor[is.na(train17$region_neighbor) & train17$latitude>=3.419e+07 & train17$latitude< 3.427e+07 &
                          train17$longitude< -1.185e+08 & train17$longitude>=-1.186e+08] <- 40548
train17$region_neighbor[is.na(train17$region_neighbor) & train17$latitude>=3.427e+07 & train17$latitude< 3.435e+07] <- 34213
train17$region_neighbor[is.na(train17$region_neighbor) & train17$latitude>=3.435e+07 & train17$longitude< -1.185e+08] <- 48200
train17$region_neighbor[is.na(train17$region_neighbor) & train17$latitude>=3.435e+07 & train17$longitude>=-1.185e+08] <- 37739
train17$region_neighbor[is.na(train17$region_neighbor) & train17$latitude>=3.435e+07 & train17$longitude>=-1.185e+08
                        & train17$longitude < -1.185e+08] <- 6952
# There were 70 left over that didn't fit into any of the rules. Setup a dummy neighborhood for these
train17$region_neighbor[is.na(train17$region_neighbor)] <- 11111


# View correlation matrix to narrow down the variables since the decision tree takes too long to run with all variables
corrData <- subset(train, select = c(num_bathroom, bedroomcnt, area_total_calc, area_live_finished, fips, num_bath,
                                     latitude, longitude, area_lot,#zoning_landuse_county, 
                                     rawcensustractandblock,region_city, region_county,
                                     region_zip,num_room,houseAge,tax_building, region_neighbor,
                                     tax_total, num_unit, quality, heating))

corrData <- na.omit(corrData)
head(corrData$quality, n=100)
corrplot(cor(corrData))

# Remove variables with too many missing values that also don't have strong correlations to log error. 
# Keep an imputed indicator as this may be a useful indicator
train$m_aircon <- 0
train$m_aircon[is.na(train$aircon)] <- 1
train$aircon <- NULL

train17$m_aircon <- 0
train17$m_aircon[is.na(train17$aircon)] <- 1
train17$aircon <- NULL


train$m_architectural_style <- 0
train$m_architectural_style[is.na(train$architectural_style)] <- 1
train$architectural_style <- NULL

train17$m_architectural_style <- 0
train17$m_architectural_style[is.na(train17$architectural_style)] <- 1
train17$architectural_style <- NULL

train$m_area_basement <- 0
train$m_area_basement[is.na(train$area_basement)] <- 1
train$area_basement <- NULL

train17$m_area_basement <- 0
train17$m_area_basement[is.na(train17$area_basement)] <- 1
train17$area_basement <- NULL

train$m_framing <- 0
train$m_framing[is.na(train$framing)] <- 1
train$framing <- NULL

train17$m_framing <- 0
train17$m_framing[is.na(train17$framing)] <- 1
train17$framing <- NULL

train$m_deck <- 0
train$m_deck[is.na(train$deck)] <- 1
train$deck <- NULL

train17$m_deck <- 0
train17$m_deck[is.na(train17$deck)] <- 1
train17$deck <- NULL

train$m_area_firstfloor_finished <- 0
train$m_area_firstfloor_finished[is.na(train$area_firstfloor_finished)] <- 1
train$area_firstfloor_finished <- NULL

train17$m_area_firstfloor_finished <- 0
train17$m_area_firstfloor_finished[is.na(train17$area_firstfloor_finished)] <- 1
train17$area_firstfloor_finished <- NULL

train$m_area_liveperi_finished <- 0
train$m_area_liveperi_finished[is.na(train$area_liveperi_finished)] <- 1
train$area_liveperi_finished <- NULL

train17$m_area_liveperi_finished <- 0
train17$m_area_liveperi_finished[is.na(train17$area_liveperi_finished)] <- 1
train17$area_liveperi_finished <- NULL


train$m_area_total_finished <- 0
train$m_area_total_finished[is.na(train$area_total_finished)] <- 1
train$area_total_finished <- NULL

train17$m_area_total_finished <- 0
train17$m_area_total_finished[is.na(train17$area_total_finished)] <- 1
train17$area_total_finished <- NULL


train$m_area_unknown <- 0
train$m_area_unknown[is.na(train$area_unknown)] <- 1
train$area_unknown <- NULL

train17$m_area_unknown <- 0
train17$m_area_unknown[is.na(train17$area_unknown)] <- 1
train17$area_unknown <- NULL


train$m_area_base <- 0
train$m_area_base[is.na(train$area_base)] <- 1
train$area_base <- NULL

train17$m_area_base <- 0
train17$m_area_base[is.na(train17$area_base)] <- 1
train17$area_base <- NULL


train$m_num_fireplace <- 0
train$m_num_fireplace[is.na(train$num_fireplace)] <- 1
train$num_fireplace <- NULL

train17$m_num_fireplace <- 0
train17$m_num_fireplace[is.na(train17$num_fireplace)] <- 1
train17$num_fireplace <- NULL


train$m_num_garage <- 0
train$m_num_garage[is.na(train$num_garage)] <- 1
train$num_garage <- NULL

train17$m_num_garage <- 0
train17$m_num_garage[is.na(train17$num_garage)] <- 1
train17$num_garage <- NULL


train$m_area_garage <- 0
train$m_area_garage[is.na(train$area_garage)] <- 1
train$area_garage <- NULL

train17$m_area_garage <- 0
train17$m_area_garage[is.na(train17$area_garage)] <- 1
train17$area_garage <- NULL


train$m_num_pool <- 0
train$m_num_pool[is.na(train$num_pool)] <- 1
train$num_pool <- NULL

train17$m_num_pool <- 0
train17$m_num_pool[is.na(train17$num_pool)] <- 1
train17$num_pool <- NULL


train$m_area_pool <- 0
train$m_area_pool[is.na(train$area_pool)] <- 1
train$area_pool <- NULL

train17$m_area_pool <- 0
train17$m_area_pool[is.na(train17$area_pool)] <- 1
train17$area_pool <- NULL


train$m_pooltypeid10 <- 0
train$m_pooltypeid10[is.na(train$pooltypeid10)] <- 1
train$pooltypeid10 <- NULL

train17$m_pooltypeid10 <- 0
train17$m_pooltypeid10[is.na(train17$pooltypeid10)] <- 1
train17$pooltypeid10 <- NULL


train$m_pooltypeid2 <- 0
train$m_pooltypeid2[is.na(train$pooltypeid2)] <- 1
train$pooltypeid2 <- NULL

train17$m_pooltypeid2 <- 0
train17$m_pooltypeid2[is.na(train17$pooltypeid2)] <- 1
train17$pooltypeid2 <- NULL


train$m_pooltypeid7 <- 0
train$m_pooltypeid7[is.na(train$pooltypeid7)] <- 1
train$pooltypeid7 <- NULL

train17$m_pooltypeid7 <- 0
train17$m_pooltypeid7[is.na(train17$pooltypeid7)] <- 1
train17$pooltypeid7 <- NULL


train$m_story <- 0
train$m_story[is.na(train$story)] <- 1
train$story <- NULL

train17$m_story <- 0
train17$m_story[is.na(train17$story)] <- 1
train17$story <- NULL


train$m_num_75_bath <- 0
train$m_num_75_bath[is.na(train$num_75_bath)] <- 1
train$num_75_bath <- NULL

train17$m_num_75_bath <- 0
train17$m_num_75_bath[is.na(train17$num_75_bath)] <- 1
train17$num_75_bath <- NULL


train$m_material <- 0
train$m_material[is.na(train$material)] <- 1
train$material <- NULL

train17$m_material <- 0
train17$m_material[is.na(train17$material)] <- 1
train17$material <- NULL


train$m_area_patio <- 0
train$m_area_patio[is.na(train$area_patio)] <- 1
train$area_patio <- NULL

train17$m_area_patio <- 0
train17$m_area_patio[is.na(train17$area_patio)] <- 1
train17$area_patio <- NULL


train$m_area_shed <- 0
train$m_area_shed[is.na(train$area_shed)] <- 1
train$area_shed <- NULL

train17$m_area_shed <- 0
train17$m_area_shed[is.na(train17$area_shed)] <- 1
train17$area_shed <- NULL


train$m_num_story <- 0
train$m_num_story[is.na(train$num_story)] <- 1
train$num_story <- NULL

train17$m_num_story <- 0
train17$m_num_story[is.na(train17$num_story)] <- 1
train17$num_story <- NULL

train17$m_fips <- 0
train17$m_fips[is.na(train17$fips)] <- 1
train17$fips[is.na(train17$fips)] <- as.numeric(names(which.max(table(train17$fips))))

train17$m_heating <- 0
train17$m_heating[is.na(train17$heating)] <- 1
train17$heating[is.na(train17$heating)] <- median(train17$heating[!is.na(train17$heating)])

train17$m_zoning_landuse <- 0
train17$m_zoning_landuse[is.na(train17$zoning_landuse)] <- 1
train17$zoning_landuse[is.na(train17$zoning_landuse)] <- median(train17$zoning_landuse[!is.na(train17$zoning_landuse)])


train17$m_rawcensustractandblock <- 0
train17$m_rawcensustractandblock[is.na(train17$rawcensustractandblock)] <- 1
train17$rawcensustractandblock[is.na(train17$rawcensustractandblock)] <- 
                      as.numeric(names(which.max(table(train17$rawcensustractandblock))))

train17$m_region_county <- 0
train17$m_region_county[is.na(train17$region_county)] <- 1
train17$region_county[is.na(train17$region_county)] <- 
  as.numeric(names(which.max(table(train17$region_county))))

train17$m_num_room <- 0
train17$m_num_room[is.na(train17$num_room)] <- 1
train17$num_room[is.na(train17$num_room)] <- 2

train17$m_censustractandblock <- 0
train17$m_censustractandblock[is.na(train17$censustractandblock)] <- 1
train17$censustractandblock[is.na(train17$censustractandblock)] <- 
  as.numeric(names(which.max(table(train17$censustractandblock))))



#############################################################################
# Clean up variables that have incorrect values that haven't been fixed yet #
#############################################################################

# These are all zero but the tax delinquency year is filled in for some of these
train$tax_delinquency[!is.na(train$tax_delinquency_year)] <- 1

# Delinquency year is not helpful. Better to use age of delinquency
train$m_tax_delinquency_year <- 0
train$m_tax_delinquency_year[is.na(train$tax_delinquency_year)] <- 1
train$tax_AgeDelinquency[is.na(train$tax_delinquency_year)] <- 0
train$tax_delinquency_year[train$tax_delinquency_year > 15 & !is.na(train$tax_delinquency_year)] <- 
  train$tax_delinquency_year[train$tax_delinquency_year > 15 & !is.na(train$tax_delinquency_year)] + 1900
train$tax_delinquency_year[train$tax_delinquency_year <= 15 & !is.na(train$tax_delinquency_year)] <- 
  train$tax_delinquency_year[train$tax_delinquency_year <= 15 & !is.na(train$tax_delinquency_year)] + 2000
train$tax_AgeDelinquency[!is.na(train$tax_delinquency_year)] <- 
  2016 - train$tax_delinquency_year[!is.na(train$tax_delinquency_year)]
train$tax_delinquency_year <- NULL


train17$tax_delinquency[!is.na(train17$tax_delinquency_year)] <- 1

# Delinquency year is not helpful. Better to use age of delinquency
train17$m_tax_delinquency_year <- 0
train17$m_tax_delinquency_year[is.na(train17$tax_delinquency_year)] <- 1
train17$tax_AgeDelinquency[is.na(train17$tax_delinquency_year)] <- 0
train17$tax_delinquency_year[train17$tax_delinquency_year > 15 & !is.na(train17$tax_delinquency_year)] <- 
  train17$tax_delinquency_year[train17$tax_delinquency_year > 15 & !is.na(train17$tax_delinquency_year)] + 1900
train17$tax_delinquency_year[train17$tax_delinquency_year <= 15 & !is.na(train17$tax_delinquency_year)] <- 
  train17$tax_delinquency_year[train17$tax_delinquency_year <= 15 & !is.na(train17$tax_delinquency_year)] + 2000
train17$tax_AgeDelinquency[!is.na(train17$tax_delinquency_year)] <- 
  2017 - train17$tax_delinquency_year[!is.na(train17$tax_delinquency_year)]
train17$tax_delinquency_year <- NULL


tmv()
tmv2()

###############################################
# Correlation plots of remaining imputed data #
###############################################

train$abs_logerror <- abs(logerror)
summary(train)

missing_values <- train %>% summarize_each(funs(sum(is.na(.))/n()))
missing_values <- gather(missing_values, key="feature", value="missing_pct")
good_features <- filter(missing_values, missing_pct<0.75)
vars <- good_features$feature[str_detect(good_features$feature,'num_')]
tmp <- train %>% select(dpone_of(c(vars,"abs_logerror")))

tmp2 <- subset(tmp, tmp$m_num_unit == 0 & tmp$m_num_garage == 0)

# Good correlations: num_baththroom_calc, num_bath and num_garage
# Weaker correlations: num_room and num_unit
corrplot(cor(tmp, use="complete.obs"),type="lower")

# When the null values for num_unit are added back in the correlations with the logerror come back
# there is some sort of interaction here
# Don't include m_num_unit in graph as it is all one value
tmp2$m_num_unit <- NULL
tmp2$m_num_garage <- NULL
corrplot(cor(tmp2, use="complete.obs"),type="lower")

# Correlation plot of variables starting with tax
missing_values <- train %>% summarize_each(funs(sum(is.na(.))/n()))
missing_values <- gather(missing_values, key="feature", value="missing_pct")
good_features <- filter(missing_values, missing_pct<0.75)
vars <- good_features$feature[str_detect(good_features$feature,'tax_')]
tmp <- train %>% select(one_of(c(vars,"abs_logerror")))

# Extremely faint correlations - tax data is not promising
corrplot(cor(tmp, use="complete.obs"),type="lower")


# Correlation plot of variables starting with area
missing_values <- train %>% summarize_each(funs(sum(is.na(.))/n()))
missing_values <- gather(missing_values, key="feature", value="missing_pct")
good_features <- filter(missing_values, missing_pct<0.75)
vars <- good_features$feature[str_detect(good_features$feature,'area_')]
tmp <- train %>% select(one_of(c(vars,"abs_logerror")))

# Extremely faint correlations - tax data is not promising
corrplot(cor(tmp, use="complete.obs"),type="lower")


vars <- c("houseAge", "flag_fireplace", "flag_tub")
tmp <- train %>% select(one_of(c(vars,"abs_logerror")))

# Extremely faint correlations - tax data is not promising
corrplot(cor(tmp, use="complete.obs"),type="lower")

# Do a scatterplot for houseAge against abs_logerror
par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1))
plot(train$houseAge, train$abs_logerror, main="Scatterplot Example", 
     xlab="House Age ", ylab="Absolute Log Error", pch=19)


##################################
# Boxplots for region_ variables #
##################################

# Box plots for region_county for both logerror and abs_logerror
# region 3101 has more outliers on each end than the other regions
regionCounty <- ggplot(aes(y = logerror, x = reorder(region_county, logerror, FUN = median)), data = train) + geom_boxplot(fill = "#3366FF")
regionCounty + theme(axis.text.x = element_text(angle = 90, hjust = 1))

regionCounty <- ggplot(aes(y = abs_logerror, x = reorder(region_county, abs_logerror, FUN = median)), data = train) + geom_boxplot(fill = "#3366FF")
regionCounty + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Box plots for region_city for both logerror and abs_logerror
# It looks like 11626 has more outliers than the others and 24797 has a much higher median. This looks more promising than region
regionCity <- ggplot(aes(y = logerror, x = reorder(region_city, logerror, FUN = median)), data = train) + geom_boxplot(fill = "#3366FF")
regionCity + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# TO DO: maybe collapse this into into an 8-10 category region city based on max absolute log error
regionCity <- ggplot(aes(y = abs_logerror, x = reorder(region_city, abs_logerror, FUN = max)), data = train) + geom_boxplot(fill = "#3366FF")
regionCity + theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Box plots for region_zip for both logerror and abs_logerror
# region 3101 has more outliers on each end than the other regions
regionZip <- ggplot(aes(y = logerror, x = reorder(region_zip, logerror, FUN = median)), data = train) + geom_boxplot(fill = "#3366FF")
regionZip + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# It looks like this could also be predictive but will be more work to collapse
regionZip <- ggplot(aes(y = abs_logerror, x = reorder(region_zip, abs_logerror, FUN = max)), data = train) + geom_boxplot(fill = "#3366FF")
regionZip + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# box plots for region_neighbor - this could also be predictive but will take more work to collapse
regionNeigh <- ggplot(aes(y = logerror, x = reorder(region_neighbor, logerror, FUN = median)), data = train) + geom_boxplot(fill = "#3366FF")
regionNeigh + theme(axis.text.x = element_text(angle = 90, hjust = 1))

regionNeigh <- ggplot(aes(y = abs_logerror, x = reorder(region_neighbor, abs_logerror, FUN = median)), data = train) + geom_boxplot(fill = "#3366FF")
regionNeigh + theme(axis.text.x = element_text(angle = 90, hjust = 1))

##################################
# Boxplots for zoning_ variables #
##################################

# box plots for zoning property- this could also be predictive but will take more work to collapse
zoningProp <- ggplot(aes(y = logerror, x = reorder(zoning_property, logerror, FUN = median)), data = train) + geom_boxplot(fill = "#3366FF")
zoningProp + theme(axis.text.x = element_text(angle = 90, hjust = 1))

zoningProp <- ggplot(aes(y = abs_logerror, x = reorder(zoning_property, abs_logerror, FUN = max)), data = train) + geom_boxplot(fill = "#3366FF")
zoningProp + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# box plots for zoning landuse - there area 14 categories - can leave as is or collapse it
zoningLU <- ggplot(aes(y = logerror, x = reorder(zoning_landuse, logerror, FUN = median)), data = train) + geom_boxplot(fill = "#3366FF")
zoningLU + theme(axis.text.x = element_text(angle = 90, hjust = 1))

zoningLU <- ggplot(aes(y = abs_logerror, x = reorder(zoning_landuse, abs_logerror, FUN = max)), data = train) + geom_boxplot(fill = "#3366FF")
zoningLU + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# box plots for zoning landuse county - would need to collapse this into about 9-10 categories
zoningLUC <- ggplot(aes(y = logerror, x = reorder(zoning_landuse_county, logerror, FUN = median)), data = train) + geom_boxplot(fill = "#3366FF")
zoningLUC + theme(axis.text.x = element_text(angle = 90, hjust = 1))

zoningLUC <- ggplot(aes(y = abs_logerror, x = reorder(zoning_landuse_county, abs_logerror, FUN = max)), data = train) + geom_boxplot(fill = "#3366FF")
zoningLUC + theme(axis.text.x = element_text(angle = 90, hjust = 1))

######################################################
# Boxplots for the rest of the categorical variables #
######################################################


# Some categories of quality have a lot of outliers and some don't. Could be a predictor
materialPlot <- ggplot(aes(y = logerror, x = reorder(material, logerror, FUN = median)), data = train) + geom_boxplot(fill = "#3366FF")
materialPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1))

materialPlot <- ggplot(aes(y = abs_logerror, x = reorder(material, abs_logerror, FUN = max)), data = train) + geom_boxplot(fill = "#3366FF")
materialPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Some categories of heating have a lot of outliers and some don't. Could be a predictor
heatingPlot <- ggplot(aes(y = logerror, x = reorder(heating, logerror, FUN = median)), data = train) + geom_boxplot(fill = "#3366FF")
heatingPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1))

heatingPlot <- ggplot(aes(y = abs_logerror, x = reorder(heating, abs_logerror, FUN = max)), data = train) + geom_boxplot(fill = "#3366FF")
heatingPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# month -- come months have larger outliers
monthPlot <- ggplot(aes(y = logerror, x = reorder(mo, logerror, FUN = median)), data = train) + geom_boxplot(fill = "#3366FF")
monthPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1))

monthPlot <- ggplot(aes(y = abs_logerror, x = reorder(mo, abs_logerror, FUN = max)), data = train) + geom_boxplot(fill = "#3366FF")
monthPlot + theme(axis.text.x = element_text(angle = 90, hjust = 1))


# day of month
dayPlot <- ggplot(aes(y = logerror, x = reorder(day, logerror, FUN = median)), data = train) + geom_boxplot(fill = "#3366FF")
dayPlot  + theme(axis.text.x = element_text(angle = 90, hjust = 1))

dayPlot  <- ggplot(aes(y = abs_logerror, x = reorder(day, abs_logerror, FUN = max)), data = train) + geom_boxplot(fill = "#3366FF")
dayPlot  + theme(axis.text.x = element_text(angle = 90, hjust = 1))

###############
# Buid Models #
###############

########
# MLR  #
########

summary(train)

str(train$mo)
train$mo <- as.integer(train$mo)
train$day <- as.integer(train$day)
trainMLR <- subset(train, select = c(logerror, area_total_calc, num_bathroom, num_bath, num_bedroom,
                                     tax_AgeDelinquency, m_tax_building, tax_building, houseAge, censustractandblock, 
                                     m_censustractandblock, m_area_base, m_area_firstfloor_finished,
                                      m_area_lot, m_num_pool, quality, m_area_live_finished, rawcensustractandblock,
                                     fips, m_pooltypeid7, m_area_total_calc, mo, day, m_area_basement,  tax_property, 
                                     tax_total, region_zip, m_tax_land))

# Adjusted R-squared:  0.00301  
lm16.out <- lm(logerror ~ ., data = trainMLR)
summary(lm16.out)

#   0.06829868
mae(fitted(lm16.out), train$logerror)


train17$mo <- as.integer(train17$mo)
train17$day <- as.integer(train17$day)
trainMLR17 <- subset(train17, select = c(logerror, area_total_calc, area_live_finished, num_bedroom, num_bathroom,  
                                         num_bath,  tax_AgeDelinquency, censustractandblock, m_censustractandblock, m_tax_land,
                                         region_county, quality, longitude, m_tax_delinquency_year, tax_delinquency, m_quality, 
                                         m_num_unit, num_room, m_area_garage, m_num_garage, m_region_neighbor, m_num_story, m_fips, 
                                         m_heating, m_latitude, m_longitude, m_num_bathroom, mo, day, m_num_bedroom, m_num_room,
                                         m_region_county,  m_tax_total, m_num_bath, m_tax_property, area_lot))

# Adjusted R-squared:  0.004329  
lm17.out <- lm(logerror ~ ., data = trainMLR17)
summary(lm17.out)

# 0.07071855
mae(fitted(lm17.out), train17$logerror)


#############################
# Boosted Regression Model  #
#############################

train$date <- as.factor(train$date)

# gbm does not currently handle categorical variables with more than 1024 levels. Variable 1: zoning_property has 5639 levels.
# zoning_property 

# num_bathroom num_bedroom num_bathroom_calc num_bath num_room m_num_story m_area_live_finished m_area_total_finished
# m_area_garage houseAge tax_total m_tax_total censustractandblock  region_zip m_region_zip 
# zoning_landuse_county m_deck m_num_bath longitude date m_heating m_num_bathroom_calc m_tax_building tax_building
# m_num_75_bath region_county fips rawcensustractandblock m_tax_land m_tax_property area_total_calc m_region_city
# m_region_neighbor m_aircon m_architectural_style m_area_basement m_area_firstfloor_finished m_area_liveperi_finished
# m_area_unknown m_area_base m_area_pool m_story m_material m_area_patio m_area_shed num_bedroom region_city
#   
set.seed(1) 

gbm16.out <- gbm(logerror ~  tax_AgeDelinquency + day  + m_pooltypeid10 + area_total_calc +
                 m_num_garage + m_num_unit  + num_unit + m_num_fireplace + region_neighbor + mo + area_live_finished +
                 m_censustractandblock + quality + m_framing + m_num_pool + m_quality + m_area_lot + area_lot + 
                 m_area_total_calc + m_build_year + m_pooltypeid2 + m_pooltypeid7  
               , distribution = "gaussian", interaction.depth = 5, bag.fraction = .5,
               n.trees = 1000, shrinkage = 0.001, data = train)

# View gbm model
#str(gbm.out)

train.pred <- predict.gbm(gbm16.out, n.trees = 1000)

# 0.06818651
mae(train.pred, train$logerror)

# num_bedroom num_bathroom_calc m_num_story m_area_live_finished m_area_total_finished m_area_garage m_tax_total fips
# censustractandblock region_zip m_region_zip zoning_landuse_county m_deck m_num_bath longitude date m_heating m_num_bathroom_calc
# m_tax_building region_county rawcensustractandblock m_tax_land area_total_calc m_region_city m_region_neighbor m_aircon
# m_architectural_style m_area_basement m_area_firstfloor_finished  m_area_liveperi_finished m_area_unknown m_area_pool
# m_story m_material m_area_patio m_area_shed num_bedroom region_city
 
set.seed(1) 

gbm17.out <- gbm(logerror ~  tax_AgeDelinquency + day  + m_pooltypeid10 + area_total_calc + num_bath + num_room 
                 + m_num_garage + m_num_unit  + num_unit + m_num_fireplace + region_neighbor + mo + area_live_finished 
                 + m_censustractandblock + quality + m_framing + m_num_pool + m_quality + m_area_lot + area_lot + 
                  m_area_total_calc + m_build_year + m_pooltypeid2 + m_pooltypeid7  + num_bathroom + houseAge
                 + tax_total + tax_building + m_num_75_bath + m_tax_property + m_area_base 
               , distribution = "gaussian", interaction.depth = 5, bag.fraction = .5,
               n.trees = 1000, shrinkage = 0.001, data = train17)

# View gbm model
#str(gbm.out)

train17.pred <- predict.gbm(gbm17.out, n.trees = 1000)

#  0.07031778
mae(train17.pred, train17$logerror)

################################
# Boosted Regression Model 2   #
################################

train$date <- as.factor(train$date)

# gbm does not currently handle categorical variables with more than 1024 levels. Variable 1: zoning_property has 5639 levels.
# zoning_property 

set.seed(5) 

gbm16.out <- gbm(logerror ~  tax_AgeDelinquency + day  + m_pooltypeid10 + area_total_calc +
                   m_num_garage + m_num_unit  + num_unit + m_num_fireplace + region_neighbor + mo + area_live_finished +
                   m_censustractandblock + quality + m_framing + m_num_pool + m_quality + m_area_lot + area_lot + 
                   m_area_total_calc + m_build_year + m_pooltypeid2 + m_pooltypeid7  
                 , distribution = "gaussian", interaction.depth = 10, bag.fraction = .5,
                 n.trees = 1200, shrinkage = 0.001, data = train)

# View gbm model
#str(gbm.out)

train.pred <- predict.gbm(gbm16.out, n.trees = 1200)

# 0.06818651
# Beat previous with this: 0.06806708
mae(train.pred, train$logerror)


set.seed(5) 

gbm17.out <- gbm(logerror ~  tax_AgeDelinquency + day  + m_pooltypeid10 + area_total_calc + num_bath + num_room 
                 + m_num_garage + m_num_unit  + num_unit + m_num_fireplace + region_neighbor + mo + area_live_finished 
                 + m_censustractandblock + quality + m_framing + m_num_pool + m_quality + m_area_lot + area_lot + 
                   m_area_total_calc + m_build_year + m_pooltypeid2 + m_pooltypeid7  + num_bathroom + houseAge
                 + tax_total + tax_building + m_num_75_bath + m_tax_property + m_area_base 
                 , distribution = "gaussian", interaction.depth = 10, bag.fraction = .5,
                 n.trees = 1200, shrinkage = 0.001, data = train17)

# View gbm model
#str(gbm.out)

train17.pred <- predict.gbm(gbm17.out, n.trees = 1200)

#  0.07031778
mae(train17.pred, train17$logerror)

################################
# Boosted Regression Model 4   #
################################

set.seed(5) 

gbm16.out <- gbm(logerror ~  m_pooltypeid10 + m_num_garage + m_num_unit  + m_num_fireplace + mo + day +
                   m_censustractandblock + m_framing + m_num_pool + m_quality + m_area_lot + m_area_total_calc + 
                   m_build_year + m_pooltypeid2 + m_pooltypeid7 + m_num_story + m_area_live_finished + 
                   m_area_total_finished + m_area_garage +m_tax_total + m_region_zip + m_deck + m_num_bath +
                   m_heating + m_num_bathroom_calc +  m_tax_building + m_num_75_bath + m_tax_land + m_tax_property + 
                   m_region_city +  m_region_neighbor + m_aircon + m_architectural_style + m_area_basement + 
                   m_area_firstfloor_finished + m_area_liveperi_finished + m_area_unknown + m_area_base + m_area_pool + 
                   m_story + m_material + m_area_patio + m_area_shed
                 , distribution = "gaussian", interaction.depth = 10, bag.fraction = .5,
                 n.trees = 1200, shrinkage = 0.001, data = train)

# View gbm model
#str(gbm.out)

train.pred <- predict.gbm(gbm16.out, n.trees = 1200)

# 0.06830188
mae(train.pred, train$logerror)


set.seed(5) 

gbm17.out <- gbm(logerror ~ m_pooltypeid10 + m_num_garage + m_num_unit  + m_num_fireplace + mo + day +
                   m_censustractandblock + m_framing + m_num_pool + m_quality + m_area_lot + m_area_total_calc + 
                   m_build_year + m_pooltypeid2 + m_pooltypeid7 + m_num_story + m_area_live_finished + 
                   m_area_total_finished + m_area_garage +m_tax_total + m_region_zip + m_deck + m_num_bath +
                   m_heating + m_num_bathroom_calc +  m_tax_building + m_num_75_bath + m_tax_land + m_tax_property + 
                   m_region_city +  m_region_neighbor + m_aircon + m_architectural_style + m_area_basement + 
                   m_area_firstfloor_finished + m_area_liveperi_finished + m_area_unknown + m_area_base + m_area_pool + 
                   m_story + m_material + m_area_patio + m_area_shed
                 , distribution = "gaussian", interaction.depth = 10, bag.fraction = .5,
                 n.trees = 1200, shrinkage = 0.001, data = train17)

# View gbm model
#str(gbm.out)

train17.pred <- predict.gbm(gbm17.out, n.trees = 1200)

#  0.07052693
mae(train17.pred, train17$logerror)


####################
# Arima with xreg  #
####################

# First create output files to input into Angoss to get variable importance
write.csv(train, file = "D:/Kim MSPA/Predict 413/Midterm - Zillow Kaggle/train.csv", row.names = FALSE)
write.csv(train17, file = "D:/Kim MSPA/Predict 413/Midterm - Zillow Kaggle/train17.csv", row.names = FALSE)

# Create datasets and ts dataset for the train (2016) data
# order the data by transaction date
newdata=train[order(train$date),]

# make a data table with transactiondate as the key column
newdata2=data.table(newdata,key="date")

#build whatever variables needed
mydata=newdata2[,list(date=date, logerror=mean(logerror), houseAge=mean(na.omit(houseAge)), 
                area_total_calc=mean(na.omit(area_total_calc)), area_live_finished=mean(na.omit(area_live_finished)),
                num_bathroom=mean(na.omit(num_bathroom)), num_bathroom_calc=mean(na.omit(num_bathroom_calc)),
                num_bath=mean(na.omit(num_bath)), num_bedroom=mean(na.omit(num_bedroom)), 
                tax_AgeDelinquency=mean(na.omit(tax_AgeDelinquency)), m_tax_building=mean(na.omit(m_tax_building)),
                tax_building=mean(na.omit(tax_building))), by=date] 

myts=ts(mydata$logerror, frequency =1, start=c(2016,1,1))
str(myts)

# regression elements
xregt <- subset(mydata, select= c(houseAge, area_total_calc, area_live_finished,num_bathroom,num_bath,
                                 num_bedroom, tax_AgeDelinquency, m_tax_building,tax_building))

# xregt is rank deficient -- need to find the problem and remove one of the variables
findLinearCombos(xregt)

#include regression elements with ARIMA elements
myarima = auto.arima(myts, xreg=xregt)
myarima111 = Arima(myts, order=c(1,1,1), xreg=xregt)
 myarima$coef

head(xregt)
mydata$logerror[1]
myarima$fitted[1]
myarima$residuals[1]
head(mydata)

ModelData <- cbind(xregt)
ModelData$logerror <- mydata$logerror
ModelData$residuals <- myarima$residuals
ModelData$fitted <- myarima$fitted

head(ModelData)
write.csv(ModelData, file = "D:/Kim MSPA/Predict 413/Midterm - Zillow Kaggle/arimaDataS.csv", row.names = FALSE)

# remove this -- eventually
forecast2016 <- forecast(myarima, xreg=xregt)
# points 2369, 2399, 2430
forecast2016$mean[1]



# Now do the same for the train17 (2017) data
# order the data by transaction date
newdata17=train17[order(train17$date),]

# make a data table with transactiondate as the key column
newdata2=data.table(newdata17,key="date")

#build whatever variables needed
mydata17=newdata2[,list(date=date, logerror=mean(logerror), area_total_calc=mean(na.omit(area_total_calc)), 
                      area_live_finished=mean(na.omit(area_live_finished)), num_bedroom=mean(na.omit(num_bedroom)), 
                      num_bathroom=mean(na.omit(num_bathroom)), num_bathroom_calc=mean(na.omit(num_bathroom_calc)),
                      num_bath=mean(na.omit(num_bath)), tax_AgeDelinquency=mean(na.omit(tax_AgeDelinquency)),
                      m_tax_land=mean(na.omit(m_tax_land)), quality=mean(na.omit(quality)),
                      longitude=mean(na.omit(longitude))), by=date] 

myts17=ts(mydata17$logerror, frequency =1, start=c(2017,1,1))

xregt17 <- subset(mydata17, select= c(area_total_calc, area_live_finished, num_bedroom, num_bathroom_calc, 
                                    num_bath, tax_AgeDelinquency, quality, m_tax_land, longitude))

# xregt is rank deficient -- need to find the problem and remove one of the variables
findLinearCombos(xregt17)

#include regression elements with ARIMA elements
myarima17 = auto.arima(myts17, xreg=xregt17)

plot(forecast(myarima17, xreg=xregt17))



##################
# Neural Network #
##################

#0.06842866
train$mo=as.integer(substr(train$date,6,7))
train$day=as.integer(substr(train$date,9,10)) 
ANNModel <- avNNet(logerror ~ area_total_calc + area_live_finished + num_bathroom + num_bathroom_calc + num_bath
                   + num_bedroom + tax_AgeDelinquency + m_tax_building + tax_building + m_tax_delinquency_year
                   + tax_delinquency + houseAge + censustractandblock + m_censustractandblock + m_area_base
                   + m_area_firstfloor_finished + m_area_unknown + m_area_lot + m_num_pool + quality
                   + m_area_live_finished + rawcensustractandblock + fips + m_pooltypeid7 + m_area_total_calc
                   + mo + day + m_area_basement + m_story + m_aircon + tax_property + tax_total + region_zip 
                   + m_tax_land + m_tax_total + m_region_neighbor, 
                 data=train, MaxNWts = 30000,repeats=25, size=5, decay=0.1, linout=TRUE)

summary(ANNModel)


ANN.Pred <- predict(ANNModel, train)
mae(train$logerror, ANN.Pred)


# 0.07074788
train17$mo=as.integer(substr(train17$date,6,7))
train17$day=as.integer(substr(train17$date,9,10)) 

ANNModel17 <- avNNet(logerror ~ area_total_calc + area_live_finished + num_bedroom + num_bathroom + num_bathroom_calc 
                     + num_bath + tax_AgeDelinquency + censustractandblock  + m_tax_land + region_county + quality 
                     + longitude + m_tax_delinquency_year + tax_delinquency + m_quality + m_num_unit + num_room 
                     + m_area_garage + m_num_garage + m_region_neighbor + m_num_story + m_censustractandblock 
                     + m_fips + m_heating + m_latitude + m_longitude + m_num_bathroom + mo + day + m_num_bedroom
                     + m_num_room + m_rawcensustractandblock + m_region_county + m_zoning_landuse + m_tax_total
                     + m_num_75_bath + latitude + m_num_bath + m_num_bathroom_calc + m_tax_property + area_lot
                     + heating + m_pooltypeid7 + m_tax_building + m_num_pool, 
                   data=train17, MaxNWts = 30000, repeats=25, size=5, decay=0.1, linout=TRUE)

summary(ANNModel17)

ANN.Pred17 <- predict(ANNModel17, train17)
mae(train17$logerror, ANN.Pred17)


############
# XGBoost  #
###########

train$mo <- as.integer(train$mo)
train$day <- as.integer(train$day)

# num_bedroom m_area_live_finished m_area_total_finished m_area_garage m_tax_total fips region_zip m_region_zip
# zoning_landuse_county m_deck m_num_bath m_heating m_num_bathroom_calc m_tax_building region_county m_tax_land
# rawcensustractandblock m_region_city m_region_neighbor m_aircon m_architectural_style m_area_basement
# m_area_firstfloor_finished m_area_liveperi_finished m_area_unknown m_area_pool m_story m_material region_city

columns_pred <- c("area_total_calc", "m_area_total_calc", "day","mo","num_bathroom", "num_bath", "num_bedroom",
                 "m_area_base", "m_area_live_finished", "tax_AgeDelinquency", "quality", "m_pooltypeid7",
                 "m_tax_building", "tax_building", "houseAge",  "m_area_firstfloor_finished", "region_zip",  
                 "censustractandblock", "m_censustractandblock", "m_area_lot", "m_num_pool","rawcensustractandblock",
                 "fips", "m_area_basement", "tax_property", "tax_total", "m_tax_land", "area_lot","m_build_year",
                 "m_pooltypeid10","zoning_property", "m_framing", "m_quality","m_pooltypeid2","longitude",
                 "m_area_shed")

df=as.data.frame(train)[,names(train) %in% columns_pred  ]
df = data.matrix(df)
target =as.data.frame(train)$logerror

param <- list(objective = "reg:linear",
              eval_metric = "rmse",
              max_depth = 10,
              eta = 0.0143,
              gamma = 0.0194,
              subsample = 0.649,
              colsample_bytree = 0.681,
              min_child_weight = 500,
              max_delta_step = 5,
              lamdba="L2"
)
cv.nround = 500
seed.number = 5630
set.seed(seed.number)

#model development
xgb = xgboost(data=data.matrix(df), 
              label =target,
              params=param, 
              nrounds=cv.nround,
              nthread=2,
              p_seed=seed.number,
              verbose=0)

xgb.pred <- predict(xgb,data.matrix(df))
#  0.06736966
mae(train$logerror, xgb.pred)




columns_pred17 <- c("tax_AgeDelinquency","day","m_pooltypeid10","area_total_calc","num_bath",
                    "num_room","m_num_garage","m_num_unit","num_unit", "m_num_fireplace",
                    "region_neighbor","mo","area_live_finished","quality","m_framing","m_num_pool",
                    "m_quality","m_area_lot","area_lot","m_area_total_calc","m_build_year","m_pooltypeid2",
                    "m_pooltypeid7","num_bathroom","houseAge","tax_total","tax_building","m_num_75_bath",
                    "m_tax_property","m_area_base")

df17=as.data.frame(train17)[,names(train17) %in% columns_pred17]


df17 = data.matrix(df17)

target17 =as.data.frame(train17)$logerror

#model development
xgb17 = xgboost(data=data.matrix(df17), 
              label =target17,
              params=param, 
              nrounds=cv.nround,
              nthread=2,
              p_seed=seed.number,
              verbose=0)

xgb.pred17 <- predict(xgb17,data.matrix(df17))

# 0.06997829
mae(train17$logerror, xgb.pred17)

##############
# XGBoost 2  #
##############

train$mo <- as.integer(train$mo)
train$day <- as.integer(train$day)

# num_bedroom m_area_live_finished m_area_total_finished m_area_garage m_tax_total fips region_zip m_region_zip
# zoning_landuse_county m_deck m_num_bath m_heating m_num_bathroom_calc m_tax_building region_county m_tax_land
# rawcensustractandblock m_region_city m_region_neighbor m_aircon m_architectural_style m_area_basement
# m_area_firstfloor_finished m_area_liveperi_finished m_area_unknown m_area_pool m_story m_material region_city

columns_pred <- c("area_total_calc", "m_area_total_calc", "day","mo","num_bathroom", "num_bath", "num_bedroom",
                  "m_area_base", "m_area_live_finished", "tax_AgeDelinquency", "quality", "m_pooltypeid7",
                  "m_tax_building", "tax_building", "houseAge",  "m_area_firstfloor_finished", "region_zip",  
                  "censustractandblock", "m_censustractandblock", "m_area_lot", "m_num_pool","rawcensustractandblock",
                  "fips", "m_area_basement", "tax_property", "tax_total", "m_tax_land", "area_lot","m_build_year",
                  "m_pooltypeid10","zoning_property", "m_framing", "m_quality","m_pooltypeid2","longitude",
                  "m_area_shed")

train.copy <-  cbind(train)
train.copy$abs_logerror <- NULL
train.copy$logerror <- NULL
train.copy$date <- NULL
train.copy$id_parcel <- NULL


df=as.data.frame(train.copy)#[,names(train) %in% columns_pred  ]
df = data.matrix(df)
target =as.data.frame(train)$logerror

head(df)

param <- list(objective = "reg:linear",
              eval_metric = "rmse",
              max_depth = 20,
              eta = 0.0143,
              gamma = 0.0194,
              subsample = 0.649,
              colsample_bytree = 0.681,
              min_child_weight = 500,
              max_delta_step = 5,
              lamdba="L2"
)
cv.nround = 500
seed.number = 5630
set.seed(seed.number)

#model development
xgb = xgboost(data=data.matrix(df), 
              label =target,
              params=param, 
              nrounds=cv.nround,
              nthread=4,
              p_seed=seed.number,
              verbose=0)

xgb.pred <- predict(xgb,data.matrix(df))
#   0.06667366
mae(train$logerror, xgb.pred)




columns_pred17 <- c("tax_AgeDelinquency","day","m_pooltypeid10","area_total_calc","num_bath",
                    "num_room","m_num_garage","m_num_unit","num_unit", "m_num_fireplace",
                    "region_neighbor","mo","area_live_finished","quality","m_framing","m_num_pool",
                    "m_quality","m_area_lot","area_lot","m_area_total_calc","m_build_year","m_pooltypeid2",
                    "m_pooltypeid7","num_bathroom","houseAge","tax_total","tax_building","m_num_75_bath",
                    "m_tax_property","m_area_base")

#df17=as.data.frame(train17)[,names(train17) %in% columns_pred17]

train.copy <-  cbind(train17)
train.copy$abs_logerror <- NULL
train.copy$logerror <- NULL
train.copy$date <- NULL
train.copy$id_parcel <- NULL


df17=as.data.frame(train.copy)#[,names(train) %in% columns_pred  ]
df17 = data.matrix(df17)
target17 =as.data.frame(train17)$logerror


#model development
xgb17 = xgboost(data=data.matrix(df17), 
                label =target17,
                params=param, 
                nrounds=cv.nround,
                nthread=2,
                p_seed=seed.number,
                verbose=0)

xgb.pred17 <- predict(xgb17,data.matrix(df17))

# 0.06997829
mae(train17$logerror, xgb.pred17)

##########################################################################################
# Whatever was done to the properties_2016 data set must be done to the test data set #
##########################################################################################

###########################################################
# Some variables can be removed as they don not add value #
###########################################################


properties_2016$tax_year <- NULL
properties_2016$flag_fireplace <- NULL
properties_2016$flag_tub <- NULL

# There is only one value
properties_2017$tax_year <- NULL
properties_2017$flag_fireplace <- NULL
properties_2017$flag_tub <- NULL


####################################################
# Initial correlation plots for numeric variables  #
####################################################

# Pull out the variables with too many missing values 
# Correlation plot of variables starting with num_
missing_values <- properties_2016 %>% summarize_each(funs(sum(is.na(.))/n()))
missing_values <- gather(missing_values, key="feature", value="missing_pct")
good_features <- filter(missing_values, missing_pct<0.75)
vars <- good_features$feature[str_detect(good_features$feature,'num_')]
tmp <- properties_2016 %>% select(one_of(c(vars,"abs_logerror")))

# Good correlations: num_baththroom_calc, num_bath and num_garage
# Weaker correlations: num_room and num_unit
corrplot(cor(tmp, use="complete.obs"),type="lower")

# Correlation plot of variables starting with tax
vars <- good_features$feature[str_detect(good_features$feature,'tax_')]
tmp <- properties_2016 %>% select(one_of(c(vars,"abs_logerror")))

# No correlations
corrplot(cor(tmp, use="complete.obs"),type="lower")

# Correlation plot of variables starting with area
vars <- good_features$feature[str_detect(good_features$feature,'area_')]
tmp <- properties_2016 %>% select(one_of(c(vars,"abs_logerror")))

# No correlations
corrplot(cor(tmp, use="complete.obs"),type="lower")

# list of missing values 
tmv()

# number of rows to see if some data points should just be dropped
nrow(properties_2016) #90275


# Do the same as above for 2017
missing_values <- properties_2017 %>% summarize_each(funs(sum(is.na(.))/n()))
missing_values <- gather(missing_values, key="feature", value="missing_pct")
good_features <- filter(missing_values, missing_pct<0.75)
vars <- good_features$feature[str_detect(good_features$feature,'num_')]
tmp <- properties_2017 %>% select(one_of(c(vars,"abs_logerror")))

# Good correlations: num_baththroom_calc, num_bath and num_garage
# Weaker correlations: num_room and num_unit
corrplot(cor(tmp, use="complete.obs"),type="lower")

# Correlation plot of variables starting with tax
vars <- good_features$feature[str_detect(good_features$feature,'tax_')]
tmp <- properties_2017 %>% select(one_of(c(vars,"abs_logerror")))

# No correlations
corrplot(cor(tmp, use="complete.obs"),type="lower")

# Correlation plot of variables starting with area
vars <- good_features$feature[str_detect(good_features$feature,'area_')]
tmp <- properties_2017 %>% select(one_of(c(vars,"abs_logerror")))

# No correlations
corrplot(cor(tmp, use="complete.obs"),type="lower")

#rattle()

####################################################
# Impute Missing Values using decision tree rules  #
####################################################
summary(properties_2016)
# Impute tax_total
properties_2016$m_tax_total <- 0
properties_2016$m_tax_total[is.na(properties_2016$tax_total)] <- 1
properties_2016$tax_total[is.na(properties_2016$tax_total)] <- 168709

properties_2017$m_tax_total <- 0
properties_2017$m_tax_total[is.na(properties_2017$tax_total)] <- 1
properties_2017$tax_total[is.na(properties_2017$tax_total)] <- 0

# Impute tax_land
properties_2016$m_tax_land <- 0
properties_2016$m_tax_land[is.na(properties_2016$tax_land)] <- 1
properties_2016$tax_land[is.na(properties_2016$tax_land)] <- 74070

properties_2017$m_tax_land <- 0
properties_2017$m_tax_land[is.na(properties_2017$tax_land)] <- 1
properties_2017$tax_land[is.na(properties_2017$tax_land)] <- median(properties_2017$tax_land[!is.na(properties_2017$tax_land)])

# Impute tax_property
properties_2016$m_tax_property <- 0
properties_2016$m_tax_property[is.na(properties_2016$tax_property)] <- 1
properties_2016$tax_property[is.na(properties_2016$tax_property) & properties_2016$tax_total < 5.131e+05] <- 3745
properties_2016$tax_property[is.na(properties_2016$tax_property) & properties_2016$tax_total < 1.409e+06
                             & properties_2016$tax_total >= 5.131e+05] <- 9356
properties_2016$tax_property[is.na(properties_2016$tax_property) & properties_2016$tax_total < 4.761e+06
                             & properties_2016$tax_total >= 1.409e+06] <- 25800
properties_2016$tax_property[is.na(properties_2016$tax_property) & properties_2016$tax_total >= 4.761e+06] <- 85855

properties_2017$m_tax_property <- 0
properties_2017$m_tax_property[is.na(properties_2017$tax_property)] <- 1
properties_2017$tax_property[is.na(properties_2017$tax_property) & properties_2017$tax_total < 6.38e+05] <- 3844
properties_2017$tax_property[is.na(properties_2017$tax_property) & properties_2017$tax_total < 1.924e+06
                             & properties_2017$tax_total >= 6.38e+05] <- 11368
properties_2017$tax_property[is.na(properties_2017$tax_property) & properties_2017$tax_total < 8.258e+06
                             & properties_2017$tax_total >= 1.924e+06] <- 35482
properties_2017$tax_property[is.na(properties_2017$tax_property) & properties_2017$tax_total >= 8.258e+06] <- 158447



# Impute tax_building
properties_2016$m_tax_building <- 0
properties_2016$m_tax_building[is.na(properties_2016$tax_building)] <- 1
properties_2016$tax_building[is.na(properties_2016$tax_building)
                             & properties_2016$tax_total < 4.82e+05] <- 114668
properties_2016$tax_building[is.na(properties_2016$tax_building) &
                               properties_2016$tax_total >= 4.82e+05 & properties_2016$tax_total < 1.24e+06] <- 255698
properties_2016$tax_building[is.na(properties_2016$tax_building) &
                               properties_2016$tax_total >= 1.24e+06 & properties_2016$tax_property < 6.355e+04] <- 685596
properties_2016$tax_building[is.na(properties_2016$tax_building) &
                               properties_2016$tax_total >= 1.24e+06 & properties_2016$tax_property >= 6.355e+04] <- 2455593

properties_2017$m_tax_building <- 0
properties_2017$m_tax_building[is.na(properties_2017$tax_building)] <- 1
properties_2017$tax_building[is.na(properties_2017$tax_building)
                             & properties_2017$tax_property< 6771] <- 122900
properties_2017$tax_building[is.na(properties_2017$tax_building) &
                               properties_2017$tax_property>=6771 & properties_2017$tax_property< 1.701e+04] <- 291689
properties_2017$tax_building[is.na(properties_2017$tax_building) &
                               properties_2017$tax_property>=1.701e+04 & properties_2017$tax_property< 5.933e+04] <- 749209
properties_2017$tax_building[is.na(properties_2017$tax_building) &
                               properties_2017$tax_property>=5.933e+04] <- 2411452


properties_2016$m_rawcensustractandblock <- 0
properties_2016$m_rawcensustractandblock[is.na(properties_2016$rawcensustractandblock)] <- 1
properties_2016$rawcensustractandblock[is.na(properties_2016$rawcensustractandblock)] <- 
  as.numeric(names(which.max(table(properties_2016$rawcensustractandblock))))

# Impute censustractandblock - this is censustractandblock with another number appended
# have no way to get that number so pad with zeros
properties_2016$m_censustractandblock <- 0
properties_2016$m_censustractandblock[is.na(properties_2016$censustractandblock)] <- 1
properties_2016$censustractandblock[is.na(properties_2016$censustractandblock)] <-
  as.numeric(str_pad(properties_2016$rawcensustractandblock[is.na(properties_2016$censustractandblock)], width = 14, side = "right", pad = "0"))


properties_2017$m_censustractandblock <- 0
properties_2017$m_censustractandblock[is.na(properties_2017$censustractandblock)] <- 1
properties_2017$censustractandblock[is.na(properties_2017$censustractandblock)] <-
  as.numeric(str_pad(properties_2017$rawcensustractandblock[is.na(properties_2017$censustractandblock)], width = 14, side = "right", pad = "0"))


properties_2016$m_num_bathroom <- 0
properties_2016$m_num_bathroom[is.na(properties_2016$num_bathroom)] <- 1
properties_2016$num_bathroom[is.na(properties_2016$num_bathroom)] <- 
                median(properties_2016$num_bathroom[!is.na(properties_2016$num_bathroom)])


# num_bathroom has a number of zero values which doesn't make sense - clean this up
properties_2016$num_bathroom[properties_2016$num_bathroom == 0 & properties_2016$area_total_calc < 904.5] <- 1
properties_2016$num_bathroom[properties_2016$num_bathroom == 0 & properties_2016$area_total_calc >= 904.5
                             & properties_2016$area_total_calc < 1174] <- 1.5
properties_2016$num_bathroom[properties_2016$num_bathroom == 0 & properties_2016$area_total_calc < 1654 
                             & properties_2016$area_total_calc >= 1174] <- 2
properties_2016$num_bathroom[properties_2016$num_bathroom == 0 & properties_2016$area_total_calc >= 1654 
                             & properties_2016$area_total_calc < 2238] <- 2.5
properties_2016$num_bathroom[properties_2016$num_bathroom == 0 & properties_2016$area_total_calc >= 2238 
                             & properties_2016$area_total_calc < 3030] <- 3
properties_2016$num_bathroom[properties_2016$num_bathroom == 0 & properties_2016$area_total_calc >= 3030 
                             & properties_2016$area_total_calc < 3530] <- 3.5
properties_2016$num_bathroom[properties_2016$num_bathroom == 0 & properties_2016$area_total_calc >= 3530 
                             & properties_2016$area_total_calc < 4602] <- 4
properties_2016$num_bathroom[properties_2016$num_bathroom == 0 & properties_2016$area_total_calc >= 4602] <- 5.5

properties_2017$m_num_bathroom <- 0
properties_2017$m_num_bathroom[is.na(properties_2017$num_bathroom)] <- 1
properties_2017$num_bathroom[is.na(properties_2017$num_bathroom)] <- median(properties_2017$num_bathroom[!is.na(properties_2017$num_bathroom)])

properties_2017$num_bathroom[properties_2017$num_bathroom == 0 & properties_2017$area_total_calc < 904.5] <- 1
properties_2017$num_bathroom[properties_2017$num_bathroom == 0 & properties_2017$area_total_calc >= 904.5
                             & properties_2017$area_total_calc < 1174] <- 1.5
properties_2017$num_bathroom[properties_2017$num_bathroom == 0 & properties_2017$area_total_calc < 1654 
                             & properties_2017$area_total_calc >= 1174] <- 2
properties_2017$num_bathroom[properties_2017$num_bathroom == 0 & properties_2017$area_total_calc >= 1654 
                             & properties_2017$area_total_calc < 2238] <- 2.5
properties_2017$num_bathroom[properties_2017$num_bathroom == 0 & properties_2017$area_total_calc >= 2238 
                             & properties_2017$area_total_calc < 3030] <- 3
properties_2017$num_bathroom[properties_2017$num_bathroom == 0 & properties_2017$area_total_calc >= 3030 
                             & properties_2017$area_total_calc < 3530] <- 3.5
properties_2017$num_bathroom[properties_2017$num_bathroom == 0 & properties_2017$area_total_calc >= 3530 
                             & properties_2017$area_total_calc < 4602] <- 4
properties_2017$num_bathroom[properties_2017$num_bathroom == 0 & properties_2017$area_total_calc >= 4602] <- 5.5


# Impute num_bathroom_calc -- This is actually always the same as num_bathroom, so I'll delete this variable
#  However it might be helpful to know which records were missing this data so I'll keep an indicator
properties_2016$m_num_bathroom_calc <- 0
properties_2016$m_num_bathroom_calc[is.na(properties_2016$num_bathroom_calc)] <- 1
properties_2016$num_bathroom_calc[is.na(properties_2016$num_bathroom_calc)] <- 
        properties_2016$num_bathroom[is.na(properties_2016$num_bathroom_calc)]

properties_2017$m_num_bathroom_calc <- 0
properties_2017$m_num_bathroom_calc[is.na(properties_2017$num_bathroom_calc)] <- 1
properties_2017$num_bathroom_calc[is.na(properties_2017$num_bathroom_calc)] <- properties_2017$num_bathroom[is.na(properties_2017$num_bathroom_calc)]


# Impute num_bath - this is the bathroom count rounded down to the nearest whole number
properties_2016$m_num_bath <- 0
properties_2016$m_num_bath[is.na(properties_2016$num_bath)] <- 1
properties_2016$num_bath[is.na(properties_2016$num_bath)] <- 
    floor(properties_2016$num_bathroom[is.na(properties_2016$num_bath)])

properties_2017$m_num_bath <- 0
properties_2017$m_num_bath[is.na(properties_2017$num_bath)] <- 1
properties_2017$num_bath[is.na(properties_2017$num_bath)] <- floor(properties_2017$num_bathroom[is.na(properties_2017$num_bath)])


# num_bedroom has a number of zero values which doesn't make sense - clean this up
properties_2016$m_num_bedroom<- 0
properties_2016$m_num_bedroom[is.na(properties_2016$num_bedroom)] <- 1
properties_2016$num_bedroom[is.na(properties_2016$num_bedroom)] <- median(properties_2016$num_bedroom[!is.na(properties_2016$num_bedroom)])


properties_2016$num_bedroom[properties_2016$num_bedroom == 0 & properties_2016$area_total_calc < 781.5] <- 1
properties_2016$num_bedroom[properties_2016$num_bedroom == 0 & properties_2016$area_total_calc > 781.5
                            & properties_2016$area_total_calc < 1024] <- 2
properties_2016$num_bedroom[properties_2016$num_bedroom == 0 & properties_2016$area_total_calc >= 1024 
                            & properties_2016$area_total_calc < 1724] <- 3
properties_2016$num_bedroom[properties_2016$num_bedroom == 0 & properties_2016$area_total_calc >= 1724
                            & properties_2016$num_bathroom < 3.75 & properties_2016$area_total_calc < 2184] <- 3
properties_2016$num_bedroom[properties_2016$num_bedroom == 0 & properties_2016$area_total_calc >= 1724
                            & properties_2016$num_bathroom < 3.75 & properties_2016$area_total_calc >= 2184] <- 4
properties_2016$num_bedroom[properties_2016$num_bedroom == 0 & properties_2016$area_total_calc >= 1724
                            & properties_2016$num_bathroom >= 3.75 &  properties_2016$num_bathroom < 5.75] <- 5
properties_2016$num_bedroom[properties_2016$num_bedroom == 0 & properties_2016$area_total_calc >= 1724
                            & properties_2016$num_bathroom >= 3.75 &  properties_2016$num_bathroom >= 5.75] <- 6

properties_2017$m_num_bedroom<- 0
properties_2017$m_num_bedroom[is.na(properties_2017$num_bedroom)] <- 1
properties_2017$num_bedroom[is.na(properties_2017$num_bedroom)] <- median(properties_2017$num_bedroom[!is.na(properties_2017$num_bedroom)])

properties_2017$num_bedroom[properties_2017$num_bedroom == 0 & properties_2017$area_total_calc < 781.5] <- 1
properties_2017$num_bedroom[properties_2017$num_bedroom == 0 & properties_2017$area_total_calc > 781.5
                            & properties_2017$area_total_calc < 1024] <- 2
properties_2017$num_bedroom[properties_2017$num_bedroom == 0 & properties_2017$area_total_calc >= 1024 
                            & properties_2017$area_total_calc < 1724] <- 3
properties_2017$num_bedroom[properties_2017$num_bedroom == 0 & properties_2017$area_total_calc >= 1724
                            & properties_2017$num_bathroom < 3.75 & properties_2017$area_total_calc < 2184] <- 3
properties_2017$num_bedroom[properties_2017$num_bedroom == 0 & properties_2017$area_total_calc >= 1724
                            & properties_2017$num_bathroom < 3.75 & properties_2017$area_total_calc >= 2184] <- 4
properties_2017$num_bedroom[properties_2017$num_bedroom == 0 & properties_2017$area_total_calc >= 1724
                            & properties_2017$num_bathroom >= 3.75 &  properties_2017$num_bathroom < 5.75] <- 5
properties_2017$num_bedroom[properties_2017$num_bedroom == 0 & properties_2017$area_total_calc >= 1724
                            & properties_2017$num_bathroom >= 3.75 &  properties_2017$num_bathroom >= 5.75] <- 6


properties_2016$m_num_bathroom<- 0
properties_2016$m_num_bathroom[is.na(properties_2016$num_bathroom)] <- 1
properties_2016$num_bathroom[is.na(properties_2016$num_bathroom)] <- 
          median(properties_2016$num_bathroom[!is.na(properties_2016$num_bathroom)])



# Impute area_total_calc
properties_2016$m_area_total_calc <- 0
properties_2016$m_area_total_calc[is.na(properties_2016$area_total_calc)] <- 1
properties_2016$area_total_calc[is.na(properties_2016$area_total_calc)
                                & properties_2016$num_bathroom < 2.25] <- 1338
properties_2016$area_total_calc[is.na(properties_2016$area_total_calc) &
                                  properties_2016$num_bathroom > 2.25 & properties_2016$num_bathroom < 3.25] <- 2068
properties_2016$area_total_calc[is.na(properties_2016$area_total_calc) &
                                  properties_2016$num_bathroom < 5.25 & properties_2016$num_bathroom >= 3.25] <- 3391
properties_2016$area_total_calc[is.na(properties_2016$area_total_calc) &
                                  properties_2016$num_bathroom >= 5.25] <- 5514

properties_2017$m_area_total_calc <- 0
properties_2017$m_area_total_calc[is.na(properties_2017$area_total_calc)] <- 1
properties_2017$area_total_calc[is.na(properties_2017$area_total_calc)
                                & properties_2017$num_bathroom < 2.25] <- 1338
properties_2017$area_total_calc[is.na(properties_2017$area_total_calc) &
                                  properties_2017$num_bathroom > 2.25 & properties_2017$num_bathroom < 3.25] <- 2068
properties_2017$area_total_calc[is.na(properties_2017$area_total_calc) &
                                  properties_2017$num_bathroom < 5.25 & properties_2017$num_bathroom >= 3.25] <- 3391
properties_2017$area_total_calc[is.na(properties_2017$area_total_calc) &
                                  properties_2017$num_bathroom >= 5.25] <- 5514


properties_2017$m_latitude <- 0
properties_2017$m_latitude[is.na(properties_2017$latitude)] <- 1
properties_2017$latitude[is.na(properties_2017$latitude)] <- as.numeric(names(which.max(table(properties_2017$latitude))))

properties_2017$m_longitude <- 0
properties_2017$m_longitude[is.na(properties_2017$longitude)] <- 1
properties_2017$longitude[is.na(properties_2017$longitude)] <-  -118444000


# Impute region_zip 
properties_2016$m_region_zip <- 0
properties_2016$m_region_zip[is.na(properties_2016$region_zip)] <- 1

# Create subset dataset that incluldes all region_zip fields that are missing
zipFinder <- subset(properties_2016, is.na(properties_2016$region_zip), select = c(region_zip, longitude, latitude))

# Loop through subset of data- get longitude and latitude data and use ggmap library to look 
#  up the zip code using the longitude and latitude. Then set the zip code to the missing value
for (i in 1:nrow(zipFinder)) {
  
  lonlat_sample <- c(round(zipFinder$longitude[i] / 986931, 2),
                     round(zipFinder$latitude[i] / 867022, 2))
  res <- ggmap::revgeocode(lonlat_sample, output = "more")
  zipcode <- res$postal_code
  properties_2016$region_zip[is.na(properties_2016$region_zip) & properties_2016$longitude == zipFinder$longitude[i]
                             & properties_2016$latitude == zipFinder$latitude[i]] <- zipcode
}

tmv2016()

properties_2017$m_region_zip <- 0
properties_2017$m_region_zip[is.na(properties_2017$region_zip)] <- 1

# Create subset dataset that incluldes all region_zip fields that are missing
zipFinder <- subset(properties_2017, is.na(properties_2017$region_zip), select = c(region_zip, longitude, latitude))

# Loop through subset of data- get longitude and latitude data and use ggmap library to look 
#  up the zip code using the longitude and latitude. Then set the zip code to the missing value
for (i in 1:nrow(zipFinder)) {
  
  lonlat_sample <- c(round(zipFinder$longitude[i] / 986931, 2),
                     round(zipFinder$latitude[i] / 867022, 2))
  res <- ggmap::revgeocode(lonlat_sample, output = "more")
  zipcode <- res$postal_code
  if(is.integer(zipcode)) 
    properties_2017$region_zip[is.na(properties_2017$region_zip) & properties_2017$longitude == zipFinder$longitude[i]
                               & properties_2017$latitude == zipFinder$latitude[i]] <- zipcode
  else
    properties_2017$region_zip[is.na(properties_2017$region_zip) & properties_2017$longitude == zipFinder$longitude[i]
                               & properties_2017$latitude == zipFinder$latitude[i]] <- 96126
  
}



# Impute build_year
properties_2016$m_build_year <- 0
properties_2016$m_build_year[is.na(properties_2016$build_year)] <- 1
properties_2016$build_year[is.na(properties_2016$build_year) & properties_2016$latitude < 3.436e+07 & !(properties_2016$zoning_landuse_county %in%
                                                                                                          c("010", "0102", "010C", "010D", "010M", "0114", "012C", "012D", "0131", "01DC", "01HC", "020M",
                                                                                                            "0700", "070D", "1", "1111", "1116", "1117", "1128", "1129", "1200", "122", "1310", "1432",
                                                                                                            "34"))] <- 1951
properties_2016$build_year[is.na(properties_2016$build_year) & properties_2016$tax_building >= 1.237e+05
                           & properties_2016$zoning_landuse_county %in% c("010", "0102", "010C", "010D", "010M", "0114", "012C",
                                                                          "012D", "0131", "01DC", "01HC", "020M", "0700", "070D", "1", "1111", "1116", "1117", "1128", "1129",
                                                                          "1200", "122", "1310", "1432", "34")] <- 1988
properties_2016$build_year[is.na(properties_2016$build_year) & properties_2016$tax_building < 1.237e+05
                           & properties_2016$zoning_landuse_county %in% c("010", "0102", "010C", "010D", "010M", "0114", "012C",
                                                                          "012D", "0131", "01DC", "01HC", "020M", "0700", "070D", "1", "1111", "1116", "1117", "1128", "1129",
                                                                          "1200", "122", "1310", "1432", "34")] <- 1971
properties_2016$build_year[is.na(properties_2016$build_year) & properties_2016$latitude >= 3.436e+07 & !(properties_2016$zoning_landuse_county %in%
                                                                                                           c("010", "0102", "010C", "010D", "010M", "0114", "012C", "012D", "0131", "01DC", "01HC", "020M",
                                                                                                             "0700", "070D", "1", "1111", "1116", "1117", "1128", "1129", "1200", "122", "1310", "1432",
                                                                                                             "34"))] <- 1985

properties_2017$m_build_year <- 0
properties_2017$m_build_year[is.na(properties_2017$build_year)] <- 1
properties_2017$build_year[is.na(properties_2017$build_year) & properties_2017$latitude < 3.436e+07 & !(properties_2017$zoning_landuse_county %in%
                                                                                                          c("010", "0102", "010C", "010D", "010M", "0114", "012C", "012D", "0131", "01DC", "01HC", "020M",
                                                                                                            "0700", "070D", "1", "1111", "1116", "1117", "1128", "1129", "1200", "122", "1310", "1432",
                                                                                                            "34"))] <- 1951
properties_2017$build_year[is.na(properties_2017$build_year) & properties_2017$tax_building >= 1.237e+05
                           & properties_2017$zoning_landuse_county %in% c("010", "0102", "010C", "010D", "010M", "0114", "012C",
                                                                          "012D", "0131", "01DC", "01HC", "020M", "0700", "070D", "1", "1111", "1116", "1117", "1128", "1129",
                                                                          "1200", "122", "1310", "1432", "34")] <- 1988
properties_2017$build_year[is.na(properties_2017$build_year) & properties_2017$tax_building < 1.237e+05
                           & properties_2017$zoning_landuse_county %in% c("010", "0102", "010C", "010D", "010M", "0114", "012C",
                                                                          "012D", "0131", "01DC", "01HC", "020M", "0700", "070D", "1", "1111", "1116", "1117", "1128", "1129",
                                                                          "1200", "122", "1310", "1432", "34")] <- 1971
properties_2017$build_year[is.na(properties_2017$build_year) & properties_2017$latitude >= 3.436e+07 & !(properties_2017$zoning_landuse_county %in%
                                                                                                           c("010", "0102", "010C", "010D", "010M", "0114", "012C", "012D", "0131", "01DC", "01HC", "020M",
                                                                                                             "0700", "070D", "1", "1111", "1116", "1117", "1128", "1129", "1200", "122", "1310", "1432",
                                                                                                             "34"))] <- 1985

# Impute region_city - base this off the zip code and just select the first unique 
#                       city id found
properties_2016$m_region_city <- 0
properties_2016$m_region_city[is.na(properties_2016$region_city)] <- 1
city <- subset(properties_2016, is.na(properties_2016$region_city), select =region_zip)

for (i in 1:nrow(city)) {
  
  zip <- city$region_zip[i]
  zipcities <- subset(properties_2016, !is.na(properties_2016$region_city) & properties_2016$region_zip == zip,
                      select = region_city)
  cities <- unique(zipcities)
  properties_2016$region_city[is.na(properties_2016$region_city) & properties_2016$region_zip == zip] <- cities[1,]
}

properties_2016$region_city[is.na(properties_2016$region_city) & properties_2016$region_zip == 96395] <- 11111
properties_2016$region_city[is.na(properties_2016$region_city) & properties_2016$region_zip == 96500] <- 22222

tmv()

properties_2017$m_region_city <- 0
properties_2017$m_region_city[is.na(properties_2017$region_city)] <- 1
city <- subset(properties_2017, is.na(properties_2017$region_city), select =region_zip)

for (i in 1:nrow(city)) {
  
  zip <- city$region_zip[i]
  zipcities <- subset(properties_2017, !is.na(properties_2017$region_city) & properties_2017$region_zip == zip,
                      select = region_city)
  cities <- unique(zipcities)
  properties_2017$region_city[is.na(properties_2017$region_city) & properties_2017$region_zip == zip] <- zipcities[1,]
}

properties_2017$region_city[is.na(properties_2017$region_city) & properties_2017$region_zip == 96395] <- 11111
properties_2017$region_city[is.na(properties_2017$region_city) & properties_2017$region_zip == 96500] <- 22222

#################################################################
# Ran out of geocode allowance so just use most frequent value  # 
#################################################################

properties_2016$region_city[is.na(properties_2016$region_city)] <- as.numeric(names(which.max(table(properties_2016$region_city))))
properties_2017$region_city[is.na(properties_2017$region_city)] <- as.numeric(names(which.max(table(properties_2017$region_city))))

properties_2016$region_zip[is.na(properties_2016$region_zip)] <- as.numeric(names(which.max(table(properties_2016$region_zip))))
properties_2017$region_zip[is.na(properties_2017$region_zip)] <- as.numeric(names(which.max(table(properties_2017$region_zip))))


tmv2()

# Impute area_live_finished
properties_2016$m_area_live_finished <- 0
properties_2016$m_area_live_finished[is.na(properties_2016$area_live_finished)] <- 1
properties_2016$area_live_finished[is.na(properties_2016$area_live_finished)
                                   & properties_2016$area_total_calc < 1466] <- 1120
properties_2016$area_live_finished[is.na(properties_2016$area_live_finished) & properties_2016$area_total_calc >= 1466
                                   & properties_2016$area_total_calc < 2316] <- 1810
properties_2016$area_live_finished[is.na(properties_2016$area_live_finished) & properties_2016$area_total_calc < 4101
                                   & properties_2016$area_total_calc >= 2316] <- 2905
properties_2016$area_live_finished[is.na(properties_2016$area_live_finished)
                                   & properties_2016$area_total_calc >= 4101] <- 5296

properties_2017$m_area_live_finished <- 0
properties_2017$m_area_live_finished[is.na(properties_2017$area_live_finished)] <- 1
properties_2017$area_live_finished[is.na(properties_2017$area_live_finished)
                                   & properties_2017$area_total_calc < 1466] <- 1120
properties_2017$area_live_finished[is.na(properties_2017$area_live_finished) & properties_2017$area_total_calc >= 1466
                                   & properties_2017$area_total_calc < 2316] <- 1810
properties_2017$area_live_finished[is.na(properties_2017$area_live_finished) & properties_2017$area_total_calc < 4101
                                   & properties_2017$area_total_calc >= 2316] <- 2905
properties_2017$area_live_finished[is.na(properties_2017$area_live_finished)
                                   & properties_2017$area_total_calc >= 4101] <- 5296

# Impute area_lot
properties_2016$m_area_lot <- 0
properties_2016$m_area_lot[is.na(properties_2016$area_lot)] <- 1
properties_2016$area_lot[is.na(properties_2016$area_lot) & !(properties_2016$zoning_landuse_county %in%
                                                               c("0109", "010C", "010F", "010G", "010M", "012C", "01DC", "020M", "0700", 
                                                                 "1112", "1432"))] <- 10779
properties_2016$area_lot[is.na(properties_2016$area_lot) & properties_2016$latitude < 3.431e+07
                         & properties_2016$zoning_landuse_county %in% c("0109", "010C", "010F", "010G", "010M", 
                                                                        "012C", "01DC", "020M", "0700", "1112", "1432")] <- 104180
properties_2016$area_lot[is.na(properties_2016$area_lot) & properties_2016$latitude >= 3.431e+07
                         & properties_2016$zoning_landuse_county %in% c("0109", "010C", "010F", "010G", "010M",
                                                                        "012C", "01DC", "020M", "0700", "1112", "1432")] <- 354662

properties_2017$m_area_lot <- 0
properties_2017$m_area_lot[is.na(properties_2017$area_lot)] <- 1
properties_2017$area_lot[is.na(properties_2017$area_lot) & !(properties_2017$zoning_landuse_county %in%
                                                               c("0109", "010C", "010F", "010G", "010M", "012C", "01DC", "020M", "0700", 
                                                                 "1112", "1432"))] <- 10779
properties_2017$area_lot[is.na(properties_2017$area_lot) & properties_2017$latitude < 3.431e+07
                         & properties_2017$zoning_landuse_county %in% c("0109", "010C", "010F", "010G", "010M", 
                                                                        "012C", "01DC", "020M", "0700", "1112", "1432")] <- 104180
properties_2017$area_lot[is.na(properties_2017$area_lot) & properties_2017$latitude >= 3.431e+07
                         & properties_2017$zoning_landuse_county %in% c("0109", "010C", "010F", "010G", "010M",
                                                                        "012C", "01DC", "020M", "0700", "1112", "1432")] <- 354662

# Impute num_unit 
properties_2016$m_num_unit <- 0
properties_2016$m_num_unit[is.na(properties_2016$num_unit)] <- 1
properties_2016$num_unit[is.na(properties_2016$num_unit) & properties_2016$num_bedroom < 7.5] <- 1
properties_2016$num_unit[is.na(properties_2016$num_unit) & properties_2016$num_bedroom >= 7.5] <-4

summary(properties_2016$num_unit)

properties_2017$m_num_unit <- 0
properties_2017$m_num_unit[is.na(properties_2017$num_unit)] <- 1
properties_2017$num_unit[is.na(properties_2017$num_unit) & properties_2017$num_bedroom < 7.5] <- 1
properties_2017$num_unit[is.na(properties_2017$num_unit) & properties_2017$num_bedroom >= 7.5] <-4


# Zero units doesn't make sense, so set this equal to 1
properties_2016$num_unit[properties_2016$num_unit == 0] <- 1
properties_2017$num_unit[properties_2016$num_unit == 0] <- 1


# Create houseAge variable
properties_2016$houseAge <- 2016 - properties_2016$build_year
properties_2016$build_year <- NULL

properties_2016$houseAge[is.na(properties_2016$houseAge)] <- 
        median(properties_2016$houseAge[!is.na(properties_2016$houseAge)])


properties_2017$houseAge <- 2017 - properties_2017$build_year
properties_2017$build_year <- NULL

# Create "roll your own" data
properties_2016$mo=as.factor(substr(properties_2016$date,6,7))
properties_2016$day=as.factor(substr(properties_2016$date,9,10)) 

properties_2017$mo=as.factor(substr(properties_2017$date,6,7))
properties_2017$day=as.factor(substr(properties_2017$date,9,10)) 


# Impute quality
properties_2016$m_quality <- 0
properties_2016$m_quality[is.na(properties_2016$quality)] <- 1
properties_2016$quality[is.na(properties_2016$quality) & properties_2016$houseAge >= 58.5] <- 7
properties_2016$quality[is.na(properties_2016$quality) & properties_2016$houseAge < 58.5] <-4

summary(properties_2016$quality)

properties_2017$m_quality <- 0
properties_2017$m_quality[is.na(properties_2017$quality)] <- 1
properties_2017$quality[is.na(properties_2017$quality) & properties_2017$houseAge >= 58.5] <- 7
properties_2017$quality[is.na(properties_2017$quality) & properties_2017$houseAge < 58.5] <-4


# Impute heating
properties_2016$m_heating <- 0
properties_2016$m_heating[is.na(properties_2016$heating)] <- 1
properties_2016$heating[is.na(properties_2016$heating) & properties_2016$fips< 6048] <- 2
properties_2016$heating[is.na(properties_2016$heating) & properties_2016$fips >= 6048 & properties_2016$num_room <6.5] <- 24
properties_2016$heating[is.na(properties_2016$heating) & properties_2016$fips >= 6048 & properties_2016$num_room >=6.5] <- 6

properties_2017$m_heating <- 0
properties_2017$m_heating[is.na(properties_2017$heating)] <- 1
properties_2017$heating[is.na(properties_2017$heating) & properties_2017$fips< 6048] <- 2
properties_2017$heating[is.na(properties_2017$heating) & properties_2017$fips >= 6048 & properties_2017$num_room <6.5] <- 24
properties_2017$heating[is.na(properties_2017$heating) & properties_2017$fips >= 6048 & properties_2017$num_room >=6.5] <- 6

# Impute region_neighbor
properties_2016$m_region_neighbor <- 0
properties_2016$m_region_neighbor[is.na(properties_2016$region_neighbor)] <- 1
properties_2016$region_neighbor[is.na(properties_2016$region_neighbor) & properties_2016$latitude>=3.419e+07 & 
                                  properties_2016$latitude< 3.427e+07 & properties_2016$longitude>=-1.185e+08] <- 41131
properties_2016$region_neighbor[is.na(properties_2016$region_neighbor) & properties_2016$latitude>=3.419e+07 & 
                                  properties_2016$latitude>=3.422e+07 & properties_2016$longitude>=-1.186e+08 & properties_2016$longitudelongitude< -1.185e+08] <- 33183
properties_2016$region_neighbor[is.na(properties_2016$region_neighbor) & properties_2016$latitude< 3.408e+07 & properties_2016$longitude>=-1.182e+08] <- 113455
properties_2016$region_neighbor[is.na(properties_2016$region_neighbor) & properties_2016$latitude< 3.392e+07 & 
                                  properties_2016$longitude< -1.182e+08 & properties_2016$longitude>=-1.184e+08] <- 54300
properties_2016$region_neighbor[is.na(properties_2016$region_neighbor) & properties_2016$latitude< 3.408e+07 & properties_2016$latitude>=3.403e+07 &
                                  properties_2016$longitude< -1.182e+08 & properties_2016$longitude>=-1.184e+08] <- 274514
properties_2016$region_neighbor[is.na(properties_2016$region_neighbor) & properties_2016$latitude< 3.403e+07 & properties_2016$latitude>=3.392e+07 &
                                  properties_2016$longitude< -1.182e+08 & properties_2016$longitude>=-1.184e+08] <- 268496
properties_2016$region_neighbor[is.na(properties_2016$region_neighbor) & properties_2016$latitude< 3.403e+07 & properties_2016$latitude>=3.392e+07 &
                                  properties_2016$longitude< -1.183e+08 & properties_2016$longitude>=-1.184e+08] <- 118208
properties_2016$region_neighbor[is.na(properties_2016$region_neighbor) & properties_2016$latitude< 3.408e+07 & properties_2016$longitude< -1.184e+08] <- 118920
properties_2016$region_neighbor[is.na(properties_2016$region_neighbor) & properties_2016$latitude>=3.408e+07 & properties_2016$latitude< 3.419e+07
                                & properties_2016$longitude>=-1.183e+08] <- 275405
properties_2016$region_neighbor[is.na(properties_2016$region_neighbor) & properties_2016$latitude< 3.413e+07 & properties_2016$latitude>=3.408e+07 &
                                  properties_2016$longitude< -1.183e+08 & properties_2016$longitude>=-1.185e+08] <- 274049
properties_2016$region_neighbor[is.na(properties_2016$region_neighbor) & properties_2016$latitude>=3.413e+07 & properties_2016$latitude< 3.419e+07 &
                                  properties_2016$longitude>=-1.184e+08 & properties_2016$longitude< -1.183e+08] <- 47880
properties_2016$region_neighbor[is.na(properties_2016$region_neighbor) & properties_2016$latitude>=3.413e+07 & properties_2016$latitude< 3.419e+07 &
                                  properties_2016$longitude< -1.184e+08 & properties_2016$longitude>=-1.185e+08] <- 27080
properties_2016$region_neighbor[is.na(properties_2016$region_neighbor) & properties_2016$latitude>=3.413e+07 & properties_2016$latitude< 3.419e+07 &
                                  properties_2016$longitude< -1.187e+08] <- 46736
properties_2016$region_neighbor[is.na(properties_2016$region_neighbor) & properties_2016$latitude>=3.413e+07 & properties_2016$latitude< 3.419e+07 &
                                  properties_2016$longitude>= -1.187e+08 & properties_2016$longitude< -1.186e+08] <- 48570
properties_2016$region_neighbor[is.na(properties_2016$region_neighbor) & properties_2016$latitude>=3.408e+07 & properties_2016$latitude< 3.419e+07 &
                                  properties_2016$longitude>= -1.185e+08 & properties_2016$longitude< -1.185e+08] <- 51906
properties_2016$region_neighbor[is.na(properties_2016$region_neighbor) & properties_2016$latitude>=3.408e+07 & properties_2016$latitude< 3.419e+07 &
                                  properties_2016$longitude>= -1.186e+08 & properties_2016$longitude< -1.185e+08] <- 47950
properties_2016$region_neighbor[is.na(properties_2016$region_neighbor) & properties_2016$latitude>=3.419e+07 & properties_2016$latitude< 3.427e+07 &
                                  properties_2016$longitude< -1.186e+08] <- 268588
properties_2016$region_neighbor[is.na(properties_2016$region_neighbor) & properties_2016$latitude>=3.419e+07 & properties_2016$latitude< 3.427e+07 &
                                  properties_2016$longitude< -1.185e+08 & properties_2016$longitude>=-1.186e+08] <- 40548
properties_2016$region_neighbor[is.na(properties_2016$region_neighbor) & properties_2016$latitude>=3.427e+07 & properties_2016$latitude< 3.435e+07] <- 34213
properties_2016$region_neighbor[is.na(properties_2016$region_neighbor) & properties_2016$latitude>=3.435e+07 & properties_2016$longitude< -1.185e+08] <- 48200
properties_2016$region_neighbor[is.na(properties_2016$region_neighbor) & properties_2016$latitude>=3.435e+07 & properties_2016$longitude>=-1.185e+08] <- 37739
properties_2016$region_neighbor[is.na(properties_2016$region_neighbor) & properties_2016$latitude>=3.435e+07 & properties_2016$longitude>=-1.185e+08
                                & properties_2016$longitude < -1.185e+08] <- 6952
# There were 70 left over that didn't fit into any of the rules. Setup a dummy neighborhood for these
properties_2016$region_neighbor[is.na(properties_2016$region_neighbor)] <- 11111

properties_2017$m_region_neighbor <- 0
properties_2017$m_region_neighbor[is.na(properties_2017$region_neighbor)] <- 1
properties_2017$region_neighbor[is.na(properties_2017$region_neighbor) & properties_2017$latitude>=3.419e+07 & 
                                  properties_2017$latitude< 3.427e+07 & properties_2017$longitude>=-1.185e+08] <- 41131
properties_2017$region_neighbor[is.na(properties_2017$region_neighbor) & properties_2017$latitude>=3.419e+07 & 
                                  properties_2017$latitude>=3.422e+07 & properties_2017$longitude>=-1.186e+08 & properties_2017$longitudelongitude< -1.185e+08] <- 33183
properties_2017$region_neighbor[is.na(properties_2017$region_neighbor) & properties_2017$latitude< 3.408e+07 & properties_2017$longitude>=-1.182e+08] <- 113455
properties_2017$region_neighbor[is.na(properties_2017$region_neighbor) & properties_2017$latitude< 3.392e+07 & 
                                  properties_2017$longitude< -1.182e+08 & properties_2017$longitude>=-1.184e+08] <- 54300
properties_2017$region_neighbor[is.na(properties_2017$region_neighbor) & properties_2017$latitude< 3.408e+07 & properties_2017$latitude>=3.403e+07 &
                                  properties_2017$longitude< -1.182e+08 & properties_2017$longitude>=-1.184e+08] <- 274514
properties_2017$region_neighbor[is.na(properties_2017$region_neighbor) & properties_2017$latitude< 3.403e+07 & properties_2017$latitude>=3.392e+07 &
                                  properties_2017$longitude< -1.182e+08 & properties_2017$longitude>=-1.184e+08] <- 268496
properties_2017$region_neighbor[is.na(properties_2017$region_neighbor) & properties_2017$latitude< 3.403e+07 & properties_2017$latitude>=3.392e+07 &
                                  properties_2017$longitude< -1.183e+08 & properties_2017$longitude>=-1.184e+08] <- 118208
properties_2017$region_neighbor[is.na(properties_2017$region_neighbor) & properties_2017$latitude< 3.408e+07 & properties_2017$longitude< -1.184e+08] <- 118920
properties_2017$region_neighbor[is.na(properties_2017$region_neighbor) & properties_2017$latitude>=3.408e+07 & properties_2017$latitude< 3.419e+07
                                & properties_2017$longitude>=-1.183e+08] <- 275405
properties_2017$region_neighbor[is.na(properties_2017$region_neighbor) & properties_2017$latitude< 3.413e+07 & properties_2017$latitude>=3.408e+07 &
                                  properties_2017$longitude< -1.183e+08 & properties_2017$longitude>=-1.185e+08] <- 274049
properties_2017$region_neighbor[is.na(properties_2017$region_neighbor) & properties_2017$latitude>=3.413e+07 & properties_2017$latitude< 3.419e+07 &
                                  properties_2017$longitude>=-1.184e+08 & properties_2017$longitude< -1.183e+08] <- 47880
properties_2017$region_neighbor[is.na(properties_2017$region_neighbor) & properties_2017$latitude>=3.413e+07 & properties_2017$latitude< 3.419e+07 &
                                  properties_2017$longitude< -1.184e+08 & properties_2017$longitude>=-1.185e+08] <- 27080
properties_2017$region_neighbor[is.na(properties_2017$region_neighbor) & properties_2017$latitude>=3.413e+07 & properties_2017$latitude< 3.419e+07 &
                                  properties_2017$longitude< -1.187e+08] <- 46736
properties_2017$region_neighbor[is.na(properties_2017$region_neighbor) & properties_2017$latitude>=3.413e+07 & properties_2017$latitude< 3.419e+07 &
                                  properties_2017$longitude>= -1.187e+08 & properties_2017$longitude< -1.186e+08] <- 48570
properties_2017$region_neighbor[is.na(properties_2017$region_neighbor) & properties_2017$latitude>=3.408e+07 & properties_2017$latitude< 3.419e+07 &
                                  properties_2017$longitude>= -1.185e+08 & properties_2017$longitude< -1.185e+08] <- 51906
properties_2017$region_neighbor[is.na(properties_2017$region_neighbor) & properties_2017$latitude>=3.408e+07 & properties_2017$latitude< 3.419e+07 &
                                  properties_2017$longitude>= -1.186e+08 & properties_2017$longitude< -1.185e+08] <- 47950
properties_2017$region_neighbor[is.na(properties_2017$region_neighbor) & properties_2017$latitude>=3.419e+07 & properties_2017$latitude< 3.427e+07 &
                                  properties_2017$longitude< -1.186e+08] <- 268588
properties_2017$region_neighbor[is.na(properties_2017$region_neighbor) & properties_2017$latitude>=3.419e+07 & properties_2017$latitude< 3.427e+07 &
                                  properties_2017$longitude< -1.185e+08 & properties_2017$longitude>=-1.186e+08] <- 40548
properties_2017$region_neighbor[is.na(properties_2017$region_neighbor) & properties_2017$latitude>=3.427e+07 & properties_2017$latitude< 3.435e+07] <- 34213
properties_2017$region_neighbor[is.na(properties_2017$region_neighbor) & properties_2017$latitude>=3.435e+07 & properties_2017$longitude< -1.185e+08] <- 48200
properties_2017$region_neighbor[is.na(properties_2017$region_neighbor) & properties_2017$latitude>=3.435e+07 & properties_2017$longitude>=-1.185e+08] <- 37739
properties_2017$region_neighbor[is.na(properties_2017$region_neighbor) & properties_2017$latitude>=3.435e+07 & properties_2017$longitude>=-1.185e+08
                                & properties_2017$longitude < -1.185e+08] <- 6952
# There were 70 left over that didn't fit into any of the rules. Setup a dummy neighborhood for these
properties_2017$region_neighbor[is.na(properties_2017$region_neighbor)] <- 11111


# Remove variables with too many missing values that also don't have strong correlations to log error. 
# Keep an imputed indicator as this may be a useful indicator
properties_2016$m_aircon <- 0
properties_2016$m_aircon[is.na(properties_2016$aircon)] <- 1
properties_2016$aircon <- NULL

properties_2017$m_aircon <- 0
properties_2017$m_aircon[is.na(properties_2017$aircon)] <- 1
properties_2017$aircon <- NULL


properties_2016$m_architectural_style <- 0
properties_2016$m_architectural_style[is.na(properties_2016$architectural_style)] <- 1
properties_2016$architectural_style <- NULL

properties_2017$m_architectural_style <- 0
properties_2017$m_architectural_style[is.na(properties_2017$architectural_style)] <- 1
properties_2017$architectural_style <- NULL

properties_2016$m_area_basement <- 0
properties_2016$m_area_basement[is.na(properties_2016$area_basement)] <- 1
properties_2016$area_basement <- NULL

properties_2017$m_area_basement <- 0
properties_2017$m_area_basement[is.na(properties_2017$area_basement)] <- 1
properties_2017$area_basement <- NULL

properties_2016$m_framing <- 0
properties_2016$m_framing[is.na(properties_2016$framing)] <- 1
properties_2016$framing <- NULL

properties_2017$m_framing <- 0
properties_2017$m_framing[is.na(properties_2017$framing)] <- 1
properties_2017$framing <- NULL

properties_2016$m_deck <- 0
properties_2016$m_deck[is.na(properties_2016$deck)] <- 1
properties_2016$deck <- NULL

properties_2017$m_deck <- 0
properties_2017$m_deck[is.na(properties_2017$deck)] <- 1
properties_2017$deck <- NULL

properties_2016$m_area_firstfloor_finished <- 0
properties_2016$m_area_firstfloor_finished[is.na(properties_2016$area_firstfloor_finished)] <- 1
properties_2016$area_firstfloor_finished <- NULL

properties_2017$m_area_firstfloor_finished <- 0
properties_2017$m_area_firstfloor_finished[is.na(properties_2017$area_firstfloor_finished)] <- 1
properties_2017$area_firstfloor_finished <- NULL

properties_2016$m_area_liveperi_finished <- 0
properties_2016$m_area_liveperi_finished[is.na(properties_2016$area_liveperi_finished)] <- 1
properties_2016$area_liveperi_finished <- NULL

properties_2017$m_area_liveperi_finished <- 0
properties_2017$m_area_liveperi_finished[is.na(properties_2017$area_liveperi_finished)] <- 1
properties_2017$area_liveperi_finished <- NULL


properties_2016$m_area_total_finished <- 0
properties_2016$m_area_total_finished[is.na(properties_2016$area_total_finished)] <- 1
properties_2016$area_total_finished <- NULL

properties_2017$m_area_total_finished <- 0
properties_2017$m_area_total_finished[is.na(properties_2017$area_total_finished)] <- 1
properties_2017$area_total_finished <- NULL


properties_2016$m_area_unknown <- 0
properties_2016$m_area_unknown[is.na(properties_2016$area_unknown)] <- 1
properties_2016$area_unknown <- NULL

properties_2017$m_area_unknown <- 0
properties_2017$m_area_unknown[is.na(properties_2017$area_unknown)] <- 1
properties_2017$area_unknown <- NULL


properties_2016$m_area_base <- 0
properties_2016$m_area_base[is.na(properties_2016$area_base)] <- 1
properties_2016$area_base <- NULL

properties_2017$m_area_base <- 0
properties_2017$m_area_base[is.na(properties_2017$area_base)] <- 1
properties_2017$area_base <- NULL


properties_2016$m_num_fireplace <- 0
properties_2016$m_num_fireplace[is.na(properties_2016$num_fireplace)] <- 1
properties_2016$num_fireplace <- NULL

properties_2017$m_num_fireplace <- 0
properties_2017$m_num_fireplace[is.na(properties_2017$num_fireplace)] <- 1
properties_2017$num_fireplace <- NULL


properties_2016$m_num_garage <- 0
properties_2016$m_num_garage[is.na(properties_2016$num_garage)] <- 1
properties_2016$num_garage <- NULL

properties_2017$m_num_garage <- 0
properties_2017$m_num_garage[is.na(properties_2017$num_garage)] <- 1
properties_2017$num_garage <- NULL


properties_2016$m_area_garage <- 0
properties_2016$m_area_garage[is.na(properties_2016$area_garage)] <- 1
properties_2016$area_garage <- NULL

properties_2017$m_area_garage <- 0
properties_2017$m_area_garage[is.na(properties_2017$area_garage)] <- 1
properties_2017$area_garage <- NULL


properties_2016$m_num_pool <- 0
properties_2016$m_num_pool[is.na(properties_2016$num_pool)] <- 1
properties_2016$num_pool <- NULL

properties_2017$m_num_pool <- 0
properties_2017$m_num_pool[is.na(properties_2017$num_pool)] <- 1
properties_2017$num_pool <- NULL


properties_2016$m_area_pool <- 0
properties_2016$m_area_pool[is.na(properties_2016$area_pool)] <- 1
properties_2016$area_pool <- NULL

properties_2017$m_area_pool <- 0
properties_2017$m_area_pool[is.na(properties_2017$area_pool)] <- 1
properties_2017$area_pool <- NULL


properties_2016$m_pooltypeid10 <- 0
properties_2016$m_pooltypeid10[is.na(properties_2016$pooltypeid10)] <- 1
properties_2016$pooltypeid10 <- NULL


properties_2017$m_pooltypeid10 <- 0
properties_2017$m_pooltypeid10[is.na(properties_2017$pooltypeid10)] <- 1
properties_2017$pooltypeid10 <- NULL


properties_2016$m_pooltypeid2 <- 0
properties_2016$m_pooltypeid2[is.na(properties_2016$pooltypeid2)] <- 1
properties_2016$pooltypeid2 <- NULL

properties_2017$m_pooltypeid2 <- 0
properties_2017$m_pooltypeid2[is.na(properties_2017$pooltypeid2)] <- 1
properties_2017$pooltypeid2 <- NULL


properties_2016$m_pooltypeid7 <- 0
properties_2016$m_pooltypeid7[is.na(properties_2016$pooltypeid7)] <- 1
properties_2016$pooltypeid7 <- NULL

properties_2017$m_pooltypeid7 <- 0
properties_2017$m_pooltypeid7[is.na(properties_2017$pooltypeid7)] <- 1
properties_2017$pooltypeid7 <- NULL


properties_2016$m_story <- 0
properties_2016$m_story[is.na(properties_2016$story)] <- 1
properties_2016$story <- NULL

properties_2017$m_story <- 0
properties_2017$m_story[is.na(properties_2017$story)] <- 1
properties_2017$story <- NULL


properties_2016$m_num_75_bath <- 0
properties_2016$m_num_75_bath[is.na(properties_2016$num_75_bath)] <- 1
properties_2016$num_75_bath <- NULL

properties_2017$m_num_75_bath <- 0
properties_2017$m_num_75_bath[is.na(properties_2017$num_75_bath)] <- 1
properties_2017$num_75_bath <- NULL


properties_2016$m_material <- 0
properties_2016$m_material[is.na(properties_2016$material)] <- 1
properties_2016$material <- NULL

properties_2017$m_material <- 0
properties_2017$m_material[is.na(properties_2017$material)] <- 1
properties_2017$material <- NULL


properties_2016$m_area_patio <- 0
properties_2016$m_area_patio[is.na(properties_2016$area_patio)] <- 1
properties_2016$area_patio <- NULL

properties_2017$m_area_patio <- 0
properties_2017$m_area_patio[is.na(properties_2017$area_patio)] <- 1
properties_2017$area_patio <- NULL


properties_2016$m_area_shed <- 0
properties_2016$m_area_shed[is.na(properties_2016$area_shed)] <- 1
properties_2016$area_shed <- NULL

properties_2017$m_area_shed <- 0
properties_2017$m_area_shed[is.na(properties_2017$area_shed)] <- 1
properties_2017$area_shed <- NULL


properties_2016$m_num_story <- 0
properties_2016$m_num_story[is.na(properties_2016$num_story)] <- 1
properties_2016$num_story <- NULL

properties_2017$m_num_story <- 0
properties_2017$m_num_story[is.na(properties_2017$num_story)] <- 1
properties_2017$num_story <- NULL


properties_2016$m_fips <- 0
properties_2016$m_fips[is.na(properties_2016$fips)] <- 1
properties_2016$fips[is.na(properties_2016$fips)] <- as.numeric(names(which.max(table(properties_2016$fips))))

properties_2017$m_fips <- 0
properties_2017$m_fips[is.na(properties_2017$fips)] <- 1
properties_2017$fips[is.na(properties_2017$fips)] <- as.numeric(names(which.max(table(properties_2017$fips))))

properties_2017$m_heating <- 0
properties_2017$m_heating[is.na(properties_2017$heating)] <- 1
properties_2017$heating[is.na(properties_2017$heating)] <- median(properties_2017$heating[!is.na(properties_2017$heating)])

properties_2017$m_zoning_landuse <- 0
properties_2017$m_zoning_landuse[is.na(properties_2017$zoning_landuse)] <- 1
properties_2017$zoning_landuse[is.na(properties_2017$zoning_landuse)] <- median(properties_2017$zoning_landuse[!is.na(properties_2017$zoning_landuse)])


properties_2017$m_rawcensustractandblock <- 0
properties_2017$m_rawcensustractandblock[is.na(properties_2017$rawcensustractandblock)] <- 1
properties_2017$rawcensustractandblock[is.na(properties_2017$rawcensustractandblock)] <- 
  as.numeric(names(which.max(table(properties_2017$rawcensustractandblock))))

properties_2017$m_region_county <- 0
properties_2017$m_region_county[is.na(properties_2017$region_county)] <- 1
properties_2017$region_county[is.na(properties_2017$region_county)] <- 
  as.numeric(names(which.max(table(properties_2017$region_county))))

properties_2017$m_num_room <- 0
properties_2017$m_num_room[is.na(properties_2017$num_room)] <- 1
properties_2017$num_room[is.na(properties_2017$num_room)] <- 2

properties_2017$m_censustractandblock <- 0
properties_2017$m_censustractandblock[is.na(properties_2017$censustractandblock)] <- 1
properties_2017$censustractandblock[is.na(properties_2017$censustractandblock)] <- 
  as.numeric(names(which.max(table(properties_2017$censustractandblock))))



#############################################################################
# Clean up variables that have incorrect values that haven't been fixed yet #
#############################################################################

# These are all zero but the tax delinquency year is filled in for some of these
properties_2016$tax_delinquency[!is.na(properties_2016$tax_delinquency_year)] <- 1

# Delinquency year is not helpful. Better to use age of delinquency
properties_2016$m_tax_delinquency_year <- 0
properties_2016$m_tax_delinquency_year[is.na(properties_2016$tax_delinquency_year)] <- 1
properties_2016$tax_AgeDelinquency[is.na(properties_2016$tax_delinquency_year)] <- 0
properties_2016$tax_delinquency_year[properties_2016$tax_delinquency_year > 15 & !is.na(properties_2016$tax_delinquency_year)] <- 
  properties_2016$tax_delinquency_year[properties_2016$tax_delinquency_year > 15 & !is.na(properties_2016$tax_delinquency_year)] + 1900
properties_2016$tax_delinquency_year[properties_2016$tax_delinquency_year <= 15 & !is.na(properties_2016$tax_delinquency_year)] <- 
  properties_2016$tax_delinquency_year[properties_2016$tax_delinquency_year <= 15 & !is.na(properties_2016$tax_delinquency_year)] + 2000
properties_2016$tax_AgeDelinquency[!is.na(properties_2016$tax_delinquency_year)] <- 
  2016 - properties_2016$tax_delinquency_year[!is.na(properties_2016$tax_delinquency_year)]
properties_2016$tax_delinquency_year <- NULL


properties_2017$tax_delinquency[!is.na(properties_2017$tax_delinquency_year)] <- 1

# Delinquency year is not helpful. Better to use age of delinquency
properties_2017$m_tax_delinquency_year <- 0
properties_2017$m_tax_delinquency_year[is.na(properties_2017$tax_delinquency_year)] <- 1
properties_2017$tax_AgeDelinquency[is.na(properties_2017$tax_delinquency_year)] <- 0
properties_2017$tax_delinquency_year[properties_2017$tax_delinquency_year > 15 & !is.na(properties_2017$tax_delinquency_year)] <- 
  properties_2017$tax_delinquency_year[properties_2017$tax_delinquency_year > 15 & !is.na(properties_2017$tax_delinquency_year)] + 1900
properties_2017$tax_delinquency_year[properties_2017$tax_delinquency_year <= 15 & !is.na(properties_2017$tax_delinquency_year)] <- 
  properties_2017$tax_delinquency_year[properties_2017$tax_delinquency_year <= 15 & !is.na(properties_2017$tax_delinquency_year)] + 2000
properties_2017$tax_AgeDelinquency[!is.na(properties_2017$tax_delinquency_year)] <- 
  2017 - properties_2017$tax_delinquency_year[!is.na(properties_2017$tax_delinquency_year)]
properties_2017$tax_delinquency_year <- NULL


#######################
# Output File for GBM #
#######################

properties_2016$day <- 1
properties_2016$mo <- 10
oct2016 <- predict.gbm(gbm16.out, newdata = properties_2016, n.trees = 1000)
summary(is.na(oct2016))

properties_2016$day <- 1
properties_2016$mo <- 11
nov2016 <- predict.gbm(gbm16.out, newdata = properties_2016, n.trees = 1000)
summary(is.na(nov2016))

properties_2016$day <- 1
properties_2016$mo <- 12
dec2016 <- predict.gbm(gbm16.out, newdata = properties_2016, n.trees = 1000)
summary(is.na(dec2016))

properties_2017$day <- 1
properties_2017$mo <- 10
oct2017 <- predict.gbm(gbm17.out, newdata = properties_2017, n.trees = 1000)
summary(is.na(oct2017))

properties_2017$day <- 1
properties_2017$mo <- 11
nov2017 <- predict.gbm(gbm17.out, newdata = properties_2017, n.trees = 1000)
summary(is.na(nov2017))

properties_2017$day <- 1
properties_2017$mo <- 12
dec2017 <- predict.gbm(gbm17.out, newdata = properties_2017, n.trees = 1000)
summary(is.na(dec2017))

submission20170526 <- data.frame("ParcelId" = as.integer(properties_2016$id_parcel), "201610" = oct2016,
                                 "201611" = nov2016, "201612" = dec2016, "201710" = oct2017,
                                 "201711" = nov2017, "201712" = dec2017)


colnames(submission20170526) <- c("ParcelId","201610","201611","201612", "201710", "201711", "201712")
write.csv(submission20170526, file = "D:/Kim MSPA/Predict 413/Midterm - Zillow Kaggle/submissionGBM2.csv", row.names = FALSE)

#######################
# Output File for GBM #
#######################

properties_2016$day <- 1
properties_2016$mo <- 10
oct2016 <- predict.gbm(gbm16.out, newdata = properties_2016, n.trees = 1000)
summary(is.na(oct2016))

properties_2016$day <- 1
properties_2016$mo <- 11
nov2016 <- predict.gbm(gbm16.out, newdata = properties_2016, n.trees = 1000)
summary(is.na(nov2016))

properties_2016$day <- 1
properties_2016$mo <- 12
dec2016 <- predict.gbm(gbm16.out, newdata = properties_2016, n.trees = 1000)
summary(is.na(dec2016))

properties_2017$day <- 1
properties_2017$mo <- 10
oct2017 <- predict.gbm(gbm17.out, newdata = properties_2017, n.trees = 1000)
summary(is.na(oct2017))

properties_2017$day <- 1
properties_2017$mo <- 11
nov2017 <- predict.gbm(gbm17.out, newdata = properties_2017, n.trees = 1000)
summary(is.na(nov2017))

properties_2017$day <- 1
properties_2017$mo <- 12
dec2017 <- predict.gbm(gbm17.out, newdata = properties_2017, n.trees = 1000)
summary(is.na(dec2017))

submission20170526 <- data.frame("ParcelId" = as.integer(properties_2016$id_parcel), "201610" = oct2016,
                                 "201611" = nov2016, "201612" = dec2016, "201710" = oct2017,
                                 "201711" = nov2017, "201712" = dec2017)


colnames(submission20170526) <- c("ParcelId","201610","201611","201612", "201710", "201711", "201712")
write.csv(submission20170526, file = "D:/Kim MSPA/Predict 413/Midterm - Zillow Kaggle/submissionGBM4.csv", row.names = FALSE)


#######################
# Output File for LM  #
#######################

str(train$mo)

properties_2016$day <- 1
properties_2016$mo <- 10
oct2016 <- predict(lm16.out, newdata = properties_2016, interval="none")
summary(is.na(oct2016))


properties_2016$day <- 1
properties_2016$mo <- 11
nov2016 <- predict(lm16.out, newdata = properties_2016, interval="none")
summary(is.na(nov2016))

properties_2016$day <- 1
properties_2016$mo <- 12
dec2016 <- predict(lm16.out, newdata = properties_2016, interval="none")
summary(is.na(dec2016))


properties_2017$day <- 1
properties_2017$mo <- 10
oct2017 <- predict(lm17.out, newdata = properties_2017, interval="none")
summary(is.na(oct2017))

properties_2017$day <- 1
properties_2017$mo <- 11
nov2017 <- predict(lm17.out, newdata = properties_2017, interval="none")
summary(is.na(nov2017))

properties_2017$day <- 1
properties_2017$mo <- 12
dec2017 <- predict(lm17.out, newdata = properties_2017, interval="none")
summary(is.na(dec2017))

submission <- data.frame("ParcelId" = as.integer(properties_2016$id_parcel), "201610" = oct2016,
                                 "201611" = nov2016, "201612" = dec2016, "201710" = oct2017,
                                 "201711" = nov2017, "201712" = dec2017)

colnames(submission) <- c("ParcelId","201610","201611","201612", "201710", "201711", "201712")
write.csv(submission, file = "D:/Kim MSPA/Predict 413/Midterm - Zillow Kaggle/submissionLM.csv", row.names = FALSE)


###############################
# Output file for Arima model #
###############################


forecast2016 <- forecast(myarima, xreg=xregt)
forecast2017 <- forecast(myarima17, xreg=xregt17)


oct2016 <- forecast2016$mean[1]
nov2016 <- forecast2016$mean[32]
dec2016 <- forecast2016$mean[62]
oct2017 <- forecast2017$mean[1]
nov2017 <- forecast2017$mean[32]
dec2017 <- forecast2017$mean[62]

submission <- data.frame("ParcelId" = as.integer(properties_2016$id_parcel), "201610" = oct2016,
                         "201611" = nov2016, "201612" = dec2016, "201710" = oct2017,
                         "201711" = nov2017, "201712" = dec2017)


colnames(submission) <- c("ParcelId","201610","201611","201612", "201710", "201711", "201712")
write.csv(submission, file = "D:/Kim MSPA/Predict 413/Midterm - Zillow Kaggle/submissionArima.csv", row.names = FALSE)


###############################
# Output File for Neural Net  #
###############################

properties_2016$day <- 1
properties_2016$mo <- 10
oct2016 <- predict(ANNModel, properties_2016)
summary(is.na(oct2016))

properties_2016$day <- 1
properties_2016$mo <- 11
nov2016 <- predict(ANNModel, properties_2016)
summary(is.na(nov2016))

properties_2016$day <- 1
properties_2016$mo <- 12
dec2016 <- predict(ANNModel, properties_2016)
summary(is.na(dec2016))


properties_2017$day <- 1
properties_2017$mo <- 10
oct2017 <- predict(ANNModel17, properties_2017)
summary(is.na(oct2017))



properties_2017$day <- 1
properties_2017$mo <- 11
nov2017 <- predict(ANNModel17, properties_2017)
summary(is.na(nov2017))

properties_2017$day <- 1
properties_2017$mo <- 12
dec2017 <- predict(ANNModel17, properties_2017)
summary(is.na(dec2017))

submission <- data.frame("ParcelId" = as.integer(properties_2016$id_parcel), "201610" = oct2016,
                         "201611" = nov2016, "201612" = dec2016, "201710" = oct2017,
                         "201711" = nov2017, "201712" = dec2017)


colnames(submission) <- c("ParcelId","201610","201611","201612", "201710", "201711", "201712")

write.csv(submission, file = "D:/Kim MSPA/Predict 413/Midterm - Zillow Kaggle/submissionANN.csv", row.names = FALSE)


###############################
# Output File for xgboost     #
###############################

properties_2016$day <- 1
properties_2016$mo <- 10
df=as.data.frame(properties_2016)[,names(properties_2016) %in% columns_pred  ]
df = data.matrix(df)
oct2016 <- predict(xgb, df)
summary(is.na(oct2016))

properties_2016$day <- 1
properties_2016$mo <- 11
df=as.data.frame(properties_2016)[,names(properties_2016) %in% columns_pred  ]
df = data.matrix(df)
nov2016 <- predict(xgb, df)
summary(is.na(nov2016))

properties_2016$day <- 1
properties_2016$mo <- 12
df=as.data.frame(properties_2016)[,names(properties_2016) %in% columns_pred  ]
df = data.matrix(df)
dec2016 <- predict(xgb, df)
summary(is.na(dec2016))


properties_2017$day <- 1
properties_2017$mo <- 10
df=as.data.frame(properties_2017)[,names(properties_2017) %in% columns_pred  ]
df = data.matrix(df)
oct2017 <- predict(xgb17, df)
summary(is.na(oct2017))


properties_2017$day <- 1
properties_2017$mo <- 11
df=as.data.frame(properties_2017)[,names(properties_2017) %in% columns_pred  ]
df = data.matrix(df)
nov2017 <- predict(xgb17, df)
summary(is.na(nov2017))

properties_2017$day <- 1
properties_2017$mo <- 12
df=as.data.frame(properties_2017)[,names(properties_2017) %in% columns_pred  ]
df = data.matrix(df)
dec2017 <- predict(xgb17, df)
summary(is.na(dec2017))

submission <- data.frame("ParcelId" = as.integer(properties_2016$id_parcel), "201610" = oct2016,
                         "201611" = nov2016, "201612" = dec2016, "201710" = oct2017,
                         "201711" = nov2017, "201712" = dec2017)

head(submission)

colnames(submission) <- c("ParcelId","201610","201611","201612", "201710", "201711", "201712")
write.csv(submission, file = "D:/Kim MSPA/Predict 413/Midterm - Zillow Kaggle/submissionxgBoost3.csv", row.names = FALSE)

