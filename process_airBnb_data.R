library(lubridate); library(data.table); library(dplyr); library(DataExplorer)


# Load training and test data sets
trainZ <- read.csv("~/Misc/AirBnb/train.csv", stringsAsFactors = FALSE)
testZ <- read.csv("~/Misc/AirBnb/test.csv", stringsAsFactors = FALSE)

# Rental price (log) appears to be normally distributed
summary(trainZ$log_price)
hist(trainZ$log_price)

# Does not appear as any unqiue property types or room types need to be combined/manipulated
table(trainZ$property_type)
table(trainZ$room_type)

# Look for certain amenities and create dummy columns for each of those
# Pool, wireless, fireplace, parking, air conditioning, pets allowed, washer
amenitiesFunc <- function(x){
  pool <- ifelse(grepl("pool|Pool|POOL", x$amenities), 1,0)
  wireless <- ifelse(grepl("wireless|Wireless|WIRELESS",x$amenities), 1, 0)
  fireplace <- ifelse(grepl("fireplae","Fireplace", x$amenities), 1, 0)
  parking <- ifelse(grepl("parking|Parking|PARKING", x$amenities), 1, 0)
  ac <- ifelse(grepl("air conditioning|Air Conditioning", x$amenities), 1, 0)
  pets <- ifelse(grepl("Pets allowed", x$amenities), 1, 0)
  washer <- ifelse(grepl("Washer|washer", x$amenities), 1, 0)
  
  return(list(pool=pool, wireless=wireless, fireplace=fireplace, parking=parking, ac=ac, pets=pets, washer=washer))
}

trainZ <- as.data.table(trainZ)
trainZ[,c("pool", "wireless", "fireplace", "parking", "ac", "pets", "washer") := amenitiesFunc(trainZ)]


# Convert all blank values to NAs for character fields taht we would be interested in using in our model
charVars <- c("property_type","room_type","bed_type","cancellation_policy","cleaning_fee","city","host_has_profile_pic","host_identity_verified","instant_bookable",
              "neighbourhood")

blankFun <- function(x){
  gsub("^$|^ $", NA, x)
}

trainZ <- as.data.frame(trainZ)
trainZ[,charVars] <- apply(trainZ[,charVars], 2, blankFun)


trainZ$host_response_rate <- as.numeric(sub("%", "", trainZ$host_response_rate))

# Convert 'host_since' character value to length of time (in years)
trainZ$host_since <- as.Date(trainZ$host_since, '%Y-%m-%d')
trainZ$host_year_duration <- as.numeric(Sys.Date()-trainZ$host_since)/365

# Neighborhood field appears to be highly dimensional, but clean in it's descriptions. Meaning no tidying appears to be required
table(trainZ$neighbourhood)

# Identify fields that we are interested in using for modeling purposes - We will filter down the number of variables used later in the process
modelVars <- c("id","log_price","property_type","room_type","accommodates","bathrooms","bed_type","cancellation_policy","city","host_has_profile_pic",
               "host_identity_verified","host_response_rate","instant_bookable","neighbourhood","number_of_reviews","review_scores_rating","bedrooms","beds","pool",
               "wireless","fireplace","parking","ac","pets","washer","host_year_duration")

trainZ <- as.data.table(trainZ)
trainDf <- trainZ[, ..modelVars]


