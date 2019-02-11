options(java.parameters = "-Xmx30g")
library(lubridate); library(h2o); library(data.table); library(dplyr); library(DataExplorer); library(plotly)

# assess number of missing values
plot_missing(trainDf)

# Create factor variables
factorVars <- c("property_type","room_type","bed_type","cancellation_policy","city","host_has_profile_pic","host_identity_verified","instant_bookable",
                "neighbourhood","pool","wireless","fireplace","parking","ac","pets","washer")

trainDf <- as.data.frame(trainDf)
trainDf[factorVars] <- lapply(trainDf[factorVars], factor)


# Separate training data into training, validation, and testing
trainEnd <- round(nrow(trainDf)*0.7)
validateStart <- trainEnd+1
validateEnd <- round(nrow(trainDf)*0.85)
testStart <- validateEnd+1

trainGbm <- trainDf[1:trainEnd,]
validateGbm <- trainDf[validateStart:validateEnd,]
testGbm <- trainDf[testStart:nrow(trainDf),]

# Initialize h2o session and convert training, validation, and testing data frames to h2o objects
h2o.init()

trainH2o <- as.h2o(trainGbm)
validateH2o <- as.h2o(validateGbm)
testH2o <- as.h2o(testGbm)

##### Perform grid search on training & validation data sets
y <- "log_price"
x <- c("property_type","room_type","accommodates","bathrooms","bed_type","cancellation_policy","city","host_has_profile_pic",
       "host_identity_verified","host_response_rate","instant_bookable","neighbourhood","number_of_reviews","review_scores_rating","bedrooms","beds","pool",
       "wireless","fireplace","parking","ac","pets","washer","host_year_duration")

# Set GBM hyperparamters
gbm_params <- list(learn_rate = 0.01,
                   max_depth = c(7, 13), # increase max_depth to accommodate large number of independent variables
                   sample_rate = 0.8,
                   col_sample_rate = 0.8, # column sampling isn't critical as missing values are only common in 3 fields
                   ntrees = c(300, 400, 500),
                   min_rows = c(5, 10)) # reduce min_rows to accommodate small sample set

# Train and validate a grid of GBMs
airBnb_gbm_gs <- h2o.grid("gbm", x = x, y = y,
                          training_frame = trainH2o,
                          validation_frame = validateH2o,
                          seed = 33233,
                          hyper_params = gbm_params)

# Get the grid results, sorted by validation RMSE
grid_search_results <- h2o.getGrid(grid_id = airBnb_gbm_gs@grid_id, sort_by = "RMSE")
top_airBnb_model <- grid_search_results@summary_table[1,]


# Merge training and validation data sets in order to train GBM model on a larger data set now that we have the 
# appropriate hyper parameters selected
trainFullGbm <- rbind(trainGbm, validateGbm)
trainFullH2o <- as.h2o(trainFullGbm)

## Apply appropriate hyper parameters to model and review variable importance as determined by the residual sum of squares 
airBnb_gbm <- h2o.gbm(x = x,
                      y = y,
                      training_frame = trainFullH2o,
                      sample_rate = 0.8,
                      col_sample_rate = 0.8,
                      ntrees = 500,
                      learn_rate = .01,
                      max_depth = 7,
                      min_rows = 5)


# Obtain variable importance
h2o.varimp(airBnb_gbm)
h2o.varimp_plot(airBnb_gbm)



# If desired for production purposes, re-train model using only the variables with >= .001 importance to reduce complexity
# Perform grid searh with updated set of hyper parameters
y <- "log_price"
x2 <- c("property_type","room_type","accommodates","bathrooms","cancellation_policy","city",
        "host_response_rate","neighbourhood","number_of_reviews","review_scores_rating","bedrooms","beds","pool",
        "washer","host_year_duration")

# Set GBM hyperparamters
gbm_params2 <- list(learn_rate = 0.01,
                    max_depth = c(7, 10), # increase max_depth to accommodate large number of independent variables
                    sample_rate = 0.8,
                    col_sample_rate = 0.8, # column sampling isn't critical as missing values are only common in 3 fields
                    ntrees = c(450, 500, 550),
                    min_rows = c(5, 7)) # reduce min_rows to accommodate small sample set

# Train and validate a grid of GBMs
airBnb_gbm_gs2 <- h2o.grid("gbm", x = x2, y = y,
                           training_frame = trainH2o,
                           validation_frame = validateH2o,
                           seed = 33233,
                           hyper_params = gbm_params2)

# Get the grid results, sorted by validation RMSE
grid_search_results2 <- h2o.getGrid(grid_id = airBnb_gbm_gs2@grid_id, sort_by = "RMSE")
top_airBnb_model2 <- grid_search_results2@summary_table[1,]


# Re-train model with reduced number of inputs
airBnb_gbm_final <- h2o.gbm(x = x2,
                            y = y,
                            training_frame = trainFullH2o,
                            sample_rate = 0.8,
                            col_sample_rate = 0.8,
                            ntrees = 550,
                            learn_rate = .01,
                            max_depth = 7,
                            min_rows = 7)



model_file <- h2o.download_mojo(airBnb_gbm_final, path="~/Misc/AirBnb/", get_genmodel_jar=TRUE)


# Predict on Clean Test Set
airBnbPreds <- as.data.frame(h2o.predict(object = airBnb_gbm_final, newdata = testH2o))
test_gbm_results <- cbind(testGbm, airBnbPreds)

