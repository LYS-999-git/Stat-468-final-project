library(vetiver)
library(aws.s3)
library(duckdb)
library(face)
library(readr)
library(tibble)
library(jsonlite)
library(tidyverse)
library(dplyr)
library(fields)
library(digest)
library(pins)



bucket <- Sys.getenv("S3_BUCKET")
prefix <- Sys.getenv("S3_PREFIX")

# read data
# Download the .rds file from S3
obj <- get_object("projects/stat468/data/fd_data.rds", 
                  bucket = Sys.getenv("S3_BUCKET"))

# Save to a temporary file
tmp <- tempfile(fileext = ".rds")
writeBin(obj, tmp)

# Load the R object
fd_data <- readRDS(tmp)

# # write to duckDB
# con <- dbConnect(duckdb::duckdb())
# duckdb::dbWriteTable(con, "fd_data", fd_data)
# 
# dbGetQuery(con, "SELECT * FROM fd_data LIMIT 2")


#Read model from raw object
fit_face <- s3read_using(readRDS,
                      object = "projects/stat468/models/fit_face.rds",
                      bucket = Sys.getenv("S3_BUCKET"))
orig_class <- class(fit_face)
# class(fit_face) <- c("face_sparse", orig_class)

# Function for predict one athlete for shiny app
predict_sparse<- function(x_id, fit_model) {
  print(paste("Predicting for athlete:", x_id))
  
  # initial dataframe
  id <- fd_data$athlete_id
  uid <- unique(id)
  
  # ages where we predict the score
  seq <- 11:58
  # preprare data used for prediction
  k <- length(seq)
  data <- data.frame(y=fd_data$iaaf_score, argvals = fd_data$performance_age, subj = fd_data$athlete_id)
  data.h <- data
  sel <- which(id == x_id)
  dati <- data.h[sel,]
  # print("Selection:")
  # print(dati)
  

  #Create the data frame for prediction
  dati_pred <- data.frame(y = rep(NA, nrow(dati) + k),
                          argvals = c(rep(NA, nrow(dati)), seq),
                          subj = rep(dati$subj[1], nrow(dati) + k ))
  # print("timstamp 2")
  # print(dati_pred[1:nrow(dati),])
  # print(dati)
  
  #Fill the first part of the data set with the observations for the subject that will be predicted
  dati_pred[1:nrow(dati),] <- dati
  print("timstamp 3")
  #Produce the predictions for subject i
  yhat2 <- predict(fit_model, dati_pred)
  print("timstamp 4")
  
  Ord  <- (nrow(dati) + 1):(nrow(dati) + k)
  # print("Result:")
  # print(yhat2$y.pred[Ord])
  return(yhat2$y.pred[Ord])
}

# Defined prediction handler for vetiver
handler_predict.face.sparse <- function(v) {
  force(v)
  function(data) {
    ids <- as.character(unlist(data$athlete_id))

    if (!length(ids)) {
      return(tibble(error = "Send JSON like: [{\"athlete_id\":\"linkAndEvent\"}]"))
    }

    print(paste("Received request for athlete IDs:", paste(ids, collapse = ", ")))

    preds <- lapply(ids, function(id) predict_sparse(id, v$model))

    tibble(
      prediction = preds
    )
  }
}

# Create vetiver model object
v_fit_face <- vetiver_model(fit_face, 
                         model_name = "fit_face",
                         description = "STAT468 face.sparse model (RDS from S3)",
                         save_prototype = FALSE)

# Serve with plumber
pr <- plumber::pr()
pr <- vetiver_api(pr, v_fit_face,
                  check_prototype = FALSE) 
pr$run(host = "0.0.0.0", port = 8000)
# pr$run(host = "127.0.0.1", port = 8000)

# 
# curl -X POST http://127.0.0.1:8000/predict \
# -H 'Content-Type: application/json' \
# -d '{"data":[{"athlete_id":"https://worldathletics.org/athletes/zimbabwe/sharon-tavengwa-014325382 Road Marathon"}]}'
