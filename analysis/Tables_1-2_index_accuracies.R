# We explored using five different vegetation indices in this study described in Table S1 of our manuscript.
# The search grid for each vegetation index is stored in data/intermediate/index_parameters. In this script, we find the
# optimal accuracy on the training data set and the corresponding accuracy on the validation data set for each
# vegetation index and display the resulting table. The tables we read in for this script are computed in 
# hpc/sierra/align_trees/XXXX_01_grid_search.R and hpc/sierra/align_trees/XXXX_02_optim_params.R where
# XXXX represents the vegetation index (e.g. ndvi). 

library(dplyr)

df_list <- list()

## NDVI
ndvi_train <- read.csv("data/intermediate/index_parameters/ndvi_indices_labels_train.csv")

idx <- which.max(ndvi_train$acc)

ndvi_train <- ndvi_train[idx,]
ndvi_train

ndvi_valid <- read.csv("data/intermediate/index_parameters/ndvi_indices_labels_valid.csv")
ndvi_valid
df_list[[1]] <- data.frame(idx = "NDVI", thr = ndvi_train$ndvi, prop = ndvi_train$prop, train_acc = ndvi_train$acc, valid_acc = ndvi_valid$acc, sensitivity = (ndvi_valid$count11)/(ndvi_valid$count11 + ndvi_valid$count10),
                         specificity = (ndvi_valid$count00)/(ndvi_valid$count00 + ndvi_valid$count01))

## NDMI1

ndmi1_train <- read.csv("data/intermediate/index_parameters/ndmi1_indices_labels_train.csv")

idx <- which.max(ndmi1_train$acc)

ndmi1_train <- ndmi1_train[idx,]
ndmi1_train

ndmi1_valid <- read.csv("data/intermediate/index_parameters/ndmi1_indices_labels_valid.csv")
ndmi1_valid

df_list[[2]] <- data.frame(idx = "NDMI1", thr = ndmi1_train$ndmi1, prop = ndmi1_train$prop, train_acc = ndmi1_train$acc, valid_acc = ndmi1_valid$acc, sensitivity = (ndmi1_valid$count11)/(ndmi1_valid$count11 + ndmi1_valid$count10),
                         specificity = (ndmi1_valid$count00)/(ndmi1_valid$count00 + ndmi1_valid$count01))

## NDMI

ndmi_train <- read.csv("data/intermediate/index_parameters/ndmi_indices_labels_train.csv")

idx <- which.max(ndmi_train$acc)

ndmi_train <- ndmi_train[idx,]
ndmi_train

ndmi_valid <- read.csv("data/intermediate/index_parameters/ndmi_indices_labels_valid.csv")
ndmi_valid

df_list[[3]] <- data.frame(idx = "NDMI", thr = ndmi_train$ndmi, prop = ndmi_train$prop, train_acc = ndmi_train$acc, valid_acc = ndmi_valid$acc, sensitivity = (ndmi_valid$count11)/(ndmi_valid$count11 + ndmi_valid$count10),
                         specificity = (ndmi_valid$count00)/(ndmi_valid$count00 + ndmi_valid$count01))

## NDMI2

ndmi2_train <- read.csv("data/intermediate/index_parameters/ndmi2_indices_labels_train.csv")

idx <- which.max(ndmi2_train$acc)

ndmi2_train <- ndmi2_train[idx,]
ndmi2_train

ndmi2_valid <- read.csv("data/intermediate/index_parameters/ndmi2_indices_labels_valid.csv")
ndmi2_valid

df_list[[4]] <- data.frame(idx = "NDMI2", thr = ndmi2_train$ndmi2, prop = ndmi2_train$prop, train_acc = ndmi2_train$acc, valid_acc = ndmi2_valid$acc, sensitivity = (ndmi2_valid$count11)/(ndmi2_valid$count11 + ndmi2_valid$count10),
                         specificity = (ndmi2_valid$count00)/(ndmi2_valid$count00 + ndmi2_valid$count01))

# Relative Greenness
rgreen_train <- read.csv("data/intermediate/index_parameters/rgreen_indices_labels_train.csv")

idx <- which.max(rgreen_train$acc)

rgreen_train <- rgreen_train[idx,]
rgreen_train

rgreen_valid <- read.csv("data/intermediate/index_parameters/rgreen_indices_labels_valid.csv")
rgreen_valid

df_list[[5]] <- data.frame(idx = "RGREEN", thr = rgreen_train$rgreen, prop = rgreen_train$prop, train_acc = rgreen_train$acc, valid_acc = rgreen_valid$acc, sensitivity = (rgreen_valid$count11)/(rgreen_valid$count11 + rgreen_valid$count10),
                         specificity = (rgreen_valid$count00)/(rgreen_valid$count00 + rgreen_valid$count01))

df_all <- do.call(rbind.data.frame, df_list)

# Values for Table 1
df_all <- arrange(df_all, desc(train_acc))
df_all

# Collect the training, validation, and test confusion matrix values for Table 2
rgreen_test <- read.csv("data/intermediate/index_parameters/rgreen_optim_params_test.csv")

# Table 2 Values
rgreen_train
rgreen_valid
rgreen_test
