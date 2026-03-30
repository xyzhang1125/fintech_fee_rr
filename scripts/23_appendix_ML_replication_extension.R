#--------------------------------------------------------------------------------------------
# File name: 		      23_appendix_ML_replication_extension.R
# Creation date:      
# Author:          		Nicholas Min
# Files used:
# 	- here("proc", "balance_table_data.csv")
# 	- here("proc", "fintech_fee_light.csv")
# 	- here("proc", "rmse_ridge_nowinsor_std_agemeanimpute.csv")
# 	- here("proc", "rmse_ridge_winsortopX1_std_agemeanimpute.csv")
# 	- here("proc", "rmse_ridge_winsortopX1_nostd_agemeanimpute.csv")
# 	- here("proc", "rmse_ridge_nowinsor_nostd_agemeanimpute.csv")
# Files created:
# 	- here("proc", "rmse_ridge_winsortopX1_std_agemeanimpute.csv")
# 	- here("proc", "rmse_ridge_nowinsor_std_agemeanimpute.csv")
# 	- here("proc", "rmse_ridge_winsortopX1_nostd_agemeanimpute.csv")
# 	- here("proc", "rmse_ridge_nowinsor_nostd_agemeanimpute.csv")
# 	- here("results", "figures", "heatmap_percent_betterrmse.eps")
# 	- here("results", "numbers", "percent_to_number_heatmap_4_1.tex")
# 	- here("results", "numbers", "percent_heatmap_4_1.tex")
# 	- here("results", "numbers", "length_25.tex")
# 	- here("results", "numbers", "length_med.tex")
# 	- here("results", "numbers", "length_mean.tex")
# 	- here("results", "numbers", "length_75.tex")
# 	- here("results", "numbers", "percent_negative_25.tex")
# 	- here("results", "numbers", "percent_negative_med.tex")
# 	- here("results", "numbers", "percent_negative_mean.tex")
# 	- here("results", "numbers", "percent_negative_75.tex")
# Purpose:
# 	- Figure D.1: Percent of OOS RMSEs lower than not winsorized or standardized (1,1) OOS RMS
#--------------------------------------------------------------------------------------------

#table of contents:
#1. compute rmses for 4 different models (1.1 - 1.4)
#2. create rmse rank table
#3. take derivative and bootstrap - for (2,2) non-standardized, non-W model

#note 1: each section is self-contained and can be run independently
#note 2: the first section is computationally intensive and takes a long time to run.
#note 3: tested with several other number of random seeds for robust.
#################### Import packages ###########################################
rm(list=ls())
library(glmnet)
library(doMC)
library(gt)
library(gridExtra)
library(foreach)
library(doParallel)
library(showtext)
library(extrafont)
library(ggtext)
library(here)
library(tidyverse)
library(lubridate)
library(magrittr)
library(fixest)
library(ggplot2)
library(scales)
library(tabulator)
library(DoubleML)
library(ranger)
library(mlr3)
library(tictoc)
options(readr.show_col_types = FALSE)
source(here("scripts", "programs", "winsorize.R"))
################################################################################

################################################################################
#1. compute rmses for 4 different models (1.1 - 1.4)############################
# note from Nico: takes a long time to run
# advice: skip this and go to 2. create rmse rank table
################################################################################

# #1.1 winsorized all cov and X1 (top 5%), standardized, age mean imputed
# ##################################################
# ##    (1): Process data and obtain covariates.  ##
# ##################################################
# rm(list=ls())
# num_seeds <- 200
# # Register parallel backend
# num_cores <- detectCores() - 24  # Use one less than the total number of cores
# cl <- makeCluster(num_cores)
# registerDoParallel(cl)

# # (1.1): Import balance table data.
# balance_data <- read_csv(here("proc", "balance_table_data.csv")) %>% 
#   mutate(antrem = ifelse(anticipated_reminder, "Anticipated reminder", "Unanticipated reminder"))

# # (1.2): Fix observations without owner age.
# balance_data %<>% mutate(owner_age = ifelse(is.na(owner_age), 0, owner_age))

# # (1.3): Restrict data to anticipated / unanticipated reminder
# balance_data_antrem <- balance_data %>% 
#   filter(anticipated_reminder == 1 | unanticipated_reminder == 1)

# # (1.3.5): impute age
# age_average <- mean(balance_data_antrem$owner_age[balance_data_antrem$owner_age>0])
# age_average
# balance_data_antrem$owner_age[which(balance_data_antrem$owner_age==0)] <- age_average


# # (1.4): Define vector with covariates. # We actually have w5 vars in the balance test data
# # remove other type to avoid multicollinearity
# baseline_covariates <- c("owner_sex_female",  "owner_age", "owner_age_unknown",
#                          "new_buss_type_beauty", "new_buss_type_clothing", "new_buss_type_professionals", 
#                          "new_buss_type_restaurants", "new_buss_type_small_retailers",
#                          "make_sale", "log_valid_volume", "log_nr_valid_payments")

# baseline_X1 <- c("months_since_ft")


# # (1.5): winsorization
# # define winsorize function for tidyverse
# winsorize <- function(data, var, newvar, w = 5, highonly = FALSE, na.rm = TRUE) {
#   var_vector <- data[[var]]
  
#   if (na.rm) {
#     var_vector <- var_vector[!is.na(var_vector)]
#   }
  
#   if (highonly) {
#     upper <- quantile(var_vector, 1 - w/100, na.rm = na.rm)
#     data[[newvar]] <- ifelse(data[[var]] > upper, upper, data[[var]])
#   } else {
#     lower <- quantile(var_vector, w/100, na.rm = na.rm)
#     upper <- quantile(var_vector, 1 - w/100, na.rm = na.rm)
#     data[[newvar]] <- ifelse(data[[var]] < lower, lower, 
#                             ifelse(data[[var]] > upper, upper, data[[var]]))
#   }
  
#   return(data)
# }
# # define winsor var list
# winsor_vars <- c("months_since_ft", "owner_age", "make_sale", 
#                  "log_valid_volume", "log_nr_valid_payments")

# # check if any var < 0
# check_highonly <- function(data, var) {
#   if (any(data[[var]] < 0, na.rm = TRUE)) {
#     return(FALSE)
#   } else {
#     return(TRUE)
#   }
# }

# # winsorization
# for (var in winsor_vars) {
#   # check first
#   highonly <- check_highonly(balance_data_antrem, var)

#   balance_data_antrem <- winsorize(
#     data = balance_data_antrem,
#     var = var,
#     newvar = paste0(var, "_w5"), 
#     w = 5,
#     highonly = highonly,
#     na.rm = TRUE
#   )
# }

# winsorized_vars <- paste0(winsor_vars, "_w5")

# # update the covariate matrix
# balance_data_antrem_covariates_winsor <- as.matrix(
#   balance_data_antrem %>%
#     mutate(
#       owner_age = owner_age_w5,
#       make_sale = make_sale_w5,
#       log_valid_volume = log_valid_volume_w5,
#       log_nr_valid_payments = log_nr_valid_payments_w5
#     ) %>%
#     select(all_of(baseline_covariates))
# )

# balance_data_antrem_X1_winsor <- as.matrix(
#   balance_data_antrem %>%
#     select(months_since_ft_w5)
# )

# # (1.6): Create degree=3 polynomial for covariance (due to computation time) & degree=5 polynomial for X1
# #note: poly() gives ALL polynomials (and interactions) under the #degrees

# #toy e.g.:
# #asdf <- as.matrix(cbind(rep(1,100),rep(2,100),rep(3,100)))
# #asdf_poly <- as.data.frame(poly(asdf, degree =3, raw=TRUE))

# polynomial_data_covariates <-as.data.frame(
#   poly(balance_data_antrem_covariates_winsor , degree = 4, raw=TRUE))

# polynomial_data_X1 <-as.data.frame(
#   poly(balance_data_antrem_X1_winsor , degree = 5, raw=TRUE))

# colnames(polynomial_data_X1) <- c("X1","X1^2","X1^3","X1^4","X1^5")

# # (1.7): scale covariates and X1
# polynomial_data_covariates_scaled <- scale(polynomial_data_covariates, scale = TRUE)
# polynomial_data_X1_scaled <- scale(polynomial_data_X1, scale = TRUE)

# # (1.8): take out NaN columns
# # would be necessary if we scale=TRUE above because we divide by sd=0 if all elements in two columns are zeroes.
# # reduces redundant cols and reduce computations going forward

# polynomial_data_covariates_scaled <- polynomial_data_covariates_scaled[ , colSums(is.na(polynomial_data_covariates_scaled)) == 0] 
# polynomial_data_X1_scaled <- polynomial_data_X1_scaled[ , colSums(is.na(polynomial_data_X1_scaled)) == 0] 

# dim(polynomial_data_covariates_scaled) #dim reduced.

# # (1.9): now introduce T
# polynomial_data_covariates_scaled_T <- cbind(balance_data_antrem$anticipated_reminder,polynomial_data_covariates_scaled)
# polynomial_data_X1_scaled_T <- cbind(balance_data_antrem$anticipated_reminder,polynomial_data_X1_scaled)
# dim(polynomial_data_covariates_scaled_T) #obviously, +1 from above!
# dim(polynomial_data_X1_scaled_T)#obviously, +1 from above!

# colnames(polynomial_data_covariates_scaled_T)[1] <- "anticipated_reminder"
# colnames(polynomial_data_X1_scaled_T)[1] <- "anticipated_reminder"

# #convert to df (so that the below function works)
# polynomial_data_covariates_scaled_T <- as.data.frame(polynomial_data_covariates_scaled_T)
# polynomial_data_X1_scaled_T <- as.data.frame(polynomial_data_X1_scaled_T)

# # (1.10): interact covariate/X1 polynomials with T
# gen_interactions <- function(df, var) {
#   new_var <- str_replace(var, ":", "_")
#   var1 <- str_split(var, ":")[[1]][1]
#   var2 <- str_split(var, ":")[[1]][2]
#   df %<>% mutate(!!new_var := eval(as.name(var1)) * eval(as.name(var2)))
#   return(df)
# }

# interactions_covariates_T <- str_subset(combn(colnames(polynomial_data_covariates_scaled_T), 2, FUN = paste, collapse = ":"), "anticipated_reminder")
# all_covariates_T <- colnames(polynomial_data_covariates_scaled_T) %>% append(interactions_covariates_T)

# interactions_X1_T <- str_subset(combn(colnames(polynomial_data_X1_scaled_T), 2, FUN = paste, collapse = ":"), "anticipated_reminder")
# all_X1_T <- colnames(polynomial_data_X1_scaled_T) %>% append(interactions_X1_T)

# system.time(
#   for (int in interactions_covariates_T) {
#     polynomial_data_covariates_scaled_T %<>% gen_interactions(int)
#   })
# system.time(
#   for (int in interactions_X1_T) {
#     polynomial_data_X1_scaled_T %<>% gen_interactions(int)
#   })

# dim(polynomial_data_covariates_scaled_T)
# dim(polynomial_data_X1_scaled_T)

# # (1.11): Import treatment and acceptance status, merge data.
# fintech_fee <- read_csv(here("proc", "fintech_fee_light.csv")) %>%
#   select(organization_uuid,"accepted_offer_late")

# ##################################################
# ##    (2): fit glmnet   ##
# ##################################################

# #(2.1) define variables
# X_df_scaled_all <- cbind(polynomial_data_X1_scaled_T, 
#                          polynomial_data_covariates_scaled_T %>% select(-anticipated_reminder)) #get rid of anticipated reminder in covariate matrix (to avoid redundancy)


# #(2.1) define variables (cont.)
# X <- as.matrix(X_df_scaled_all)#here, includes the treatment W too.
# X <- X[order(X_df_scaled_all$X1),]  #order by months_usage so that we can downweight outliers in months of usage

# org_id <- as.data.frame(balance_data_antrem$organization_uuid)
# colnames(org_id) <- "organization_uuid"
# Y <- as.matrix(left_join(org_id, fintech_fee, by="organization_uuid") %>% select(accepted_offer_late))
# Y <- Y[order(X_df_scaled_all$X1),]  #order by months_usage so that we can downweight outliers in months of usage



# #(2.2) fit glmnet (using default 10-fold CV) and plot
# wts <- c(rep(1,14629)) #downweight outliers (top/bottom 5%)

# # Function to compute RMSE
# compute_rmse <- function(actual, predicted) {
#   n <- length(actual)
#   squared_errors <- (actual - predicted)^2
#   mean_squared_error <- sum(squared_errors) / n
#   rmse <- sqrt(mean_squared_error)
#   return(rmse)
# }


# ##################################################
# ##    (2): fit glmnet   ##
# ##################################################
# n_row <- num_seeds
# degree_covariate_range <- 1:4
# degree_X1_range <- 1:5


# #(2.0) preprocess X1 and covariates based on #degree polynomials

# system.time({
#   foreach(i = 1:n_row, .packages = c('glmnet', 'dplyr', 'stringr', 'tidyverse'), .combine = 'c') %dopar% {
#     j <- 1
    
#     # New seed
#     set.seed(i + 42)
#     test_index <- sample(1:nrow(X), 0.1 * nrow(X))
#     train_index <- setdiff(seq(1, nrow(X)), test_index)
#     Y_test <- Y[test_index]
#     Y_train <- Y[train_index]
    
#     results <- vector("list", length(degree_covariate_range) * length(degree_X1_range))
    
#     for (degree_covariate in degree_covariate_range) {
#       for (degree_X1 in degree_X1_range) {
        
#         # X1
#         all_cols_X1 <- colnames(polynomial_data_X1_scaled_T)
#         default_cols_X1 <- "^anticipated_reminder$|^X1$|^anticipated_reminder_X1$"
#         additional_cols_X1 <- paste(paste0("\\^", 1:degree_X1, collapse = ), collapse = "|")
#         selected_X1 <- paste0(default_cols_X1, "|", additional_cols_X1)
#         polynomial_data_X1_scaled_T_selected <- polynomial_data_X1_scaled_T %>%
#           select(all_of(str_subset(all_cols_X1, selected_X1)))
        
#         # Covariates
#         all_cols_cov <- colnames(polynomial_data_covariates_scaled_T)
#         dimensions_cov <- sapply(sapply(str_extract_all(all_cols_cov, "1|2|3|4"), as.numeric), sum)
#         index_degree_less_than <- which(dimensions_cov <= degree_covariate & dimensions_cov > 0)
#         polynomial_data_covariates_scaled_T_selected <- polynomial_data_covariates_scaled_T[, index_degree_less_than]
        
#         # Define variables
#         X_df_scaled_all_selected <- cbind(polynomial_data_X1_scaled_T_selected,
#                                           polynomial_data_covariates_scaled_T_selected)
        
#         # Define variables (cont.)
#         X_selected <- as.matrix(X_df_scaled_all_selected)
#         X_selected <- X_selected[order(polynomial_data_X1_scaled_T$X1), ]  # Order by months_usage
#         X_selected_test <- X_selected[test_index, ]
#         X_selected <- X_selected[train_index, ]
        
#         # Fit glmnet (using default 10-fold CV)
#         cvfit_ridge_selected <- cv.glmnet(X_selected, Y_train, type.measure = "mse", alpha = 0, weights = wts, parallel = TRUE)
        
#         # OOS RMSE using test set
#         Y_pred_ridge <- predict(cvfit_ridge_selected, X_selected_test, s = "lambda.min")
        
#         # Compute RMSE
#         rmse_ridge <- compute_rmse(Y_test, Y_pred_ridge)
        
#         results[[j]] <- rmse_ridge
        
#         j <- j + 1
#       }
#     }
#     results
#   } -> rmse_ridge_results
  
#   # Combine the results into the rmse_ridge_matrix
#   rmse_ridge_matrix <- do.call(rbind, rmse_ridge_results)
# })

# # Stop the parallel cluster
# stopCluster(cl)

# # format rmse_ridge_matrix -- 20 columns
# rmse_ridge_matrix <- matrix(rmse_ridge_matrix, ncol = 20, byrow = TRUE)

# write.csv(rmse_ridge_matrix, here("proc","rmse_ridge_winsortopX1_std_agemeanimpute.csv"))

# #1.2 not winsorized, standardized, age mean imputed

# ##################################################
# ##    (1): Process data and obtain covariates.  ##
# ##################################################
# rm(list=ls())
# num_seeds <- 200
# # Register parallel backend
# num_cores <- detectCores() - 24  # Use one less than the total number of cores
# cl <- makeCluster(num_cores)
# registerDoParallel(cl)

# # (1.1): Import balance table data.
# balance_data <- read_csv(here("proc", "balance_table_data.csv")) %>% 
#   mutate(antrem = ifelse(anticipated_reminder, "Anticipated reminder", "Unanticipated reminder"))

# # (1.2): Fix observations without owner age.
# balance_data %<>% mutate(owner_age = ifelse(is.na(owner_age), 0, owner_age))

# # (1.3): Restrict data to anticipated / unanticipated reminder
# balance_data_antrem <- balance_data %>% 
#   filter(anticipated_reminder == 1 | unanticipated_reminder == 1)

# # (1.3.5): impute age
# age_average <- mean(balance_data_antrem$owner_age[balance_data_antrem$owner_age>0])
# age_average
# balance_data_antrem$owner_age[which(balance_data_antrem$owner_age==0)] <- age_average


# # (1.4): Define vector with covariates.
# # remove other type to avoid multicollinearity
# baseline_covariates <- c("owner_sex_female",  "owner_age", "owner_age_unknown",
#                          "new_buss_type_beauty", "new_buss_type_clothing", "new_buss_type_professionals", 
#                          "new_buss_type_restaurants", "new_buss_type_small_retailers", 
#                          "make_sale", "log_valid_volume", "log_nr_valid_payments")

# baseline_X1 <- c( "months_since_ft")

# # (1.5): Create df using the covariates defined above
# balance_data_antrem_covariates <- as.matrix(balance_data_antrem %>% select(all_of(baseline_covariates)))
# balance_data_antrem_X1 <- as.matrix(balance_data_antrem %>% select(all_of(baseline_X1)))

# #WINSORIZE -- X1 and COV
# balance_data_antrem_covariates_winsor <- balance_data_antrem_covariates
# balance_data_antrem_X1_winsor <- balance_data_antrem_X1



# # (1.6): Create degree=3 polynomial for covariance (due to computation time) & degree=5 polynomial for X1
# #note: poly() gives ALL polynomials (and interactions) under the #degrees

# #toy e.g.:
# #asdf <- as.matrix(cbind(rep(1,100),rep(2,100),rep(3,100)))
# #asdf_poly <- as.data.frame(poly(asdf, degree =3, raw=TRUE))

# polynomial_data_covariates <-as.data.frame(
#   poly(balance_data_antrem_covariates_winsor , degree = 4, raw=TRUE))

# polynomial_data_X1 <-as.data.frame(
#   poly(balance_data_antrem_X1_winsor , degree = 5, raw=TRUE))

# colnames(polynomial_data_X1) <- c("X1","X1^2","X1^3","X1^4","X1^5")

# # (1.7): scale covariates and X1
# polynomial_data_covariates_scaled <- scale(polynomial_data_covariates, scale = TRUE)
# polynomial_data_X1_scaled <- scale(polynomial_data_X1, scale = TRUE)
# # (1.8): take out NaN columns
# # would be necessary if we scale=TRUE above because we divide by sd=0 if all elements in two columns are zeroes.
# # reduces redundant cols and reduce computations going forward

# polynomial_data_covariates_scaled <- polynomial_data_covariates_scaled[ , colSums(is.na(polynomial_data_covariates_scaled)) == 0] 
# polynomial_data_X1_scaled <- polynomial_data_X1_scaled[ , colSums(is.na(polynomial_data_X1_scaled)) == 0] 

# dim(polynomial_data_covariates_scaled) #dim reduced.

# # (1.8): now introduce T
# polynomial_data_covariates_scaled_T <- cbind(balance_data_antrem$anticipated_reminder,polynomial_data_covariates_scaled)
# polynomial_data_X1_scaled_T <- cbind(balance_data_antrem$anticipated_reminder,polynomial_data_X1_scaled)
# dim(polynomial_data_covariates_scaled_T) #obviously, +1 from above!
# dim(polynomial_data_X1_scaled_T)#obviously, +1 from above!

# colnames(polynomial_data_covariates_scaled_T)[1] <- "anticipated_reminder"
# colnames(polynomial_data_X1_scaled_T)[1] <- "anticipated_reminder"

# #convert to df (so that the below function works)
# polynomial_data_covariates_scaled_T <- as.data.frame(polynomial_data_covariates_scaled_T)
# polynomial_data_X1_scaled_T <- as.data.frame(polynomial_data_X1_scaled_T)

# # (1.9): interact covariate/X1 polynomials with T
# gen_interactions <- function(df, var) {
#   new_var <- str_replace(var, ":", "_")
#   var1 <- str_split(var, ":")[[1]][1]
#   var2 <- str_split(var, ":")[[1]][2]
#   df %<>% mutate(!!new_var := eval(as.name(var1)) * eval(as.name(var2)))
#   return(df)
# }

# interactions_covariates_T <- str_subset(combn(colnames(polynomial_data_covariates_scaled_T), 2, FUN = paste, collapse = ":"), "anticipated_reminder")
# all_covariates_T <- colnames(polynomial_data_covariates_scaled_T) %>% append(interactions_covariates_T)

# interactions_X1_T <- str_subset(combn(colnames(polynomial_data_X1_scaled_T), 2, FUN = paste, collapse = ":"), "anticipated_reminder")
# all_X1_T <- colnames(polynomial_data_X1_scaled_T) %>% append(interactions_X1_T)

# system.time(
#   for (int in interactions_covariates_T) {
#     polynomial_data_covariates_scaled_T %<>% gen_interactions(int)
#   })
# system.time(
#   for (int in interactions_X1_T) {
#     polynomial_data_X1_scaled_T %<>% gen_interactions(int)
#   })

# dim(polynomial_data_covariates_scaled_T)
# dim(polynomial_data_X1_scaled_T)

# # (1.10): Import treatment and acceptance status, merge data.
# fintech_fee <- read_csv(here("proc", "fintech_fee_light.csv")) %>%
#   select(organization_uuid,"accepted_offer_late")

# ##################################################
# ##    (2): fit glmnet   ##
# ##################################################

# #(2.1) define variables
# X_df_scaled_all <- cbind(polynomial_data_X1_scaled_T, 
#                          polynomial_data_covariates_scaled_T %>% select(-anticipated_reminder)) #get rid of anticipated reminder in covariate matrix (to avoid redundancy)


# #(2.1) define variables (cont.)
# X <- as.matrix(X_df_scaled_all)#here, includes the treatment W too.
# X <- X[order(X_df_scaled_all$X1),]  #order by months_usage so that we can downweight outliers in months of usage

# org_id <- as.data.frame(balance_data_antrem$organization_uuid)
# colnames(org_id) <- "organization_uuid"
# Y <- as.matrix(left_join(org_id, fintech_fee, by="organization_uuid") %>% select(accepted_offer_late))
# Y <- Y[order(X_df_scaled_all$X1),]  #order by months_usage so that we can downweight outliers in months of usage



# #(2.2) fit glmnet (using default 10-fold CV) and plot
# wts <- c(rep(1,14629)) #downweight outliers (top/bottom 5%)

# # Function to compute RMSE
# compute_rmse <- function(actual, predicted) {
#   n <- length(actual)
#   squared_errors <- (actual - predicted)^2
#   mean_squared_error <- sum(squared_errors) / n
#   rmse <- sqrt(mean_squared_error)
#   return(rmse)
# }


# ##################################################
# ##    (2): fit glmnet   ##
# ##################################################
# n_row <- num_seeds
# degree_covariate_range <- 1:4
# degree_X1_range <- 1:5


# #(2.0) preprocess X1 and covariates based on #degree polynomials

# system.time({
#   foreach(i = 1:n_row, .packages = c('glmnet', 'dplyr', 'stringr', 'tidyverse'), .combine = 'c') %dopar% {
#     j <- 1
    
#     # New seed
#     set.seed(i + 42)
#     test_index <- sample(1:nrow(X), 0.1 * nrow(X))
#     train_index <- setdiff(seq(1, nrow(X)), test_index)
#     Y_test <- Y[test_index]
#     Y_train <- Y[train_index]
    
#     results <- vector("list", length(degree_covariate_range) * length(degree_X1_range))
    
#     for (degree_covariate in degree_covariate_range) {
#       for (degree_X1 in degree_X1_range) {
        
#         # X1
#         all_cols_X1 <- colnames(polynomial_data_X1_scaled_T)
#         default_cols_X1 <- "^anticipated_reminder$|^X1$|^anticipated_reminder_X1$"
#         additional_cols_X1 <- paste(paste0("\\^", 1:degree_X1, collapse = ), collapse = "|")
#         selected_X1 <- paste0(default_cols_X1, "|", additional_cols_X1)
#         polynomial_data_X1_scaled_T_selected <- polynomial_data_X1_scaled_T %>%
#           select(all_of(str_subset(all_cols_X1, selected_X1)))
        
#         # Covariates
#         all_cols_cov <- colnames(polynomial_data_covariates_scaled_T)
#         dimensions_cov <- sapply(sapply(str_extract_all(all_cols_cov, "1|2|3|4"), as.numeric), sum)
#         index_degree_less_than <- which(dimensions_cov <= degree_covariate & dimensions_cov > 0)
#         polynomial_data_covariates_scaled_T_selected <- polynomial_data_covariates_scaled_T[, index_degree_less_than]
        
#         # Define variables
#         X_df_scaled_all_selected <- cbind(polynomial_data_X1_scaled_T_selected,
#                                           polynomial_data_covariates_scaled_T_selected)
        
#         # Define variables (cont.)
#         X_selected <- as.matrix(X_df_scaled_all_selected)
#         X_selected <- X_selected[order(polynomial_data_X1_scaled_T$X1), ]  # Order by months_usage
#         X_selected_test <- X_selected[test_index, ]
#         X_selected <- X_selected[train_index, ]
        
#         # Fit glmnet (using default 10-fold CV)
#         cvfit_ridge_selected <- cv.glmnet(X_selected, Y_train, type.measure = "mse", alpha = 0, weights = wts, parallel = TRUE)
        
#         # OOS RMSE using test set
#         Y_pred_ridge <- predict(cvfit_ridge_selected, X_selected_test, s = "lambda.min")
        
#         # Compute RMSE
#         rmse_ridge <- compute_rmse(Y_test, Y_pred_ridge)
        
#         results[[j]] <- rmse_ridge
        
#         j <- j + 1
#       }
#     }
#     results
#   } -> rmse_ridge_results
  
#   # Combine the results into the rmse_ridge_matrix
#   rmse_ridge_matrix <- do.call(rbind, rmse_ridge_results)
# })

# # Stop the parallel cluster
# stopCluster(cl)

# # format rmse_ridge_matrix -- 20 columns
# rmse_ridge_matrix <- matrix(rmse_ridge_matrix, ncol = 20, byrow = TRUE)

# head(rmse_ridge_matrix)
# write.csv(rmse_ridge_matrix, here("proc","rmse_ridge_nowinsor_std_agemeanimpute.csv"))

# #1.3 winsorized (top 5%, ONLY X1), not standardized, age mean imputed
# ##################################################
# ##    (1): Process data and obtain covariates.  ##
# ##################################################
# rm(list=ls())
# num_seeds <- 200
# # Register parallel backend
# num_cores <- detectCores() - 24  # Use one less than the total number of cores
# cl <- makeCluster(num_cores)
# registerDoParallel(cl)

# # (1.1): Import balance table data.
# balance_data <- read_csv(here("proc", "balance_table_data.csv")) %>% 
#   mutate(antrem = ifelse(anticipated_reminder, "Anticipated reminder", "Unanticipated reminder"))

# # (1.2): Fix observations without owner age.
# balance_data %<>% mutate(owner_age = ifelse(is.na(owner_age), 0, owner_age))

# # (1.3): Restrict data to anticipated / unanticipated reminder
# balance_data_antrem <- balance_data %>% 
#   filter(anticipated_reminder == 1 | unanticipated_reminder == 1)

# # (1.3.5): impute age
# age_average <- mean(balance_data_antrem$owner_age[balance_data_antrem$owner_age>0])
# age_average
# balance_data_antrem$owner_age[which(balance_data_antrem$owner_age==0)] <- age_average


# # (1.4): Define vector with covariates. # We actually have w5 vars in the balance test data
# # remove other type to avoid multicollinearity
# baseline_covariates <- c("owner_sex_female",  "owner_age", "owner_age_unknown",
#                          "new_buss_type_beauty", "new_buss_type_clothing", "new_buss_type_professionals", 
#                          "new_buss_type_restaurants", "new_buss_type_small_retailers",
#                          "make_sale", "log_valid_volume", "log_nr_valid_payments")

# baseline_X1 <- c("months_since_ft")


# # (1.5): winsorization
# # define winsorize function for tidyverse
# winsorize <- function(data, var, newvar, w = 5, highonly = FALSE, na.rm = TRUE) {
#   var_vector <- data[[var]]
  
#   if (na.rm) {
#     var_vector <- var_vector[!is.na(var_vector)]
#   }
  
#   if (highonly) {
#     upper <- quantile(var_vector, 1 - w/100, na.rm = na.rm)
#     data[[newvar]] <- ifelse(data[[var]] > upper, upper, data[[var]])
#   } else {
#     lower <- quantile(var_vector, w/100, na.rm = na.rm)
#     upper <- quantile(var_vector, 1 - w/100, na.rm = na.rm)
#     data[[newvar]] <- ifelse(data[[var]] < lower, lower, 
#                             ifelse(data[[var]] > upper, upper, data[[var]]))
#   }
  
#   return(data)
# }
# # define winsor var list
# winsor_vars <- c("months_since_ft", "owner_age", "make_sale", 
#                  "log_valid_volume", "log_nr_valid_payments")

# # check if any var < 0
# check_highonly <- function(data, var) {
#   if (any(data[[var]] < 0, na.rm = TRUE)) {
#     return(FALSE)
#   } else {
#     return(TRUE)
#   }
# }

# # winsorization
# for (var in winsor_vars) {
#   # check first
#   highonly <- check_highonly(balance_data_antrem, var)

#   balance_data_antrem <- winsorize(
#     data = balance_data_antrem,
#     var = var,
#     newvar = paste0(var, "_w5"), 
#     w = 5,
#     highonly = highonly,
#     na.rm = TRUE
#   )
# }

# winsorized_vars <- paste0(winsor_vars, "_w5")

# # update the covariate matrix
# balance_data_antrem_covariates_winsor <- as.matrix(
#   balance_data_antrem %>%
#     mutate(
#       owner_age = owner_age_w5,
#       make_sale = make_sale_w5,
#       log_valid_volume = log_valid_volume_w5,
#       log_nr_valid_payments = log_nr_valid_payments_w5
#     ) %>%
#     select(all_of(baseline_covariates))
# )

# balance_data_antrem_X1_winsor <- as.matrix(
#   balance_data_antrem %>%
#     select(months_since_ft_w5)
# )


# # (1.6): Create degree=3 polynomial for covariance (due to computation time) & degree=5 polynomial for X1
# #note: poly() gives ALL polynomials (and interactions) under the #degrees

# #toy e.g.:
# #asdf <- as.matrix(cbind(rep(1,100),rep(2,100),rep(3,100)))
# #asdf_poly <- as.data.frame(poly(asdf, degree =3, raw=TRUE))

# polynomial_data_covariates <-as.data.frame(
#   poly(balance_data_antrem_covariates_winsor , degree = 4, raw=TRUE))

# polynomial_data_X1 <-as.data.frame(
#   poly(balance_data_antrem_X1_winsor , degree = 5, raw=TRUE))

# colnames(polynomial_data_X1) <- c("X1","X1^2","X1^3","X1^4","X1^5")

# # (1.7): scale covariates and X1
# polynomial_data_covariates_scaled <- polynomial_data_covariates
# polynomial_data_X1_scaled <- polynomial_data_X1
# # (1.8): take out NaN columns
# # would be necessary if we scale=TRUE above because we divide by sd=0 if all elements in two columns are zeroes.
# # reduces redundant cols and reduce computations going forward

# polynomial_data_covariates_scaled <- polynomial_data_covariates_scaled[ , colSums(is.na(polynomial_data_covariates_scaled)) == 0] 
# polynomial_data_X1_scaled <- polynomial_data_X1_scaled[ , colSums(is.na(polynomial_data_X1_scaled)) == 0] 

# dim(polynomial_data_covariates_scaled) #dim reduced.

# # (1.9): now introduce T
# polynomial_data_covariates_scaled_T <- cbind(balance_data_antrem$anticipated_reminder,polynomial_data_covariates_scaled)
# polynomial_data_X1_scaled_T <- cbind(balance_data_antrem$anticipated_reminder,polynomial_data_X1_scaled)
# dim(polynomial_data_covariates_scaled_T) #obviously, +1 from above!
# dim(polynomial_data_X1_scaled_T)#obviously, +1 from above!

# colnames(polynomial_data_covariates_scaled_T)[1] <- "anticipated_reminder"
# colnames(polynomial_data_X1_scaled_T)[1] <- "anticipated_reminder"

# #convert to df (so that the below function works)
# polynomial_data_covariates_scaled_T <- as.data.frame(polynomial_data_covariates_scaled_T)
# polynomial_data_X1_scaled_T <- as.data.frame(polynomial_data_X1_scaled_T)

# # (1.10): interact covariate/X1 polynomials with T
# gen_interactions <- function(df, var) {
#   new_var <- str_replace(var, ":", "_")
#   var1 <- str_split(var, ":")[[1]][1]
#   var2 <- str_split(var, ":")[[1]][2]
#   df %<>% mutate(!!new_var := eval(as.name(var1)) * eval(as.name(var2)))
#   return(df)
# }

# interactions_covariates_T <- str_subset(combn(colnames(polynomial_data_covariates_scaled_T), 2, FUN = paste, collapse = ":"), "anticipated_reminder")
# all_covariates_T <- colnames(polynomial_data_covariates_scaled_T) %>% append(interactions_covariates_T)

# interactions_X1_T <- str_subset(combn(colnames(polynomial_data_X1_scaled_T), 2, FUN = paste, collapse = ":"), "anticipated_reminder")
# all_X1_T <- colnames(polynomial_data_X1_scaled_T) %>% append(interactions_X1_T)

# system.time(
#   for (int in interactions_covariates_T) {
#     polynomial_data_covariates_scaled_T %<>% gen_interactions(int)
#   })
# system.time(
#   for (int in interactions_X1_T) {
#     polynomial_data_X1_scaled_T %<>% gen_interactions(int)
#   })

# dim(polynomial_data_covariates_scaled_T)
# dim(polynomial_data_X1_scaled_T)

# # (1.11): Import treatment and acceptance status, merge data.
# fintech_fee <- read_csv(here("proc", "fintech_fee_light.csv")) %>%
#   select(organization_uuid,"accepted_offer_late")

# ##################################################
# ##    (2): fit glmnet   ##
# ##################################################

# #(2.1) define variables
# X_df_scaled_all <- cbind(polynomial_data_X1_scaled_T, 
#                          polynomial_data_covariates_scaled_T %>% select(-anticipated_reminder)) #get rid of anticipated reminder in covariate matrix (to avoid redundancy)


# #(2.1) define variables (cont.)
# X <- as.matrix(X_df_scaled_all)#here, includes the treatment W too.
# X <- X[order(X_df_scaled_all$X1),]  #order by months_usage so that we can downweight outliers in months of usage

# org_id <- as.data.frame(balance_data_antrem$organization_uuid)
# colnames(org_id) <- "organization_uuid"
# Y <- as.matrix(left_join(org_id, fintech_fee, by="organization_uuid") %>% select(accepted_offer_late))
# Y <- Y[order(X_df_scaled_all$X1),]  #order by months_usage so that we can downweight outliers in months of usage



# #(2.2) fit glmnet (using default 10-fold CV) and plot
# wts <- c(rep(1,14629)) #downweight outliers (top/bottom 5%)

# # Function to compute RMSE
# compute_rmse <- function(actual, predicted) {
#   n <- length(actual)
#   squared_errors <- (actual - predicted)^2
#   mean_squared_error <- sum(squared_errors) / n
#   rmse <- sqrt(mean_squared_error)
#   return(rmse)
# }


# ##################################################
# ##    (2): fit glmnet   ##
# ##################################################
# n_row <- num_seeds
# degree_covariate_range <- 1:4
# degree_X1_range <- 1:5


# #(2.0) preprocess X1 and covariates based on #degree polynomials

# system.time({
#   foreach(i = 1:n_row, .packages = c('glmnet', 'dplyr', 'stringr', 'tidyverse'), .combine = 'c') %dopar% {
#     j <- 1
    
#     # New seed
#     set.seed(i + 42)
#     test_index <- sample(1:nrow(X), 0.1 * nrow(X))
#     train_index <- setdiff(seq(1, nrow(X)), test_index)
#     Y_test <- Y[test_index]
#     Y_train <- Y[train_index]
    
#     results <- vector("list", length(degree_covariate_range) * length(degree_X1_range))
    
#     for (degree_covariate in degree_covariate_range) {
#       for (degree_X1 in degree_X1_range) {
        
#         # X1
#         all_cols_X1 <- colnames(polynomial_data_X1_scaled_T)
#         default_cols_X1 <- "^anticipated_reminder$|^X1$|^anticipated_reminder_X1$"
#         additional_cols_X1 <- paste(paste0("\\^", 1:degree_X1, collapse = ), collapse = "|")
#         selected_X1 <- paste0(default_cols_X1, "|", additional_cols_X1)
#         polynomial_data_X1_scaled_T_selected <- polynomial_data_X1_scaled_T %>%
#           select(all_of(str_subset(all_cols_X1, selected_X1)))
        
#         # Covariates
#         all_cols_cov <- colnames(polynomial_data_covariates_scaled_T)
#         dimensions_cov <- sapply(sapply(str_extract_all(all_cols_cov, "1|2|3|4"), as.numeric), sum)
#         index_degree_less_than <- which(dimensions_cov <= degree_covariate & dimensions_cov > 0)
#         polynomial_data_covariates_scaled_T_selected <- polynomial_data_covariates_scaled_T[, index_degree_less_than]
        
#         # Define variables
#         X_df_scaled_all_selected <- cbind(polynomial_data_X1_scaled_T_selected,
#                                           polynomial_data_covariates_scaled_T_selected)
        
#         # Define variables (cont.)
#         X_selected <- as.matrix(X_df_scaled_all_selected)
#         X_selected <- X_selected[order(polynomial_data_X1_scaled_T$X1), ]  # Order by months_usage
#         X_selected_test <- X_selected[test_index, ]
#         X_selected <- X_selected[train_index, ]
        
#         # Fit glmnet (using default 10-fold CV)
#         cvfit_ridge_selected <- cv.glmnet(X_selected, Y_train, type.measure = "mse", alpha = 0, weights = wts, parallel = TRUE)
        
#         # OOS RMSE using test set
#         Y_pred_ridge <- predict(cvfit_ridge_selected, X_selected_test, s = "lambda.min")
        
#         # Compute RMSE
#         rmse_ridge <- compute_rmse(Y_test, Y_pred_ridge)
        
#         results[[j]] <- rmse_ridge
        
#         j <- j + 1
#       }
#     }
#     results
#   } -> rmse_ridge_results
  
#   # Combine the results into the rmse_ridge_matrix
#   rmse_ridge_matrix <- do.call(rbind, rmse_ridge_results)
# })

# # Stop the parallel cluster
# stopCluster(cl)

# # format rmse_ridge_matrix -- 20 columns
# rmse_ridge_matrix <- matrix(rmse_ridge_matrix, ncol = 20, byrow = TRUE)

# head(rmse_ridge_matrix)
# write.csv(rmse_ridge_matrix, here("proc","rmse_ridge_winsortopX1_nostd_agemeanimpute.csv"))

# #1.4 not winsorized, not standardized, age mean imputed
# ##################################################
# ##    (1): Process data and obtain covariates.  ##
# ##################################################
# rm(list=ls())
# num_seeds <- 200
# # Register parallel backend
# num_cores <- detectCores() - 24  # Use one less than the total number of cores
# cl <- makeCluster(num_cores)
# registerDoParallel(cl)

# # (1.1): Import balance table data.
# balance_data <- read_csv(here("proc", "balance_table_data.csv")) %>% 
#   mutate(antrem = ifelse(anticipated_reminder, "Anticipated reminder", "Unanticipated reminder"))

# # (1.2): Fix observations without owner age.
# balance_data %<>% mutate(owner_age = ifelse(is.na(owner_age), 0, owner_age))

# # (1.3): Restrict data to anticipated / unanticipated reminder
# balance_data_antrem <- balance_data %>% 
#   filter(anticipated_reminder == 1 | unanticipated_reminder == 1)

# # (1.3.5): impute age
# age_average <- mean(balance_data_antrem$owner_age[balance_data_antrem$owner_age>0])
# age_average
# balance_data_antrem$owner_age[which(balance_data_antrem$owner_age==0)] <- age_average


# # (1.4): Define vector with covariates.
# # remove other type to avoid multicollinearity
# baseline_covariates <- c("owner_sex_female",  "owner_age", "owner_age_unknown",
#                          "new_buss_type_beauty", "new_buss_type_clothing", "new_buss_type_professionals", 
#                          "new_buss_type_restaurants", "new_buss_type_small_retailers", 
#                          "make_sale", "log_valid_volume", "log_nr_valid_payments")

# baseline_X1 <- c("months_since_ft")

# # (1.5): Create df using the covariates defined above
# balance_data_antrem_covariates <- as.matrix(balance_data_antrem %>% select(all_of(baseline_covariates)))
# balance_data_antrem_X1 <- as.matrix(balance_data_antrem %>% select(all_of(baseline_X1)))

# #WINSORIZE -- X1 and COV
# balance_data_antrem_covariates_winsor <- balance_data_antrem_covariates
# balance_data_antrem_X1_winsor <- balance_data_antrem_X1



# # (1.6): Create degree=3 polynomial for covariance (due to computation time) & degree=5 polynomial for X1
# #note: poly() gives ALL polynomials (and interactions) under the #degrees

# #toy e.g.:
# #asdf <- as.matrix(cbind(rep(1,100),rep(2,100),rep(3,100)))
# #asdf_poly <- as.data.frame(poly(asdf, degree =3, raw=TRUE))

# polynomial_data_covariates <-as.data.frame(
#   poly(balance_data_antrem_covariates_winsor , degree = 4, raw=TRUE))

# polynomial_data_X1 <-as.data.frame(
#   poly(balance_data_antrem_X1_winsor , degree = 5, raw=TRUE))

# colnames(polynomial_data_X1) <- c("X1","X1^2","X1^3","X1^4","X1^5")

# # (1.7): scale covariates and X1
# polynomial_data_covariates_scaled <- polynomial_data_covariates
# polynomial_data_X1_scaled <- polynomial_data_X1
# # (1.8): take out NaN columns
# # would be necessary if we scale=TRUE above because we divide by sd=0 if all elements in two columns are zeroes.
# # reduces redundant cols and reduce computations going forward

# polynomial_data_covariates_scaled <- polynomial_data_covariates_scaled[ , colSums(is.na(polynomial_data_covariates_scaled)) == 0] 
# polynomial_data_X1_scaled <- polynomial_data_X1_scaled[ , colSums(is.na(polynomial_data_X1_scaled)) == 0] 

# dim(polynomial_data_covariates_scaled) #dim reduced.

# # (1.8): now introduce T
# polynomial_data_covariates_scaled_T <- cbind(balance_data_antrem$anticipated_reminder,polynomial_data_covariates_scaled)
# polynomial_data_X1_scaled_T <- cbind(balance_data_antrem$anticipated_reminder,polynomial_data_X1_scaled)
# dim(polynomial_data_covariates_scaled_T) #obviously, +1 from above!
# dim(polynomial_data_X1_scaled_T)#obviously, +1 from above!

# colnames(polynomial_data_covariates_scaled_T)[1] <- "anticipated_reminder"
# colnames(polynomial_data_X1_scaled_T)[1] <- "anticipated_reminder"

# #convert to df (so that the below function works)
# polynomial_data_covariates_scaled_T <- as.data.frame(polynomial_data_covariates_scaled_T)
# polynomial_data_X1_scaled_T <- as.data.frame(polynomial_data_X1_scaled_T)

# # (1.9): interact covariate/X1 polynomials with T
# gen_interactions <- function(df, var) {
#   new_var <- str_replace(var, ":", "_")
#   var1 <- str_split(var, ":")[[1]][1]
#   var2 <- str_split(var, ":")[[1]][2]
#   df %<>% mutate(!!new_var := eval(as.name(var1)) * eval(as.name(var2)))
#   return(df)
# }

# interactions_covariates_T <- str_subset(combn(colnames(polynomial_data_covariates_scaled_T), 2, FUN = paste, collapse = ":"), "anticipated_reminder")
# all_covariates_T <- colnames(polynomial_data_covariates_scaled_T) %>% append(interactions_covariates_T)

# interactions_X1_T <- str_subset(combn(colnames(polynomial_data_X1_scaled_T), 2, FUN = paste, collapse = ":"), "anticipated_reminder")
# all_X1_T <- colnames(polynomial_data_X1_scaled_T) %>% append(interactions_X1_T)

# system.time(
#   for (int in interactions_covariates_T) {
#     polynomial_data_covariates_scaled_T %<>% gen_interactions(int)
#   })
# system.time(
#   for (int in interactions_X1_T) {
#     polynomial_data_X1_scaled_T %<>% gen_interactions(int)
#   })

# dim(polynomial_data_covariates_scaled_T)
# dim(polynomial_data_X1_scaled_T)

# # (1.10): Import treatment and acceptance status, merge data.
# fintech_fee <- read_csv(here("proc", "fintech_fee_light.csv")) %>%
#   select(organization_uuid,"accepted_offer_late")

# ##################################################
# ##    (2): fit glmnet   ##
# ##################################################

# #(2.1) define variables
# X_df_scaled_all <- cbind(polynomial_data_X1_scaled_T, 
#                          polynomial_data_covariates_scaled_T %>% select(-anticipated_reminder)) #get rid of anticipated reminder in covariate matrix (to avoid redundancy)


# #(2.1) define variables (cont.)
# X <- as.matrix(X_df_scaled_all)#here, includes the treatment W too.
# X <- X[order(X_df_scaled_all$X1),]  #order by months_usage so that we can downweight outliers in months of usage

# org_id <- as.data.frame(balance_data_antrem$organization_uuid)
# colnames(org_id) <- "organization_uuid"
# Y <- as.matrix(left_join(org_id, fintech_fee, by="organization_uuid") %>% select(accepted_offer_late))
# Y <- Y[order(X_df_scaled_all$X1),]  #order by months_usage so that we can downweight outliers in months of usage



# #(2.2) fit glmnet (using default 10-fold CV) and plot
# wts <- c(rep(1,14629)) #downweight outliers (top/bottom 5%)

# # Function to compute RMSE
# compute_rmse <- function(actual, predicted) {
#   n <- length(actual)
#   squared_errors <- (actual - predicted)^2
#   mean_squared_error <- sum(squared_errors) / n
#   rmse <- sqrt(mean_squared_error)
#   return(rmse)
# }


# ##################################################
# ##    (2): fit glmnet   ##
# ##################################################
# n_row <- num_seeds
# degree_covariate_range <- 1:4
# degree_X1_range <- 1:5


# #(2.0) preprocess X1 and covariates based on #degree polynomials

# system.time({
#   foreach(i = 1:n_row, .packages = c('glmnet', 'dplyr', 'stringr', 'tidyverse'), .combine = 'c') %dopar% {
#     j <- 1
    
#     # New seed
#     set.seed(i + 42)
#     test_index <- sample(1:nrow(X), 0.1 * nrow(X))
#     train_index <- setdiff(seq(1, nrow(X)), test_index)
#     Y_test <- Y[test_index]
#     Y_train <- Y[train_index]
    
#     results <- vector("list", length(degree_covariate_range) * length(degree_X1_range))
    
#     for (degree_covariate in degree_covariate_range) {
#       for (degree_X1 in degree_X1_range) {
        
#         # X1
#         all_cols_X1 <- colnames(polynomial_data_X1_scaled_T)
#         default_cols_X1 <- "^anticipated_reminder$|^X1$|^anticipated_reminder_X1$"
#         additional_cols_X1 <- paste(paste0("\\^", 1:degree_X1, collapse = ), collapse = "|")
#         selected_X1 <- paste0(default_cols_X1, "|", additional_cols_X1)
#         polynomial_data_X1_scaled_T_selected <- polynomial_data_X1_scaled_T %>%
#           select(all_of(str_subset(all_cols_X1, selected_X1)))
        
#         # Covariates
#         all_cols_cov <- colnames(polynomial_data_covariates_scaled_T)
#         dimensions_cov <- sapply(sapply(str_extract_all(all_cols_cov, "1|2|3|4"), as.numeric), sum)
#         index_degree_less_than <- which(dimensions_cov <= degree_covariate & dimensions_cov > 0)
#         polynomial_data_covariates_scaled_T_selected <- polynomial_data_covariates_scaled_T[, index_degree_less_than]
        
#         # Define variables
#         X_df_scaled_all_selected <- cbind(polynomial_data_X1_scaled_T_selected,
#                                           polynomial_data_covariates_scaled_T_selected)
        
#         # Define variables (cont.)
#         X_selected <- as.matrix(X_df_scaled_all_selected)
#         X_selected <- X_selected[order(polynomial_data_X1_scaled_T$X1), ]  # Order by months_usage
#         X_selected_test <- X_selected[test_index, ]
#         X_selected <- X_selected[train_index, ]
        
#         # Fit glmnet (using default 10-fold CV)
#         cvfit_ridge_selected <- cv.glmnet(X_selected, Y_train, type.measure = "mse", alpha = 0, weights = wts, parallel = TRUE)
        
#         # OOS RMSE using test set
#         Y_pred_ridge <- predict(cvfit_ridge_selected, X_selected_test, s = "lambda.min")
        
#         # Compute RMSE
#         rmse_ridge <- compute_rmse(Y_test, Y_pred_ridge)
        
#         results[[j]] <- rmse_ridge
        
#         j <- j + 1
#       }
#     }
#     results
#   } -> rmse_ridge_results
  
#   # Combine the results into the rmse_ridge_matrix
#   rmse_ridge_matrix <- do.call(rbind, rmse_ridge_results)
# })

# # Stop the parallel cluster
# stopCluster(cl)

# # format rmse_ridge_matrix -- 20 columns
# rmse_ridge_matrix <- matrix(rmse_ridge_matrix, ncol = 20, byrow = TRUE)

# head(rmse_ridge_matrix)
# write.csv(rmse_ridge_matrix, here("proc","rmse_ridge_nowinsor_nostd_agemeanimpute.csv"))

# ################################################################################
# #2. create rmse rank table#######################################################
# ################################################################################

# rm(list = ls())
# source(here("scripts", "programs", "set_theme.R"))
# source(here("scripts", "programs", "print_as_is.R"))
# source(here("scripts", "programs", "print_as_percent.R"))
# source(here("scripts", "programs", "print_N.R"))

# num_seeds <- 200

# #read csv files
# nostd_now <- read.csv(here("proc", "rmse_ridge_nowinsor_nostd_agemeanimpute.csv"))
# std_now <- read.csv(here("proc", "rmse_ridge_nowinsor_std_agemeanimpute.csv"))
# nostd_wtopX1 <- read.csv(here("proc", "rmse_ridge_winsortopX1_nostd_agemeanimpute.csv"))
# std_wtopX1 <- read.csv(here("proc", "rmse_ridge_winsortopX1_std_agemeanimpute.csv"))

# #ignore the first column
# nostd_now <- nostd_now[,-1]
# std_now <- std_now[,-1]
# nostd_wtopX1 <- nostd_wtopX1[,-1]
# std_wtopX1 <- std_wtopX1[,-1]

# #check if the OOS RMSE is lower than the first column
# improved_nostd_now <- nostd_now < nostd_now[,1]
# improved_std_now <- std_now < nostd_now[,1]
# improved_nostd_wtopX1 <- nostd_wtopX1 < nostd_now[,1]
# improved_std_wtopX1 <- std_wtopX1 < nostd_now[,1]

# improved_percent <- rbind(
#   colSums(improved_nostd_now)*100/num_seeds,
#   colSums(improved_std_now)*100/num_seeds,
#   colSums(improved_nostd_wtopX1)*100/num_seeds,
#   colSums(improved_std_wtopX1)*100/num_seeds
# )

# improved_percent <- rbind(
#   sprintf("%.1f", round(colSums(improved_nostd_now)*100/num_seeds, 1)),
#   sprintf("%.1f", round(colSums(improved_std_now)*100/num_seeds, 1)),
#   sprintf("%.1f", round(colSums(improved_nostd_wtopX1)*100/num_seeds, 1)),
#   sprintf("%.1f", round(colSums(improved_std_wtopX1)*100/num_seeds, 1))
# )


# #heat map
# #convert to long format
# Percents <- as.vector(improved_percent)
# Percents[1] <- NA
# f <- rep(c(rep(1,4), rep(2,4), rep(3,4), rep(4,4), rep(5,4)),4)
# g <- c(rep(1,20), rep(2,20), rep(3,20), rep(4,20))
# winsor_std <- rep(c("Not Winsorized and Not Standardized","Not Winsorized and Standardized","Winsorized and Not Standardized","Winsorized and Standardized"),20)
# percent_df <- data.frame(f,g,winsor_std,Percents)
# percent_df$Percents <- as.numeric(percent_df$Percents)

# # Function to determine text color based on tile color
# text_color <- function(value) {
#   ifelse(as.numeric(value) > 12, "white", "black")
# }

# # Update the data frame with the text colors
# percent_df <- percent_df %>% 
#   mutate(text_color = ifelse(is.na(Percents), "transparent", text_color(Percents)))

# percent_df_upto4 <- percent_df[(percent_df$f != 5),]

# loadfonts(device = "postscript")
# font_add("Times New Roman", regular = "times.ttf", italic = "timesi.ttf") # change to times and timesi@
# showtext_auto()

# ggplot(percent_df_upto4, aes(f,g)) +
#   facet_wrap(~winsor_std) +
#   geom_tile(aes(fill = Percents)) +
#   geom_text(aes(label = sprintf("%.1f", Percents), color = text_color), size = 5, family = "Times New Roman") +
#   scale_fill_gradient2() +
#   scale_color_identity() + # Ensure the colors are used directly
#   labs(
#     y = expression("Degree of Polynomial for *g*"),
#     x = expression("Degree of Polynomial for *f*")
#   ) + 
#   set_theme(legend_position = "none") + 
#   theme(
#     strip.background = element_blank(),
#     axis.title = element_text(size=12,family = "Times New Roman"),
#     strip.text = element_text(size=12/0.8,family = "Times New Roman"),
#     axis.text = element_text(size=12,family = "Times New Roman"),
#     axis.text.x = element_text(margin = margin(t = -8)),
#     axis.text.y = element_text(margin = margin(r = -10)),
#     axis.title.y = ggtext::element_markdown(),
#     axis.title.x = ggtext::element_markdown()
#   )
# ggsave(here("results/figures", "heatmap_percent_betterrmse.eps"), device = cairo_ps, width = 10, height = 8)
# w_notstd_4_1 <- as.vector(colSums(improved_nostd_wtopX1))[4]
# w_notstd_4_1_percent <- as.vector(colSums(improved_nostd_wtopX1)/num_seeds)[4]

# print_as_is(w_notstd_4_1, here("results/numbers", "percent_to_number_heatmap_4_1.tex"))
# print_as_percent(w_notstd_4_1_percent, round =1, here("results/numbers", "percent_heatmap_4_1.tex"))


################################################################################
#3. take derivative and bootstrap - for (2,2) non-standardized, non-W model#####
################################################################################
rm(list = ls())
source(here("scripts", "programs", "print_as_is.R"))
source(here("scripts", "programs", "print_as_percent.R"))
source(here("scripts", "programs", "print_N.R"))

set.seed(71571804)
##################################################
##    (1): Process data and obtain covariates.  ##
##################################################
# (1.1): Import balance table data.
balance_data <- read_csv(here("proc", "balance_table_data.csv")) %>% 
  mutate(antrem = ifelse(anticipated_reminder, "Anticipated reminder", "Unanticipated reminder"))

# (1.2): Fix observations without owner age.
balance_data %<>% mutate(owner_age = ifelse(is.na(owner_age), 0, owner_age))

# (1.3): Restrict data to anticipated / unanticipated reminder
balance_data_antrem <- balance_data %>% 
  filter(anticipated_reminder == 1 | unanticipated_reminder == 1)

# (1.3.5): impute age
age_average <- mean(balance_data_antrem$owner_age[balance_data_antrem$owner_age>0])
age_average
balance_data_antrem$owner_age[which(balance_data_antrem$owner_age==0)] <- age_average

# (1.4): Define vector with covariates.
baseline_covariates <- c("owner_sex_female",  "owner_age","owner_age_unknown",
                         "new_buss_type_beauty", "new_buss_type_clothing", "new_buss_type_professionals", 
                         "new_buss_type_restaurants", "new_buss_type_small_retailers", 
                         "make_sale", "log_valid_volume", "log_nr_valid_payments")
baseline_X1 <- c( "months_since_ft")


# (1.5): Create df using the covariates defined above
balance_data_antrem_covariates <- as.matrix(balance_data_antrem %>% select(all_of(baseline_covariates)))
balance_data_antrem_X1 <- as.matrix(balance_data_antrem %>% select(all_of(baseline_X1)))

#order by X1
order_X1 <- order(balance_data_antrem_X1)
balance_data_antrem_covariates_ordered <- balance_data_antrem_covariates[order_X1,]
balance_data_antrem_X1_ordered <- balance_data_antrem_X1[order_X1]

#WINSORIZE -- Non-winsorize
balance_data_antrem_covariates_winsor <- balance_data_antrem_covariates_ordered
balance_data_antrem_X1_winsor <- balance_data_antrem_X1_ordered


# (1.6): Create degree=3 polynomial for covariance (due to computation time) & degree=5 polynomial for X1
# note: poly() gives ALL polynomials (and interactions) under the #degrees

#toy e.g.:
#asdf <- as.matrix(cbind(rep(1,100),rep(2,100),rep(3,100)))
#asdf_poly <- as.data.frame(poly(asdf, degree =3, raw=TRUE))
polynomial_data_covariates <-as.data.frame(
  poly(balance_data_antrem_covariates_winsor , degree = 2, raw=TRUE))
polynomial_data_X1 <-as.data.frame(
  poly(balance_data_antrem_X1_winsor , degree = 2, raw=TRUE))
colnames(polynomial_data_X1) <- c("X1","X1^2")

# (1.7): scale covariates and X1 (not really)
polynomial_data_covariates_scaled <- polynomial_data_covariates
polynomial_data_X1_scaled <- polynomial_data_X1
dim(polynomial_data_covariates_scaled)

# (1.8): take out NaN columns
# would be necessary if we scale=TRUE above because we divide by sd=0 if all elements in two columns are zeroes.
# reduces redundant cols and reduce computations going forward
polynomial_data_covariates_scaled <- polynomial_data_covariates_scaled[ , colSums(is.na(polynomial_data_covariates_scaled)) == 0] 
polynomial_data_X1_scaled <- polynomial_data_X1_scaled[ , colSums(is.na(polynomial_data_X1_scaled)) == 0] 
dim(polynomial_data_covariates_scaled) #dim reduced.

# (1.9): now introduce T
polynomial_data_covariates_scaled_T <- cbind(balance_data_antrem$anticipated_reminder[order_X1],polynomial_data_covariates_scaled)
polynomial_data_X1_scaled_T <- cbind(balance_data_antrem$anticipated_reminder[order_X1],polynomial_data_X1_scaled)
dim(polynomial_data_covariates_scaled_T) #obviously, +1 from above!
dim(polynomial_data_X1_scaled_T)#obviously, +1 from above!

colnames(polynomial_data_covariates_scaled_T)[1] <- "anticipated_reminder"
colnames(polynomial_data_X1_scaled_T)[1] <- "anticipated_reminder"

#convert to df (so that the below function works)
polynomial_data_covariates_scaled_T <- as.data.frame(polynomial_data_covariates_scaled_T)
polynomial_data_X1_scaled_T <- as.data.frame(polynomial_data_X1_scaled_T)

# (1.10): interact covariate/X1 polynomials with T
gen_interactions <- function(df, var) {
  new_var <- str_replace(var, ":", "_")
  var1 <- str_split(var, ":")[[1]][1]
  var2 <- str_split(var, ":")[[1]][2]
  df %<>% mutate(!!new_var := eval(as.name(var1)) * eval(as.name(var2)))
  return(df)
}

interactions_covariates_T <- str_subset(combn(colnames(polynomial_data_covariates_scaled_T), 2, FUN = paste, collapse = ":"), "anticipated_reminder")
all_covariates_T <- colnames(polynomial_data_covariates_scaled_T) %>% append(interactions_covariates_T)

interactions_X1_T <- str_subset(combn(colnames(polynomial_data_X1_scaled_T), 2, FUN = paste, collapse = ":"), "anticipated_reminder")
all_X1_T <- colnames(polynomial_data_X1_scaled_T) %>% append(interactions_X1_T)

system.time(
  for (int in interactions_covariates_T) {
    polynomial_data_covariates_scaled_T %<>% gen_interactions(int)
  })
system.time(
  for (int in interactions_X1_T) {
    polynomial_data_X1_scaled_T %<>% gen_interactions(int)
  })

dim(polynomial_data_covariates_scaled_T)
dim(polynomial_data_X1_scaled_T)

# (1.11): Import treatment and acceptance status.
fintech_fee <- read_csv(here("proc", "fintech_fee_light.csv")) %>%
  select(organization_uuid,"accepted_offer_late")

##################################################
##    (2): create matrix to feed into glmnet   ##
##################################################

#(2.1) Put together all X1 and covariates
X_df_scaled_all <- cbind(polynomial_data_X1_scaled_T, 
                         polynomial_data_covariates_scaled_T %>% select(-anticipated_reminder)) #get rid of W in covariate matrix (to avoid redundancy)

#(2.2) Convert above to matrix and order by X1
X_selected <- as.matrix(X_df_scaled_all) #here, includes the treatment W too.

#(2.3) Create outcome variable and order by X1 (just as the X matrix)
org_id <- as.data.frame(balance_data_antrem$organization_uuid)
colnames(org_id) <- "organization_uuid"
Y <- as.matrix(left_join(org_id, fintech_fee, by="organization_uuid") %>% select(accepted_offer_late))
Y_ordered <- Y[order_X1,]  #order by months_usage so that we can downweight outliers in months of usage

#(2.4) Assign weights so that super long-time users are downweighted
wts <- c(rep(1,16254*0.95),1,rep(0.01,16254*0.05)) #downweight outliers (top 5%)
#wts <- c(rep(0.01,16254*0.05),rep(1,16254*0.90),1,1,rep(0.01,16254*0.05)) #downweight outliers (top/bottom 5%)

#########################################################
##    (4): bootstrap samples and fit glmnet#######
#########################################################
#(4.1) initiate
B <- 1000 #bootstrap size
set.seed(100)

#(4.1.1) grid of discrete values
grid_X1 <- as.vector(quantile(balance_data_antrem_X1,seq(from=0,to=1,length.out=100)))

X1_mean <- mean(balance_data_antrem_X1)
X1_med <- median(balance_data_antrem_X1)
X1_25 <- quantile(balance_data_antrem_X1,0.25)
X1_75 <- quantile(balance_data_antrem_X1,0.75)

X1_mean_scaled <- 0
X1_med_scaled <- median(scale(balance_data_antrem_X1, scale=TRUE))
X1_25_scaled <- quantile(scale(balance_data_antrem_X1, scale=TRUE),0.25)
X1_75_scaled <- quantile(scale(balance_data_antrem_X1, scale=TRUE),0.75)

slope_mean <- rep(NA, B)
slope_med <- rep(NA, B)
slope_25 <- rep(NA, B)
slope_75 <- rep(NA, B)

system.time(
  for (i in 1:B){
    #pick index randomly
    index_bs <- sample(1:nrow(Y), replace = TRUE)
    
    #bootstrap
    balance_data_antrem_covariates_B <- balance_data_antrem_covariates[index_bs,]
    balance_data_antrem_X1_B <- balance_data_antrem_X1[index_bs]
    Y_B <- Y[index_bs,]
    T_B <- balance_data_antrem$anticipated_reminder[index_bs]
    
    #order by X1
    order_X1_B <- order(balance_data_antrem_X1_B)
    balance_data_antrem_covariates_B <- balance_data_antrem_covariates_B[order_X1_B,]
    balance_data_antrem_X1_B <- balance_data_antrem_X1_B[order_X1_B]
    Y_B <- Y_B[order_X1_B]
    T_B <- T_B[order_X1_B]
    
    #WINSORIZE -- X1, both sides
    
    # (1.6): Create degree=3 polynomial for covariance (due to computation time) & degree=5 polynomial for X1
    # note: poly() gives ALL polynomials (and interactions) under the #degrees
    polynomial_data_covariates_B <-as.data.frame(
      poly(balance_data_antrem_covariates_B , degree = 2, raw=TRUE))
    polynomial_data_X1_B <-as.data.frame(
      poly(balance_data_antrem_X1_B , degree = 2, raw=TRUE))
    colnames(polynomial_data_X1_B) <- c("X1","X1^2")
    
    # (1.7): scale covariates and X1
    polynomial_data_covariates_scaled_B <- polynomial_data_covariates_B
    polynomial_data_X1_scaled_B <- polynomial_data_X1_B
    
    # (1.8): take out NaN columns
    # would be necessary if we scale=TRUE above because we divide by sd=0 if all elements in two columns are zeroes.
    # reduces redundant cols and reduce computations going forward
    polynomial_data_covariates_scaled_B <- polynomial_data_covariates_scaled_B[ , colSums(is.na(polynomial_data_covariates_scaled_B)) == 0] 
    polynomial_data_X1_scaled_B <- polynomial_data_X1_scaled_B[ , colSums(is.na(polynomial_data_X1_scaled_B)) == 0] 
    
    # (1.9): now introduce T
    polynomial_data_covariates_scaled_T_B <- cbind(T_B,polynomial_data_covariates_scaled_B)
    polynomial_data_X1_scaled_T_B <- cbind(T_B,polynomial_data_X1_scaled_B)
    
    colnames(polynomial_data_covariates_scaled_T_B)[1] <- "anticipated_reminder"
    colnames(polynomial_data_X1_scaled_T_B)[1] <- "anticipated_reminder"
    
    #convert to df (so that the below function works)
    polynomial_data_covariates_scaled_T_B <- as.data.frame(polynomial_data_covariates_scaled_T_B)
    polynomial_data_X1_scaled_T_B <- as.data.frame(polynomial_data_X1_scaled_T_B)
    
    # (1.10): interact covariate/X1 polynomials with T
    interactions_covariates_T_B <- str_subset(combn(colnames(polynomial_data_covariates_scaled_T_B), 2, FUN = paste, collapse = ":"), "anticipated_reminder")
    all_covariates_T_B <- colnames(polynomial_data_covariates_scaled_T_B) %>% append(interactions_covariates_T_B)
    
    interactions_X1_T_B <- str_subset(combn(colnames(polynomial_data_X1_scaled_T_B), 2, FUN = paste, collapse = ":"), "anticipated_reminder")
    all_X1_T_B <- colnames(polynomial_data_X1_scaled_T_B) %>% append(interactions_X1_T_B)
    
    
    for (int in interactions_covariates_T_B) {
      polynomial_data_covariates_scaled_T_B %<>% gen_interactions(int)
    }
    
    for (int in interactions_X1_T_B) {
      polynomial_data_X1_scaled_T_B %<>% gen_interactions(int)
    }
    
    
    #(2.1) Put together all X1 and covariates
    X_df_scaled_all_B <- cbind(polynomial_data_X1_scaled_T_B, 
                               polynomial_data_covariates_scaled_T_B %>% select(-anticipated_reminder)) #get rid of W in covariate matrix (to avoid redundancy)
    
    #(2.2) Convert above to matrix and order by X1
    X_B <- as.matrix(X_df_scaled_all_B) #here, includes the treatment W too.
    
    #fit glmnet
    cvfit_ridge_selected <- cv.glmnet(X_B, Y_B, type.measure = "mse", alpha = 0, weights = wts, parallel = TRUE)
    
    
    
    #(4.6) get ALL anticipated_reminder coefficients (THIS IS THE KEY*****)
    index_T <- which(str_detect(colnames(X_df_scaled_all_B), "anticipated_reminder")) + 1 #+1 to account for the intercept term
    coeff_ridge <- coef(cvfit_ridge_selected, s="lambda.min")[index_T,] #BETA
    
    
    
    #MAIN OUTPUTS
    alpha_3 <- coeff_ridge[2]
    alpha_4 <- coeff_ridge[3]
    
    slope_mean[i] <- alpha_3 + 2*alpha_4*X1_mean
    slope_med[i] <- alpha_3 + 2*alpha_4*X1_med
    slope_25[i] <- alpha_3 + 2*alpha_4*X1_25
    slope_75[i] <- alpha_3 + 2*alpha_4*X1_75
    ####
    
  }
)

#print percentage of negative slopes: mean, median
print_as_is(X1_25, here("results/numbers", "length_25.tex"))
print_as_is(X1_med,here("results/numbers", "length_med.tex"))
print_as_is(X1_mean,here("results/numbers", "length_mean.tex"))
print_as_is(X1_75,here("results/numbers", "length_75.tex"))

mean_percent <- sum(slope_mean<0)/B
med_percent <- sum(slope_med<0)/B
percent_25 <- sum(slope_25<0)/B
percent_75 <- sum(slope_75<0)/B

print_as_percent(percent_25,round=1,here("results/numbers", "percent_negative_25.tex"))
print_as_percent(med_percent,round=1,here("results/numbers", "percent_negative_med.tex"))
print_as_percent(mean_percent,round=1,here("results/numbers", "percent_negative_mean.tex"))
print_as_percent(percent_75,round=1,here("results/numbers", "percent_negative_75.tex"))

cat("Negative slopes (%):\n",
    percent_25, med_percent, mean_percent, percent_75, "\n")
