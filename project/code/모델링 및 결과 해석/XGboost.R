# ------ 변수 몽땅 지우기 ------ #
rm(list = ls())
# ------ basic package ------ #
library(tidyverse)
library(plyr)
library(magrittr)
library(data.table)
library(gridExtra)
library(dummies)
library(xgboost)
library(caret)
library(MLmetrics)
library(progress)
library(rBayesianOptimization)

# ------ choose working directory ------ #
setwd("D:/학회/방학세미나")
getwd()

train_sm = fread('split_train_sm.csv',data.table = F)
test_sm = fread('split_test.csv',data.table = F)

train_sm_fin = fread('train_cart_sm.csv',data.table = F)
test_sm_fin = fread('test_cart.csv',data.table = F)

# ------ XGboost------- #
# 튜닝할  것
# 인자
# 범위
# 변수 뭐 넣을 지
# iter

# ===== SMOTE 셋 ===== #

# xgboost setting
train_sm_label = train_sm$class
test_sm_label = test_sm$class

xgb_mat_train_sm = xgb.DMatrix(as.matrix(select(train_sm,-class)),label = train_sm_label)
xgb_mat_test_sm = xgb.DMatrix(as.matrix(select(test_sm,-class)),label = test_sm_label)

train_sm_fin_label = train_sm_fin$class

xgb_mat_train_sm_fin = xgb.DMatrix(as.matrix(select(train_sm_fin,-class)),label = train_sm_fin_label)
xgb_mat_test_sm_fin = xgb.DMatrix(as.matrix(test_sm_fin))

table(train_sm_label)
table(test_sm_label)

# ----- Function: cost ----- #
compute_cost = function(y,yhat) {
  cm = table(true = y, predict = yhat)
  cost = cm[2,1] * 500 + cm[1,2] * 10
  return(cost)
}

# ----- random tuning ----- #
# ,weight = ifelse(cv_tr_label == 1,30,1)
xgb_random <- function(iter, param_set, train_onehot, train_label){
  for (i in 1:iter) {
    params = param_set[i,] %>%
      select(-c(cost,f1,nrounds)) %>%
      as.list()
    
    set.seed(123)
    cv = createFolds(train_label, k = 5)
    cost = NULL
    f1 = NULL
    
    for (j in 1:5) {
      valid_idx = cv[[j]]
      cv_tr_dummy = train_onehot[-valid_idx,]
      cv_te_dummy = train_onehot[valid_idx,]
      cv_tr_label = cv_tr_dummy$class
      cv_te_label = cv_te_dummy$class
      xgb_cv_tr = xgb.DMatrix(as.matrix(select(cv_tr_dummy,-class)),label = cv_tr_label)
      xgb_cv_te = xgb.DMatrix(as.matrix(select(cv_te_dummy,-class)),label = cv_te_label)
      watchlist <- list(train = xgb_cv_tr, test = xgb_cv_te)
      
      
      set.seed(123)
      cv_xgb = xgb.train(
        params = params,
        data = xgb_cv_tr,
        nrounds = param_set[i,'nrounds'],
        early_stopping_rounds = 0.05*param_set[i,'nrounds'],
        watchlist = watchlist,
        verbose = T)
      
      cv_pred = predict(cv_xgb,newdata = xgb_cv_te)
      cv_pred = ifelse(cv_pred >= 0.5, 1, 0)
      temp_cost = compute_cost(as.integer(cv_te_label),cv_pred) * 8000 / nrow(xgb_cv_te)
      temp_f1 = F1_Score(cv_te_label,cv_pred)
      cost = c(cost,temp_cost)
      f1 = c(f1,temp_f1)
    }
    param_set[i,'cost'] = mean(cost)
    param_set[i,'f1'] = mean(f1)
  }
  return(param_set)
}

set.seed(123)
param_set = data.frame(
  max_depth = sample(seq(3,8),20,replace = TRUE),
  min_child_weight = sample(seq(3,8),20,replace = TRUE),
  subsample = runif(20,0.6,1),
  colsample_bytree = runif(20,0.6,1),
  eta = runif(20,0.01,0.3),
  nrounds = sample(c(500,600,700,800),20,replace = TRUE),
  cost = rep(NA,20),
  f1 = rep(NA,20),
  objective = "binary:logistic",
  eval_metric = 'error',
  gamma = 0
)

set.seed(123)
best_param_sm <- xgb_random(iter = 20, param_set = param_set, train_onehot = train_sm, train_label = train_sm_label)
best_param_sm <- best_param_sm[order(best_param_sm$cost),] # 9250.217 0.9770447
best_param_sm[1,] # 21025.11 0.9796853

# ----- Grid Search ----- #

# random tuning에서 찾은 최적의 파라미터 주변에 대해 grid search 진행

# ----- max_depth & min_child_weight

# parameter
# max_depth = seq(best_param_sm[1,]$max_depth-1,best_param_sm[1,]$max_depth+2,1)
max_depth = seq(best_param_sm[1,]$max_depth-2,best_param_sm[1,]$max_depth+2,1)
min_child_weight = seq(best_param_sm[1,]$min_child_weight-1,best_param_sm[1,]$min_child_weight+1,1)

grid_1 = data.frame(
  max_depth = rep(max_depth,length(min_child_weight)),
  min_child_weight = rep(min_child_weight,each = length(max_depth)),
  cost = rep(NA,length(min_child_weight) * length(max_depth)),
  f1 = rep(NA,length(min_child_weight) * length(max_depth))
)

# ,weight = ifelse(cv_tr_label == 1,30,1)
xgb_grid1 = function(param_set, opt_random_param_set, train_onehot, train_label) {
  
  pb = progress_bar$new(total = nrow(param_set))
  
  for (i in 1:nrow(param_set)) {
    # parameter set
    param_1 = list(
      max_depth = param_set[i,'max_depth'],
      min_child_weight = param_set[i,'min_child_weight'],
      subsample = opt_random_param_set[1,]$subsample,
      colsample_bytree = opt_random_param_set[1,]$colsample_bytree,
      eta = opt_random_param_set[1,]$eta,
      objective = "binary:logistic",
      eval_metric = 'error',
      gamma = 0
    )
    
    # cv fold
    set.seed(123)
    cv = createFolds(train_label,k = 5)
    cost = NULL
    f1 = NULL
    
    for (j in 1:5) {
      valid_idx = cv[[j]]
      cv_tr_dummy = train_onehot[-valid_idx,]
      cv_te_dummy = train_onehot[valid_idx,]
      cv_tr_label = cv_tr_dummy$class
      cv_te_label = cv_te_dummy$class
      xgb_cv_tr = xgb.DMatrix(as.matrix(select(cv_tr_dummy,-class)),label = cv_tr_label)
      xgb_cv_te = xgb.DMatrix(as.matrix(select(cv_te_dummy,-class)),label = cv_te_label)
      watchlist <- list(train = xgb_cv_tr, test = xgb_cv_te)
      
      # cv
      set.seed(123)
      cv_xgb = xgb.train(
        params = param_1,
        data = xgb_cv_tr,
        nrounds = opt_random_param_set[1,]$nrounds,
        early_stopping_rounds = 0.05 * opt_random_param_set[1,]$nrounds,
        watchlist = watchlist,
        verbose = F)

      
      cv_pred = predict(cv_xgb,newdata = xgb_cv_te)
      cv_pred = ifelse(cv_pred >= 0.5, 1, 0)
      temp_cost = compute_cost(as.integer(cv_te_label),cv_pred) * 8000 / nrow(xgb_cv_te)
      temp_f1 = F1_Score(cv_te_label,cv_pred)
      cost = c(cost,temp_cost)
      f1 = c(f1,temp_f1)
    }
    param_set[i,'cost'] = mean(cost)
    param_set[i,'f1'] = mean(f1)
    
    pb$tick()
    Sys.sleep(0.01)
  }
  return(param_set)
}

best_grid1 = xgb_grid1(param_set = grid_1,opt_random_param_set = best_param_sm,train_onehot = train_sm, train_label = train_sm_label)
best_grid1 = best_grid1 %>% arrange(cost)
best_grid1 # 9250.217 0.9770447
# 20900.83 0.9810794

# ----- subsample & cosmaple_bytree
subsample = seq(best_param_sm[1,]$subsample-0.1,best_param_sm[1,]$subsample+0.1,0.04)
colsample_bytree = seq(best_param_sm[1,]$colsample_bytree-0.1,best_param_sm[1,]$colsample_bytree+0.1,0.04)

grid_2 = data.frame(
  subsample = rep(subsample,length(colsample_bytree)),
  colsample_bytree = rep(colsample_bytree,each = length(subsample)),
  cost = rep(NA,length(colsample_bytree) * length(subsample)),
  f1 = rep(NA,length(colsample_bytree) * length(subsample))
)

# ,weight = ifelse(cv_tr_label == 1,30,1)
xgb_grid2 = function(param_set, opt_random_param_set, train_onehot, train_label) {
  
  pb = progress_bar$new(total = nrow(param_set))
  
  for (i in 1:nrow(param_set)) {
    # parameter set
    param_2 = list(
      subsample = param_set[i,'subsample'],
      colsample_bytree = param_set[i,'colsample_bytree'],
      max_depth = best_grid1[1,]$max_depth,
      min_child_weight = best_grid1[1,]$min_child_weight,
      eta = opt_random_param_set$eta,
      objective = "binary:logistic",
      eval_metric = 'error',
      gamma = 0
    )
    
    # cv fold
    set.seed(123)
    cv = createFolds(train_label,k = 5)
    cost = NULL
    f1 = NULL
    
    for (j in 1:5) {
      valid_idx = cv[[j]]
      cv_tr_dummy = train_onehot[-valid_idx,]
      cv_te_dummy = train_onehot[valid_idx,]
      cv_tr_label = cv_tr_dummy$class
      cv_te_label = cv_te_dummy$class
      xgb_cv_tr = xgb.DMatrix(as.matrix(select(cv_tr_dummy,-class)),label = cv_tr_label)
      xgb_cv_te = xgb.DMatrix(as.matrix(select(cv_te_dummy,-class)),label = cv_te_label)
      watchlist <- list(train = xgb_cv_tr, test = xgb_cv_te)
      
      # cv
      set.seed(123)
      cv_xgb = xgb.train(
        params = param_2,
        data = xgb_cv_tr,
        nrounds = opt_random_param_set[1,]$nrounds,
        early_stopping_rounds = 0.05 * opt_random_param_set[1,]$nrounds,
        watchlist = watchlist,
        verbose = F)
      
      cv_pred = predict(cv_xgb,newdata = xgb_cv_te)
      cv_pred = ifelse(cv_pred >= 0.5, 1, 0)
      temp_cost = compute_cost(as.integer(cv_te_label),cv_pred) * 8000 / nrow(xgb_cv_te)
      temp_f1 = F1_Score(cv_te_label,cv_pred)
      cost = c(cost,temp_cost)
      f1 = c(f1,temp_f1)
    }
    param_set[i,'cost'] = mean(cost)
    param_set[i,'f1'] = mean(f1)
    
    pb$tick()
    Sys.sleep(0.01)
  }
  return(param_set)
}

best_grid2 = xgb_grid2(param_set = grid_2,opt_random_param_set = best_param_sm,train_onehot = train_sm, train_label = train_sm_label)
best_grid2 = best_grid2 %>% arrange(cost)
best_grid2 # 9470.400 0.9745516
# 17251.90 0.9816095

# ----- eta & nrounds
eta = seq(best_param_sm[1,]$eta-0.05,best_param_sm[1,]$eta+0.05,0.02) # 0.02 간격으로
nrounds = seq(best_param_sm[1,]$nrounds-100,best_param_sm[1,]$nrounds+200,100)

grid_3 = data.frame(
  eta = rep(eta,length(nrounds)),
  nrounds = rep(nrounds,each = length(eta)),
  cost = rep(NA,length(nrounds) * length(eta)),
  f1 =  rep(NA,length(nrounds) * length(eta))
)

# ,weight = ifelse(cv_tr_label == 1,30,1)
xgb_grid3 = function(param_set, opt_random_param_set, train_onehot, train_label) {
  
  pb = progress_bar$new(total = nrow(param_set))
  
  for (i in 1:nrow(param_set)) {
    # parameter set
    param_3 = list(
      max_depth = best_grid1[1,]$max_depth,
      min_child_weight = best_grid1[1,]$min_child_weight,
      subsample = best_grid2[1,]$subsample,
      colsample_bytree = best_grid2[1,]$colsample_bytree,
      eta = param_set[i,'eta'],
      objective = "binary:logistic",
      eval_metric = 'error',
      gamma = 0
    )

    # cv fold
    set.seed(123)
    cv = createFolds(train_label,k = 5)
    cost = NULL
    f1 = NULL
    
    for (j in 1:5) {
      valid_idx = cv[[j]]
      cv_tr_dummy = train_onehot[-valid_idx,]
      cv_te_dummy = train_onehot[valid_idx,]
      cv_tr_label = cv_tr_dummy$class
      cv_te_label = cv_te_dummy$class
      xgb_cv_tr = xgb.DMatrix(as.matrix(select(cv_tr_dummy,-class)),label = cv_tr_label)
      xgb_cv_te = xgb.DMatrix(as.matrix(select(cv_te_dummy,-class)),label = cv_te_label)
      watchlist <- list(train = xgb_cv_tr, test = xgb_cv_te)
      
      # cv
      set.seed(123)
      cv_xgb = xgb.train(
        params = param_3,
        data = xgb_cv_tr,
        nrounds = param_set[i,'nrounds'],
        early_stopping_rounds = param_set[i,'nrounds'],
        watchlist = watchlist,
        verbose = F)
      
      cv_pred = predict(cv_xgb,newdata = xgb_cv_te)
      cv_pred = ifelse(cv_pred >= 0.5, 1, 0)
      temp_cost = compute_cost(as.integer(cv_te_label),cv_pred) * 8000 / nrow(xgb_cv_te)
      temp_f1 = F1_Score(cv_te_label,cv_pred)
      cost = c(cost,temp_cost)
      f1 = c(f1,temp_f1)
    }
    param_set[i,'cost'] = mean(cost)
    param_set[i,'f1'] = mean(f1)
    
    pb$tick()
    Sys.sleep(0.01)
  }
  return(param_set)
}

best_grid3 = xgb_grid3(param_set = grid_3,opt_random_param_set = best_param_sm,train_onehot = train_sm, train_label = train_sm_label)
best_grid3 = best_grid3 %>% arrange(cost)
best_grid3 # 9421.545 0.9751045
# 17202.90 0.9821414

# Prediction
best_grid = data.frame(
  max_depth = best_grid1[1,]$max_depth,
  min_child_weight = best_grid1[1,]$min_child_weight,
  subsample = best_grid2[1,]$subsample,
  colsample_bytree = best_grid2[1,]$colsample_bytree,
  eta = best_grid3[1,]$eta,
  nrounds = best_grid3[1,]$nrounds,
  objective = "binary:logistic",
  eval_metric = 'error',
  gamma = 0
)

# xgb_mat_train_sm = xgb.DMatrix(as.matrix(select(train_sm,-class)),label = train_sm_label,weight = ifelse(train_sm_label == 1,100,1))

xg_watchlist_sm <- list(train = xgb_mat_train_sm)

set.seed(123)
model_xgb_sm <- xgb.train(params = best_grid %>% select(-nrounds) %>% as.list(),
                            data = xgb_mat_train_sm,
                            watchlist = xg_watchlist_sm,
                            verbose = T,
                            print_every_n = 10,
                            nrounds = best_grid$nrounds,
                            early_stopping_rounds = 0.05 * best_grid$nrounds
)

predict_xgb_sm <- predict(model_xgb_sm, xgb_mat_test_sm)
predict_xgb_sm <- ifelse(predict_xgb_sm>=0.5, 1,0)
compute_cost(test_sm_label,predict_xgb_sm) * 8000 / nrow(xgb_mat_test_sm) # 100 : 5304.72
# 5928.41
F1_Score(test_sm_label,predict_xgb_sm) # 0.9761801
# 0.9829319
table(true = test_sm_label,predict = predict_xgb_sm)
confusionMatrix(test_sm_label %>% as.factor,predict_xgb_sm %>% as.factor)
