## Financial Analytics HW 1    ###
## Trevor Edge     1/22/20     ###
## Spring Module 1 - Orange 11 ###

rm(list = ls())

# Loading packages #
#install.packages("gmodels")
#install.packages("vcd")
#install.packages("smbinning")
#install.packages("dplyr")
#install.packages("stringr")
#install.packages("haven")

library(gmodels)
library(vcd)
library(smbinning)
library(dplyr)
library(stringr)
library(haven)

# Loading datasets #
file.dir <- "C:/Users/trevo.DESKTOP-Q3G2N9L/Documents/Financial Analytics/HW's/HW1/"
input.file1 <- "accepted2.csv"
input.file2 <- "rejected2.csv"
accepts <- read.csv(paste(file.dir, input.file1, sep = ""))
rejects <- read.csv(paste(file.dir, input.file2, sep = ""))

accepts = subset(accepts, select = -Column1)
rejects = subset(rejects, select = -Column1)

accepts[accepts$CARDS == 'VISA mybank', 'CARDS'] <- "Other credit car"
accepts[accepts$CARDS == 'VISA Others', 'CARDS'] <- "Other credit car"
accepts[accepts$CARDS == 'American Express', 'CARDS'] <- "Other credit car"
accepts$CARDS <- droplevels(accepts$CARDS)

rejects[rejects$CARDS == 'VISA Citibank', "CARDS"] <- "Other credit car"
rejects[rejects$CARDS == 'VISA Others', "CARDS"] <- "Other credit car"
rejects$CARDS <- droplevels(rejects$CARDS)

# Target variable tabels #
table(accepts$GB)

accepts$good <- abs(accepts$GB - 1)
table(accepts$good)

# Investigating categorical variables #

table(accepts$BUREAU)
table(accepts$CAR)
table(accepts$CARDS)
table(accepts$DIV)
table(accepts$EC_CARD)
table(accepts$FINLOAN)
table(accepts$LOCATION)
table(accepts$NAT)
table(accepts$PRODUCT)
table(accepts$PROF)
table(accepts$REGN)
table(accepts$RESID)
table(accepts$STATUS)
table(accepts$TEL)
table(accepts$TITLE)

# Setting Categorical Variables as Factors #
accepts$BUREAU <- as.factor(accepts$BUREAU)
accepts$CAR <- as.factor(accepts$CAR)
accepts$CARDS <- as.factor(accepts$CARDS)
accepts$DIV <- as.factor(accepts$DIV)
accepts$EC_CARD <- as.factor(accepts$EC_CARD)
accepts$FINLOAN <- as.factor(accepts$FINLOAN)
accepts$LOCATION <- as.factor(accepts$LOCATION)
accepts$NAT <- as.factor(accepts$NAT)
accepts$PRODUCT <- as.factor(accepts$PRODUCT)
accepts$PROF <- as.factor(accepts$PROF)
accepts$REGN <- as.factor(accepts$REGN)
accepts$RESID <- as.factor(accepts$RESID)
accepts$STATUS <- as.factor(accepts$STATUS)
accepts$TEL <- as.factor(accepts$TEL)
accepts$TITLE <- as.factor(accepts$TITLE)

rejects$BUREAU <- as.factor(rejects$BUREAU)
rejects$CAR <- as.factor(rejects$CAR)
rejects$CARDS <- as.factor(rejects$CARDS)
rejects$DIV <- as.factor(rejects$DIV)
rejects$EC_CARD <- as.factor(rejects$EC_CARD)
rejects$FINLOAN <- as.factor(rejects$FINLOAN)
rejects$LOCATION <- as.factor(rejects$LOCATION)
rejects$NAT <- as.factor(rejects$NAT)
rejects$PRODUCT <- as.factor(rejects$PRODUCT)
rejects$PROF <- as.factor(rejects$PROF)
rejects$REGN <- as.factor(rejects$REGN)
rejects$RESID <- as.factor(rejects$RESID)
rejects$STATUS <- as.factor(rejects$STATUS)
rejects$TEL <- as.factor(rejects$TEL)
rejects$TITLE <- as.factor(rejects$TITLE)

# Create Training and Validation #
set.seed(12345)
train_id <- sample(seq_len(nrow(accepts)), size = floor(0.70*nrow(accepts)))

train <- accepts[train_id, ]
test <- accepts[-train_id, ]

table(train$good)
table(test$good)

# Information Value for Each Variable #
iv_summary <- smbinning.sumiv(df = train, y = "good")

smbinning.sumiv.plot(iv_summary)
iv_summary # Only Continuous Variables >= 0.1 IV #

# Binning of Continuous Variables - IV >= 0.1 #
num_names <- names(train)[sapply(train, is.numeric)] # Gathering the names of numeric variables in data #

result_all_sig <- list() # Creating empty list to store all results #

for(i in 1:length(num_names)){
  check_res <- smbinning(df = train, y = "good", x = num_names[i])
  
  if(check_res == "Uniques values < 5") {
    next
  }
  else if(check_res == "No significant splits") {
    next
  }
  else if(check_res$iv < 0.1) {
    next
  }
  else {
    result_all_sig[[num_names[i]]] <- check_res
  }
}

# Binning of Categorical Variables - IV >= 0.1 #
catvarsimp <- c("STATUS", "CARDS", "EC_CARD") # Gathering the names of categorical variables in data that are important #

result_impcatvars <- list() # Creating empty list to store all results #

for(i in 1:length(num_names)){
  check_rescat <- smbinning.factor(df = train, y = "good", x = catvarsimp[i])
  
  if(check_rescat == "Uniques values < 5") {
    next
  }
  else if(check_rescat == "No significant splits") {
    next
  }
  else if(check_rescat$iv < 0.1) {
    next
  }
  else {
    result_impcatvars[[catvarsimp[i]]] <- check_rescat
  }
}

# Generating Variables of Bins and WOE Values for Continuous Variables that were important #
for(i in 1:length(result_all_sig)) {
  train <- smbinning.gen(df = train, ivout = result_all_sig[[i]], chrname = paste(result_all_sig[[i]]$x, "_bin", sep = ""))
}

for (j in 1:length(result_all_sig)) {
  for (i in 1:nrow(train)) {
    bin_name <- paste(result_all_sig[[j]]$x, "_bin", sep = "")
    bin <- substr(train[[bin_name]][i], 2, 2)
    
    woe_name <- paste(result_all_sig[[j]]$x, "_WOE", sep = "")
    
    if(bin == 0) {
      bin <- dim(result_all_sig[[j]]$ivtable)[1] - 1
      train[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    } else {
      train[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    }
  }
}

# Generating Variables of Bins and WOE Values for Categorical Important Variables #
for(i in 1:length(result_impcatvars)) {
  train <- smbinning.factor.gen(df = train, ivout = result_impcatvars[[i]], chrname = paste(result_impcatvars[[i]]$x, "_bin", sep = ""))
}

for (j in 1:length(result_impcatvars)) {
  for (i in 1:nrow(train)) {
    bin_name <- paste(result_impcatvars[[j]]$x, "_bin", sep = "")
    bin <- substr(train[[bin_name]][i], 2, 2)
    
    woe_name <- paste(result_impcatvars[[j]]$x, "_WOE", sep = "")
    
    if(bin == 0) {
      bin <- dim(result_impcatvars[[j]]$ivtable)[1] - 1
      train[[woe_name]][i] <- result_impcatvars[[j]]$ivtable[bin, "WoE"]
    } else {
      train[[woe_name]][i] <- result_impcatvars[[j]]$ivtable[bin, "WoE"]
    }
  }
}

# Build Initial Logistic Regression #
#     CARDS variable not working so binning some together then going back and redoing WOE for CARDS variable - Edit is at the top of the script#
#     One observation does not have a WOE - id = 2914 #

initial_score <- glm(data = train, GB ~ PERS_H_WOE +
                       TMJOB1_WOE + 
                       INCOME_WOE +
                       STATUS_WOE +
                       EC_CARD_WOE +
                       CARDS_WOE,
                     weights = train$weight, family = "binomial")
summary(initial_score)

# Evaluate the Initial Model - Training Data #
train$pred <- initial_score$fitted.values

smbinning.metrics(dataset = train, prediction = "pred", actualclass = "GB", report = 1)
smbinning.metrics(dataset = train, prediction = "pred", actualclass = "GB", plot = "ks")
smbinning.metrics(dataset = train, prediction = "pred", actualclass = "GB", plot = "auc")

#Evaluate the Initial Model - Testing Data #
for(i in 1:length(result_all_sig)) {
  test <- smbinning.gen(df = test, ivout = result_all_sig[[i]], chrname = paste(result_all_sig[[i]]$x, "_bin", sep = ""))
}

for (j in 1:length(result_all_sig)) {
  for (i in 1:nrow(test)) {
    bin_name <- paste(result_all_sig[[j]]$x, "_bin", sep = "")
    bin <- substr(test[[bin_name]][i], 2, 2)
    
    woe_name <- paste(result_all_sig[[j]]$x, "_WOE", sep = "")
    
    if(bin == 0) {
      bin <- dim(result_all_sig[[j]]$ivtable)[1] - 1
      test[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    } else {
      test[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    }
  }
}

for(i in 1:length(result_impcatvars)) {
  test <- smbinning.factor.gen(df = test, ivout = result_impcatvars[[i]], chrname = paste(result_impcatvars[[i]]$x, "_bin", sep = ""))
}

for (j in 1:length(result_impcatvars)) {
  for (i in 1:nrow(test)) {
    bin_name <- paste(result_impcatvars[[j]]$x, "_bin", sep = "")
    bin <- substr(test[[bin_name]][i], 2, 2)
    
    woe_name <- paste(result_impcatvars[[j]]$x, "_WOE", sep = "")
    
    if(bin == 0) {
      bin <- dim(result_impcatvars[[j]]$ivtable)[1] - 1
      test[[woe_name]][i] <- result_impcatvars[[j]]$ivtable[bin, "WoE"]
    } else {
      test[[woe_name]][i] <- result_impcatvars[[j]]$ivtable[bin, "WoE"]
    }
  }
}

test$pred <- predict(initial_score, newdata=test, type='response')

smbinning.metrics(dataset = test, prediction = "pred", actualclass = "GB", report = 1)
smbinning.metrics(dataset = test, prediction = "pred", actualclass = "GB", plot = "ks")
smbinning.metrics(dataset = test, prediction = "pred", actualclass = "GB", plot = "auc")

# Add Scores to Initial Model #
pdo <- 50
score <- 500
odds <- 20
fact <- pdo/log(2)
os <- score - fact*log(odds)
var_names <- names(initial_score$coefficients[-1])

for(i in var_names) {
  beta <- initial_score$coefficients[i]
  beta0 <- initial_score$coefficients["(Intercept)"]
  nvar <- length(var_names)
  WOE_var <- train[[i]]
  points_name <- paste(str_sub(i, end = -4), "points", sep="")
  train[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*fact + os/nvar
}

colini <- (ncol(train)-nvar + 1)
colend <- ncol(train)
train$Score <- rowSums(train[, colini:colend])

hist(train$Score, breaks = 50, xlim = c(400,700), main = "Distribution of Train Scores", xlab = "Score")

for(i in var_names) {
  beta <- initial_score$coefficients[i]
  beta0 <- initial_score$coefficients["(Intercept)"]
  nvar <- length(var_names)
  WOE_var <- test[[i]]
  points_name <- paste(str_sub(i, end = -4), "points", sep="")
  
  test[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*fact + os/nvar
}

colini <- (ncol(test)-nvar + 1)
colend <- ncol(test)
test$Score <- rowSums(test[, colini:colend])

hist(test$Score, breaks = 50, xlim = c(400,700), main = "Distribution of Test Scores", xlab = "Score")

accepts_scored <- rbind(train, test)
hist(accepts_scored$Score, breaks = 50, xlim = c(400,700), main = "Distribution of Scores", xlab = "Score")

# Reject Inference - Clean & Prepare Reject Data #
for(i in names(result_all_sig)) {
  result_all_sig[[i]]$bands[1] <- min(c(accepts[[i]], rejects[[i]]), na.rm = TRUE)
  result_all_sig[[i]]$bands[length(result_all_sig[[i]]$bands)] <- max(c(accepts[[i]], rejects[[i]]), na.rm = TRUE)
}

for(i in names(result_impcatvars)) {
  result_impcatvars[[i]]$bands[1] <- min(c(accepts[[i]], rejects[[i]]), na.rm = TRUE)
  result_impcatvars[[i]]$bands[length(result_impcatvars[[i]]$bands)] <- max(c(accepts[[i]], rejects[[i]]), na.rm = TRUE)
}

# Preparing rejects data set
rejects_scored <- rejects
for(i in 1:length(result_all_sig)) {
  rejects_scored <- smbinning.gen(df = rejects_scored, ivout = result_all_sig[[i]], chrname = paste(result_all_sig[[i]]$x, "_bin", sep = ""))
}

for (j in 1:length(result_all_sig)) {
  for (i in 1:nrow(rejects_scored)) {
    bin_name <- paste(result_all_sig[[j]]$x, "_bin", sep = "")
    bin <- substr(rejects_scored[[bin_name]][i], 2, 2)
    
    woe_name <- paste(result_all_sig[[j]]$x, "_WOE", sep = "")
    
    if(bin == 0) {
      bin <- dim(result_all_sig[[j]]$ivtable)[1] - 1
      rejects_scored[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    } else {
      rejects_scored[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    }
  }
}

for(i in 1:length(result_impcatvars)) {
  rejects_scored <- smbinning.factor.gen(df = rejects_scored, ivout = result_impcatvars[[i]], chrname = paste(result_impcatvars[[i]]$x, "_bin", sep = ""))
}

for (j in 1:length(result_impcatvars)) {
  for (i in 1:nrow(rejects_scored)) {
    bin_name <- paste(result_impcatvars[[j]]$x, "_bin", sep = "")
    bin <- substr(rejects_scored[[bin_name]][i], 2, 2)
    
    woe_name <- paste(result_impcatvars[[j]]$x, "_WOE", sep = "")
    
    if(bin == 0) {
      bin <- dim(result_impcatvars[[j]]$ivtable)[1] - 1
      rejects_scored[[woe_name]][i] <- result_impcatvars[[j]]$ivtable[bin, "WoE"]
    } else {
      rejects_scored[[woe_name]][i] <- result_impcatvars[[j]]$ivtable[bin, "WoE"]
    }
  }
}

pdo <- 50
score <- 500
odds <- 20
fact <- pdo/log(2)
os <- score - fact*log(odds)
var_names <- names(initial_score$coefficients[-1])

for(i in var_names) {
  beta <- initial_score$coefficients[i]
  beta0 <- initial_score$coefficients["(Intercept)"]
  nvar <- length(var_names)
  WOE_var <- rejects_scored[[i]]
  points_name <- paste(str_sub(i, end = -4), "points", sep="")
  rejects_scored[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*fact + os/nvar
}

colini <- (ncol(rejects_scored)-nvar + 1)
colend <- ncol(rejects_scored)
rejects_scored$Score <- rowSums(rejects_scored[, colini:colend])

# Reject Inference - Hard Cut-off #
rejects_scored$pred <- predict(initial_score, newdata=rejects_scored, type='response')

rejects$GB <- as.numeric(rejects_scored$pred > 0.0353)
rejects$weight <- ifelse(rejects$GB == 0, 30, 1)
rejects$good <- abs(rejects$GB - 1)

comb_hard <- rbind(accepts, rejects) # New Combined Data Set #

# New Weights for Good - 32.01194 #
table(comb_hard$GB)
x <- 30/(2177/2323)
print(x)

comb_hard$weight <- ifelse(comb_hard$GB == 0, 32.01194, 1)

# Build Final Scorecard Model #
comb <- comb_hard # Select which data set you want to use from above techniques #

set.seed(1234)
train_id <- sample(seq_len(nrow(comb)), size = floor(0.75*nrow(comb)))

train_comb <- comb[train_id, ]
test_comb <- comb[-train_id, ]

iv_summary <- smbinning.sumiv(df = comb_hard, y = "GB")

smbinning.sumiv.plot(iv_summary)
iv_summary

num_names <- names(train_comb)[sapply(train_comb, is.numeric)] # Gathering the names of numeric variables in data #

result_all_sig <- list() # Creating empty list to store all results #

for(i in 1:length(num_names)){
  check_res <- smbinning(df = train_comb, y = "GB", x = num_names[i])
  
  if(check_res == "Uniques values < 5") {
    next
  }
  else if(check_res == "No significant splits") {
    next
  }
  else if(check_res$iv < 0.1) {
    next
  }
  else {
    result_all_sig[[num_names[i]]] <- check_res
  }
}

for(i in 1:length(result_all_sig)) {
  train_comb <- smbinning.gen(df = train_comb, ivout = result_all_sig[[i]], chrname = paste(result_all_sig[[i]]$x, "_bin", sep = ""))
}

for (j in 1:length(result_all_sig)) {
  for (i in 1:nrow(train_comb)) {
    bin_name <- paste(result_all_sig[[j]]$x, "_bin", sep = "")
    bin <- substr(train_comb[[bin_name]][i], 2, 2)
    
    woe_name <- paste(result_all_sig[[j]]$x, "_WOE", sep = "")
    
    if(bin == 0) {
      bin <- dim(result_all_sig[[j]]$ivtable)[1] - 1
      train_comb[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    } else {
      train_comb[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    }
  }
}


##### GET ERROR HERE IN OUTPUT DATA ###
cat_names <- names(train_comb)[sapply(train_comb, is.factor)] # Gathering the names of categorical variables in data #

result_impcatvars <- list() # Creating empty list to store all results #

for(i in 1:length(cat_names)){
  check_res2 <- smbinning.factor(df = train_comb, y = "GB", x = cat_names[i])
  
  if(check_res2 == "Uniques values < 5") {
    next
  }
  else if(check_res2 == "No significant splits") {
    next
  }
  else if(check_res2$iv < 0.1) {
    next
  }
  else {
    result_impcatvars[[cat_names[i]]] <- check_res2
  }
}

for(i in 1:length(result_impcatvars)) {
  train_comb <- smbinning.factor.gen(df = train_comb, ivout = result_impcatvars[[i]], chrname = paste(result_impcatvars[[i]]$x, "_bin", sep = ""))
}

for (j in 1:length(result_impcatvars)) {
  for (i in 1:nrow(train_comb)) {
    bin_name <- paste(result_impcatvars[[j]]$x, "_bin", sep = "")
    bin <- substr(train_comb[[bin_name]][i], 2, 2)
    
    woe_name <- paste(result_impcatvars[[j]]$x, "_WOE", sep = "")
    
    if(bin == 0) {
      bin <- dim(result_impcatvars[[j]]$ivtable)[1] - 1
      train_comb[[woe_name]][i] <- result_impcatvars[[j]]$ivtable[bin, "WoE"]
    } else {
      train_comb[[woe_name]][i] <- result_impcatvars[[j]]$ivtable[bin, "WoE"]
    }
  }
}

train_comb <- train_comb[, -c(46:49)]
train_comb <- train_comb[, -c(39:42)]

final_score <- glm(data = train_comb, GB ~ PERS_H_WOE +
                     STATUS_WOE + TMJOB1_WOE + EC_CARD_WOE + CARDS_WOE + CHILDREN_WOE,
                    weights = train_comb$weight, family = "binomial")

summary(final_score)

train_comb$pred <- final_score$fitted.values

smbinning.metrics(dataset = train_comb, prediction = "pred", actualclass = "GB", report = 1)
smbinning.metrics(dataset = train_comb, prediction = "pred", actualclass = "GB", plot = "ks")
smbinning.metrics(dataset = train_comb, prediction = "pred", actualclass = "GB", plot = "auc")

for(i in 1:length(result_all_sig)) {
  test_comb <- smbinning.gen(df = test_comb, ivout = result_all_sig[[i]], chrname = paste(result_all_sig[[i]]$x, "_bin", sep = ""))
}

for (j in 1:length(result_all_sig)) {
  for (i in 1:nrow(test_comb)) {
    bin_name <- paste(result_all_sig[[j]]$x, "_bin", sep = "")
    bin <- substr(test_comb[[bin_name]][i], 2, 2)
    
    woe_name <- paste(result_all_sig[[j]]$x, "_WOE", sep = "")
    
    if(bin == 0) {
      bin <- dim(result_all_sig[[j]]$ivtable)[1] - 1
      test_comb[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    } else {
      test_comb[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    }
  }
}

for(i in 1:length(result_impcatvars)) {
  test_comb <- smbinning.factor.gen(df = test_comb, ivout = result_impcatvars[[i]], chrname = paste(result_impcatvars[[i]]$x, "_bin", sep = ""))
}

for (j in 1:length(result_impcatvars)) {
  for (i in 1:nrow(test_comb)) {
    bin_name <- paste(result_impcatvars[[j]]$x, "_bin", sep = "")
    bin <- substr(test_comb[[bin_name]][i], 2, 2)
    
    woe_name <- paste(result_impcatvars[[j]]$x, "_WOE", sep = "")
    
    if(bin == 0) {
      bin <- dim(result_impcatvars[[j]]$ivtable)[1] - 1
      test_comb[[woe_name]][i] <- result_impcatvars[[j]]$ivtable[bin, "WoE"]
    } else {
      test_comb[[woe_name]][i] <- result_impcatvars[[j]]$ivtable[bin, "WoE"]
    }
  }
}

test_comb <- test_comb[, -c(46:49)]
test_comb <- test_comb[, -c(39:42)]

test_comb$pred <- predict(final_score, newdata=test_comb, type='response')

smbinning.metrics(dataset = test_comb, prediction = "pred", actualclass = "GB", report = 1)
smbinning.metrics(dataset = test_comb, prediction = "pred", actualclass = "GB", plot = "ks")
smbinning.metrics(dataset = test_comb, prediction = "pred", actualclass = "GB", plot = "auc")

## Getting scores for train_comb data ##
pdo <- 50
score <- 500
odds <- 20
fact <- pdo/log(2)
os <- score - fact*log(odds)
var_names <- names(final_score$coefficients[-1])

for(i in var_names) {
  beta <- final_score$coefficients[i]
  beta0 <- final_score$coefficients["(Intercept)"]
  nvar <- length(var_names)
  WOE_var <- train_comb[[i]]
  points_name <- paste(str_sub(i, end = -4), "points", sep="")
  train_comb[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*fact + os/nvar
}

colini <- (ncol(train_comb)-nvar + 1)
print(colini)
colend <- ncol(train_comb)
print(colend)
train_comb$Score <- rowSums(train_comb[,colini:colend])

hist(train_comb$Score, breaks = 50, main = "Distribution of Scores", xlab = "Score")

## Getting scores for test_comb data to rbind the train_comb and test_comb ##
pdo <- 50
score <- 500
odds <- 20
fact <- pdo/log(2)
os <- score - fact*log(odds)
var_names <- names(final_score$coefficients[-1])

for(i in var_names) {
  beta <- final_score$coefficients[i]
  beta0 <- final_score$coefficients["(Intercept)"]
  nvar <- length(var_names)
  WOE_var <- test_comb[[i]]
  points_name <- paste(str_sub(i, end = -4), "points", sep="")
  test_comb[[points_name]] <- -(WOE_var*(beta) + (beta0/nvar))*fact + os/nvar
}

colini <- (ncol(test_comb)-nvar + 1)
colend <- ncol(test_comb)
test_comb$Score <- rowSums(test_comb[,colini:colend])

hist(test_comb$Score, breaks = 50, main = "Distribution of Scores", xlab = "Score")

# Getting distinct values for scorecard #
train_comb %>% distinct(CHILDREN_bin) 
train_comb %>% distinct(PERS_H_bin)
train_comb %>% distinct(TMJOB1_bin)
train_comb %>% distinct(EC_CARD_bin)
train_comb %>% distinct(CARDS_bin)
train_comb %>% distinct(STATUS_bin)

# How to get final cutoffs for actual scores # #JUST FIND TOP 75% OF TOTAL DATA AND USE THAT AS CUTOFF #
# Deciles #

train_comb2 = subset(train_comb, select = c(GB, Score))
test_comb2 = subset(test_comb, select = c(GB, Score))
All_comb <- rbind(train_comb2, test_comb2)

All_comb2 <- All_comb[order(All_comb$Score),]

All_comb2$obs <- seq(1:length(All_comb2$Score))
All_comb2$obs <- as.numeric(All_comb2$obs)

# Creating deciles #
All_comb2$decile <- ifelse((All_comb2$obs > 0) & (All_comb2$obs <= 450), "0-10%", 
              ifelse((All_comb2$obs >= 451) & (All_comb2$obs <= 900), "10-20%",
                        ifelse((All_comb2$obs >= 901) & (All_comb2$obs <= 1350), "20-30%", 
                               ifelse((All_comb2$obs >= 1351) & (All_comb2$obs <= 1800), "30-40%",
                                      ifelse((All_comb2$obs >= 1801) & (All_comb2$obs <= 2250), "40-50%",
                                             ifelse((All_comb2$obs >= 2251) & (All_comb2$obs <= 2700), "50-60%",
                                                    ifelse((All_comb2$obs >= 2701) & (All_comb2$obs <= 3150), "60-70%",
                                                           ifelse((All_comb2$obs >= 3151) & (All_comb2$obs <= 3600), "70-80%",
                                                                  ifelse((All_comb2$obs >= 3601) & (All_comb2$obs <= 4050), "80-90%", "90-100%")))))))))
# Getting default rate for each decile #
#   0-10% def_rate = 0.88
#  10-20% def_rate = 0.8377778
#  20-30% def_rate = 0.8133333
#  30-40% def_rate = 0.7288889
#  40-50% def_rate = 0.5777778
#  50-60% def_rate = 0.3533333
#  60-70% def_rate = 0.2822222
#  70-80% def_rate = 0.2688889
#  80-90% def_rate = 0.2133333
# 90-100% def_rate = 0.2066667
All_comb2 %>%
  filter(decile == "0-10%") %>%
  summarise(def_rate = sum(GB)/450)
All_comb2 %>%
  filter(decile == "10-20%") %>%
  summarise(def_rate = sum(GB)/450)
All_comb2 %>%
  filter(decile == "20-30%") %>%
  summarise(def_rate = sum(GB)/450)
All_comb2 %>%
  filter(decile == "30-40%") %>%
  summarise(def_rate = sum(GB)/450)
All_comb2 %>%
  filter(decile == "40-50%") %>%
  summarise(def_rate = sum(GB)/450)
All_comb2 %>%
  filter(decile == "50-60%") %>%
  summarise(def_rate = sum(GB)/450)
All_comb2 %>%
  filter(decile == "60-70%") %>%
  summarise(def_rate = sum(GB)/450)
All_comb2 %>%
  filter(decile == "70-80%") %>%
  summarise(def_rate = sum(GB)/450)
All_comb2 %>%
  filter(decile == "80-90%") %>%
  summarise(def_rate = sum(GB)/450)
All_comb2 %>%
  filter(decile == "90-100%") %>%
  summarise(def_rate = sum(GB)/450)


# Maximizing profit while minimizing default rate
# 75% Acceptance rate
accept_rate <- train_comb[order(-train_comb$Score),]
accept_rate_75 <- accept_rate[1:2531,]
table(accept_rate_75$GB)

# Weight of goods times $2000 * # Goods  - $52000* # Bads
# Results in $44,628,274 in profits
32.01194*2000*1519 -52000*1012

# # Bads / (# Bads + Weight of goods times # Goods)
# Event rate of 0.02038755 (2.04%)
1012/(1012+32.01194*1519)


# Getting final dataset ready
bigcomb <- rbind(train_comb, test_comb)
bigcomb2 <- bigcomb[order(-bigcomb$Score),]

# Getting final cutoff value #
# Final cutoff value should be == 442 - Acceptance rate of ~73%
bigcomb3 <- bigcomb2[1:3290,] #Getting the top 75% and a couple extra obs to see where final cutoff should be
table(bigcomb3$GB)

# 73% Cutoff
# Cutoff of 442 score - 73% of the data (accepts)
# Profit on whole dataset = $60,387,641
32.01194*2000*1995 -52000*1295

# Event rate on top 73% of data (new accepts)
#Event rate = 0.01987452
1295/(1295+32.01194*1995)

# Final cutoff value should be == - Acceptance rate of ~  %
bigcomb4 <- bigcomb2[1:3922,] #Getting the top 75% and a couple extra obs to see where final cutoff should be
table(bigcomb4$GB)

# 87% Cutoff
# Cutoff of 440 score - 87% of the data (accepts)
# Profit on whole dataset = $40,750,363
32.01194*2000*2109 -52000*1813
32.01194*2000*2109
52000*1813

# Event rate on top 73% of data (new accepts)
#Event rate = 0.01987452
1813/(1813+32.01194*2109)

# Original Profit - $12,000,000 - based on 3000 accepts data set observations
# Original Revenue - $90,000,000
# Original Costs - $78,000,000
table(accepts$GB)
30*2000*1500 -52000*1500
30*2000*1500
52000*1500