#----------------------------------#

#        Finacial Analytics        #

#                HW 1              #

#                                  #

#               Orange 6           #

#----------------------------------#



# Needed Libraries for Analysis #

install.packages('sas7bdat')
library(gmodels)

library(vcd)

library(smbinning)

library(dplyr)

library(stringr)

library(sas7bdat)



# Load Data From CSV File #

accepts <- read.sas7bdat(file = "C:\\Users\\molly\\OneDrive\\Documents\\_MSA 2018 COURSEWORK\\Financial\\Homework1_FA\\accepted_customers.sas7bdat")

head(accepts)



# Understand Target Variable #



#Make a variable bad for ease of understanding

accepts$bad = accepts$GB

table(accepts$bad)



#make a good variable

accepts$good <- abs(accepts$GB - 1)

table(accepts$good)



# Create Training and Validation #

set.seed(12345)

train_id <- sample(seq_len(nrow(accepts)), size = floor(0.7*nrow(accepts)))



train <- accepts[train_id, ]

test <- accepts[-train_id, ]



table(train$good)

table(test$good)



# Binning of Continuous Variables - IV >= 0.1 #

num_names <- names(train)[sapply(train, is.numeric)] # Gathering the names of numeric variables in data #
num_names


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

result_all_sig

# Generating Variables of Bins and WOE Values #

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



head(train)

View(train)

#need to define a weight variable where goods are 3.23 more than bads

#Since 96.77/3.23 = 29.95975, that is the weight of a good to a bad
train$weight = train$good*(96.77/3.23-1)+1
View(train)
test$weight = test$good*(96.77/3.23-1)+1

#Build Initial Logistic Regression #

initial_score <- glm(data = train, bad ~ PERS_H_WOE +
                       
                       AGE_WOE +
                       
                       TMJOB1_WOE +
                       
                       INCOME_WOE,
                       
                       weights = train$weight, family = "binomial")



summary(initial_score)







#############################################
#all new from here, pasted from class 3 code#
#############################################

# Evaluate the Initial Model - Training Data #
train$pred <- initial_score$fitted.values

smbinning.metrics(dataset = train, prediction = "pred", actualclass = "bad", report = 1)
smbinning.metrics(dataset = train, prediction = "pred", actualclass = "bad", plot = "ks")
smbinning.metrics(dataset = train, prediction = "pred", actualclass = "bad", plot = "auc")

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

test$pred <- predict(initial_score, newdata=test, type='response')


smbinning.metrics(dataset = test, prediction = "pred", actualclass = "bad", report = 1)
smbinning.metrics(dataset = test, prediction = "pred", actualclass = "bad", plot = "ks")
smbinning.metrics(dataset = test, prediction = "pred", actualclass = "bad", plot = "auc")


for(i in 1:length(result_all_sig)) {
accepts <- smbinning.gen(df = accepts, ivout = result_all_sig[[i]], chrname = paste(result_all_sig[[i]]$x, "_bin", sep = ""))
}

for (j in 1:length(result_all_sig)) {
  for (i in 1:nrow(accepts)) {
    bin_name <- paste(result_all_sig[[j]]$x, "_bin", sep = "")
    bin <- substr(accepts[[bin_name]][i], 2, 2)
    
    woe_name <- paste(result_all_sig[[j]]$x, "_WOE", sep = "")
    
    if(bin == 0) {
      bin <- dim(result_all_sig[[j]]$ivtable)[1] - 1
      accepts[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    } else {
      accepts[[woe_name]][i] <- result_all_sig[[j]]$ivtable[bin, "WoE"]
    }
  }
}






accepts$pred <- predict(initial_score, newdata=accepts, type='response')
smbinning.metrics(dataset = test, prediction = "pred", actualclass = "bad", report = 1)

# Add Scores to Initial Model #
#score of 500, odds at 20:1, double the odds at 50
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

hist(train$Score, breaks = 10, main = "Distribution of Scores", xlab = "Score")


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

accepts_scored <- rbind(train, test)

dim(train)
dim(test)
hist(accepts_scored$Score, breaks = 10, main = "Distribution of Scores", xlab = "Score")

# Reject Inference - Load Reject Data #
rejects <- read.sas7bdat(file = "C:\\Users\\molly\\OneDrive\\Documents\\_MSA 2018 COURSEWORK\\Financial\\Homework1_FA\\rejected_customers.sas7bdat")
View(rejects)
for(i in names(result_all_sig)) {
  result_all_sig[[i]]$bands[1] <- min(rejects[[i]], na.rm = TRUE)
  result_all_sig[[i]]$bands[length(result_all_sig[[i]]$bands)] <- max(rejects[[i]], na.rm = TRUE)
}
View(rejects)


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

rejects$bad <- as.numeric(rejects_scored$pred > 0.0603)
rejects$weight <- ifelse(rejects$bad == 1, 2.80, 0.59)
rejects$good <- abs(rejects$bad - 1)

comb_hard <- rbind(accepts, rejects) # New Combined Data Set #

dim(accepts)
dim(rejects)
dim(rejects_scored)


# Reject Inference - Parcelling #
parc <- seq(500, 725, 25)

accepts_scored$Score_parc <- cut(accepts_scored$Score, breaks = parc)
rejects_scored$Score_parc <- cut(rejects_scored$Score, breaks = parc)

table(accepts_scored$Score_parc, accepts_scored$bad)

parc_perc <- table(accepts_scored$Score_parc, accepts_scored$bad)[,2]/rowSums(table(accepts_scored$Score_parc, accepts_scored$bad))

rejects$bad <- 0

rej_bump <- 1.25

for(i in 1:(length(parc)-1)) {
  for(j in 1:length(rejects_scored$Score)) {
    if((rejects_scored$Score[j] > parc[i]) & 
       (rejects_scored$Score[j] <= parc[i+1]) & 
       (runif(n = 1, min = 0, max = 1) < (rej_bump*parc_perc[i]))) {
      rejects$bad[j] <- 1
    }
  }
}

table(rejects_scored$Score_parc, rejects$bad)

rejects$weight <- ifelse(rejects$bad == 1, 2.80, 0.59)
rejects$good <- abs(rejects$bad - 1)

comb_parc <- rbind(accepts, rejects) # New Combined Data Set #

# Reject Inference - Fuzzy Augmentation #
rejects_scored$pred <- predict(initial_score, newdata=rejects_scored, type='response')

rejects_g <- rejects
rejects_b <- rejects

rejects_g$bad <- 0
rejects_g$weight <- (1-rejects_scored$pred)*2.80
rejects_g$good <- 1

rejects_b$bad <- 1
rejects_b$weight <- (rejects_scored$pred)*0.59
rejects_b$good <- 0

comb_fuzz <- rbind(accepts, rejects_g, rejects_b)

# Build Final Scorecard Model #
comb <- comb_parc # Select which data set you want to use from above techniques #

set.seed(1234)
train_id <- sample(seq_len(nrow(comb)), size = floor(0.75*nrow(comb)))

train_comb <- comb[train_id, ]
test_comb <- comb[-train_id, ]

iv_summary <- smbinning.sumiv(df = train_comb, y = "good")

smbinning.sumiv.plot(iv_summary)
iv_summary

num_names <- names(train_comb)[sapply(train_comb, is.numeric)] # Gathering the names of numeric variables in data #

result_all_sig <- list() # Creating empty list to store all results #

for(i in 1:length(num_names)){
  check_res <- smbinning(df = train_comb, y = "good", x = num_names[i])
  
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

final_score <- glm(data = train_comb, bad ~ tot_tr_WOE +
                     age_oldest_tr_WOE +
                     tot_rev_line_WOE +
                     rev_util_WOE +
                     bureau_score_WOE 
                   , weights = train_comb$weight, family = "binomial")

summary(final_score)

train_comb$pred <- final_score$fitted.values

smbinning.metrics(dataset = train_comb, prediction = "pred", actualclass = "bad", report = 1)
smbinning.metrics(dataset = train_comb, prediction = "pred", actualclass = "bad", plot = "ks")
smbinning.metrics(dataset = train_comb, prediction = "pred", actualclass = "bad", plot = "auc")

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

test_comb$pred <- predict(final_score, newdata=test_comb, type='response')

smbinning.metrics(dataset = test_comb, prediction = "pred", actualclass = "bad", report = 1)
smbinning.metrics(dataset = test_comb, prediction = "pred", actualclass = "bad", plot = "ks")
smbinning.metrics(dataset = test_comb, prediction = "pred", actualclass = "bad", plot = "auc")

pdo <- 20
score <- 600
odds <- 50
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
colend <- ncol(train_comb)
train_comb$Score <- rowSums(train_comb[, colini:colend])

hist(train_comb$Score, breaks = 50, main = "Distribution of Scores", xlab = "Score")