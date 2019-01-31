#----------------------------------#
#        Finacial Analytics        #
#                HW 1              #
#                                  #
#               Orange 6           #
#----------------------------------#

# Needed Libraries for Analysis #
library(gmodels)
library(vcd)
library(smbinning)
library(dplyr)
library(stringr)
library(sas7bdat)

# Load Data From CSV File #
accepts <- read.sas7bdat(file = "C:\\Users\\chels\\Desktop\\MSA\\Spring 1\\Financial analytics\\HW 1\\accepted_customers.sas7bdat")
#head(accepts)

# Understand Target Variable #

#Make a variable bad for ease of understanding
accepts$bad = accepts$GB
table(accepts$bad)

#make a good variable
accepts$good <- abs(accepts$GB - 1)
table(accepts$good)

# Create Training and Validation #
set.seed(12345)
train_id <- sample(seq_len(nrow(accepts)), size = floor(0.75*nrow(accepts)))

train <- accepts[train_id, ]
test <- accepts[-train_id, ]

table(train$good)
table(test$good)

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

#need to define a weight variable where goods are 3.23 more than bads??? double check this 


#Build Initial Logistic Regression #
# initial_score <- glm(data = train, bad ~ PERS_H_WOE + 
#                        AGE_WOE + 
#                        TMJOB1_WOE + 
#                        INCOME_WOE +
#                      weights = train$weight, family = "binomial")
# 
# summary(initial_score)