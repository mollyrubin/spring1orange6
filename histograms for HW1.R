library(gmodels)
library(vcd)
library(smbinning)
library(dplyr)
library(stringr)
library(sas7bdat)
library(ggplot2)

# Load Data From CSV File #
#Do we want to do this with both the train and validation as one big data set or just one of them???
df_v <- read.sas7bdat(file = "C:\\Users\\chels\\Desktop\\MSA\\Spring 1\\Financial analytics\\HW 1\\from em\\em_save_validate.sas7bdat")
df_t <- read.sas7bdat(file = "C:\\Users\\chels\\Desktop\\MSA\\Spring 1\\Financial analytics\\HW 1\\from em\\em_save_train.sas7bdat")
df = rbind(df_v, df_t)
#View(df)
#colnames(df)

#renaming the prob of a bad
df$prob_default = df$P_GB1

#Group by the point value and get the mean of each of those groups
mean = group_by(df, SCORECARD_POINTS) %>% summarize(m = mean(prob_default))

#assign each value to either accept, reject, or Further Evaluate

#create a column for the card results
mean$card_result = ''

#fill in the results based on cutoffs
#there are 201 unique scores
#We choose the cutoff scores to be 500 and 520 to maximize profitability
for (i in 1:201){
  if (mean$SCORECARD_POINTS[i] < 500){
    mean$card_result[i] = "Reject" 
  }else if (mean$SCORECARD_POINTS[i] > 520){
    mean$card_result[i] = "Accept" 
  }else{
    mean$card_result[i] = "Evaluate Further"
  }
}
#View(mean)

#creating the histogram
ggplot(data=mean, aes(x=SCORECARD_POINTS, y=m, fill=card_result)) +
  geom_bar(stat="identity")+ scale_fill_manual(values=c("#10C60A", "#EED818", "#EC1D1D"))+ theme_minimal()+
  labs(title="Mapping System for an Individual's Score and Probability of Default ", 
       x="Score Card Value", y = "Probability of Default")+ guides(fill=guide_legend(title=NULL))



