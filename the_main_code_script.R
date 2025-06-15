#--------------------------------------------------
# Import libraries
#--------------------------------------------------

library(tidyverse) # metapackage with lots of helpful functions
library(gridExtra) # grid.arrange to make quick subplots
library(reshape2)
library(dplyr)
library(tidyr)
library(ggcorrplot)

library(Metrics)
library(lmtest)
install.packages("car")
library(car)
#### objective
#The primary objective of this project is to identify the key factors that significantly influence life expectancy. In essence, the aim is to address the question of 
#which variables a health organization can modify to enhance life expectancy in a particular location.

#--------------------------------------------------
# Import dataset
#--------------------------------------------------

life_exp_dt <- read.csv("/Users/burcuyesilyurt/Downloads/master/Statistics/project/Life Expectancy Data.csv")
View(life_exp_dt)


# Dependent Variable: Life Expentacy (Continous)
# Features: We have two groups of features: Health factors which are originally provided by the Global Health Observatory (GHO) and Economic factors which have been collected by the United Nation (UN).

# Data size
print("Dataset size: ")
dim(life_exp_dt)

#--------------------------------------------------
# Preprocessing
#--------------------------------------------------

# -- 1.  Rename some columns

data <- life_exp_dt %>% 
  rename('lifexp'='Life.expectancy',
         'percexp'='percentage.expenditure',
         'totexp'='Total.expenditure',
         'admort'='Adult.Mortality',
         'infmort'='infant.deaths',
         'u5deaths'='under.five.deaths',
         'hepb'='Hepatitis.B',
         'HIV'='HIV.AIDS',
         'thin1years' = 'thinness..1.19.years',
         'incomp_res' = 'Income.composition.of.resources') 

# -- 2. Missing values

missing.rows = dim(data)[1] -  dim(na.omit(data))[1]
sprintf("Dataset size: [%s]", toString(dim(data)))
sprintf("Missing rows: %s (%s%%)", missing.rows, round((missing.rows*100)/dim(data)[1], 2))

#Conclusion: Around 44% of the total data is missing

missing_counts <- data.frame(
  feature = factor(names(data)),
  counts = sapply(data, function(x) sum(is.na(x)))
)

# Order the data frame by counts in descending order
missing_counts <- missing_counts %>%
  arrange(desc(counts))

# Print the ordered data frame
print(missing_counts)

#Conclusion: The highest concentration of missing data is evident in variables such as population, GDP, and Hepatitis B. Additionally, 
# substantial gaps exist in Total Expenditure, Alcohol, Income Composition of Resources, and Schooling. 

# -- 3. Imputation
features_with_missing = c("Population","hepb","GDP","totexp","Alcohol","incomp_res","Schooling","BMI","thin1years","thinness.5.9.years","Polio","Diphtheria","lifexp","admort")
# Set plot dimensions

par(mfrow = c(2, 7))


# Loop through features with missing values and create boxplots

par(oma = c(2, 2, 0, 0))

# Create boxplots
par(mfrow = c(2, 7))
for (feature in features_with_missing) {
  boxplot(data[[feature]],
          ylab = feature,
          main = paste("Boxplot of", feature),
          col = "#FF6666",
          outcol = "#FF6666")
}

# Add a common y-axis label to the entire plot
mtext("Values", side = 2, line = 2, outer = TRUE)


for (feature in features_with_missing) {
  boxplot(data[[feature]],
          ylab = feature,
          main = paste("Boxplot of", feature),
          col = "#FF6666",
          outcol = "#FF6666")
}

#conclusions: We can see variables with many outliers as hepb, GDP, totexp, thin1years,thinness.5.9.years, polio, Diphtheria and admort
#With the variables with outliers we are going to use de median and the ones without outliers the mean
#Why? Imputing with the median is more robust to outliers. If the data has extreme values, the median may be a better choice to avoid the influence of outliers.





columns_to_process_median <- c("Population","hepb", "GDP", "totexp", "thin1years","thinness.5.9.years", "Polio", "Diphtheria", "admort")
medians_list <- list()

for (feature in columns_to_process_median) {
  medians_list[[feature]] <- median(data[[feature]], na.rm = TRUE)
  data[[feature]][is.na(data[[feature]])] <- median(data[[feature]], na.rm = TRUE)
}

columns_to_process_mean <- c("Alcohol","incomp_res","Schooling","BMI")
mean_list <- list()

for (feature in columns_to_process_mean){
  mean_list[[feature]] <- mean(data[[feature]], na.rm = TRUE)
  data[[feature]][is.na(data[[feature]])] <- mean(data[[feature]], na.rm = TRUE)
}

#We are going to delete de record with Life Expentacy  in null, since is our dependent variable (10 records)
data <- data[!is.na(data$lifexp), ]


#Check the imputations was done correctly

missing_counts <- data.frame(
  feature = factor(names(data)),
  counts = sapply(data, function(x) sum(is.na(x)))
)

# Order the data frame by counts in descending order
missing_counts <- missing_counts %>%
  arrange(desc(counts))

# Print the ordered data frame
print(missing_counts)


#-- 4. factor the variable status which is categorical
data$Status <- as.factor(data$Status)

#--------------------------------------------------
# EDA
#--------------------------------------------------

# --- 1. Distribution of our target variable

par(mfrow = c(1, 1))
hist(data$lifexp,
     col = "skyblue",  # Set the color of bars
     border = "black", # Set the color of bar borders
     main = "Histogram of Life Expectancy", # Add a main title
     xlab = "Life Expectancy", # Add a label for the x-axis
     ylab = "Frequency", # Add a label for the y-axis
     breaks = 20, # Adjust the number of bins
     xlim = c(min(data$lifexp, na.rm = TRUE), max(data$lifexp, na.rm = TRUE)) # Set x-axis limits
     
     )
  #Mean and std of our target variable
mean_expectancy <-  mean(data$lifexp, na.rm = TRUE)
std_expectancy <-  sd(data$lifexp, na.rm = TRUE)
print(cat("Mean life expectancy:", mean_expectancy, "\n","Std life expectancy:", std_expectancy, "\n" ))

# Conclusion: The target is a little left-skewed.

# --- 2.  Compare between developing and developed countries


ggplot(data, aes(x = Status, y = lifexp)) +
  geom_boxplot(fill = "#66c2a5",  alpha = 0.7) + 
  ggtitle("Life Expectancy by Country Status") +
  xlab("Country Status") +  # Add x-axis label
  ylab("Life Expectancy") +  # Add y-axis label 
  scale_fill_brewer(palette="Set1")
 

# More details by country status
data %>% 
  group_by(Status) %>% 
  summarize(count = n(),
            avg_lifexp = mean(lifexp, na.rm=TRUE))
#Compare one developing country (Colombia) vs one developed country (Slovakia)


developingC <- data %>%   filter(Country %in% c('Colombia'))
developedC <- data %>%   filter(Country %in% c('Portugal'))

# --lifexp


devdp1 <- ggplot(developingC, aes(Year, lifexp))+
  geom_smooth(aes(color=Country), show.legend=FALSE)+
  facet_grid(Country~.)

devdp2 <- ggplot(developedC, aes(Year, lifexp))+
  geom_smooth(aes(color=Country),show.legend=FALSE)+
  facet_grid(Country~.)
grid.arrange(devdp1, devdp2, nrow=1)


#-- Adult mortality

devdp1 <- ggplot(developingC, aes(Year, admort))+
  geom_smooth(aes(color=Country), show.legend=FALSE)+
  facet_grid(Country~.)

devdp2 <- ggplot(developedC, aes(Year, admort))+
  geom_smooth(aes(color=Country),show.legend=FALSE)+
  facet_grid(Country~.)
grid.arrange(devdp1, devdp2, nrow=1)

#-- Infant deaths 

devdp1 <- ggplot(developingC, aes(Year, infmort))+
  geom_smooth(aes(color=Country), show.legend=FALSE)+
  facet_grid(Country~.)

devdp2 <- ggplot(developedC, aes(Year, infmort))+
  geom_smooth(aes(color=Country),show.legend=FALSE)+
  facet_grid(Country~.)
grid.arrange(devdp1, devdp2, nrow=1)

## Distribution of life expectancy by year
data %>%
  ggplot() +
  geom_boxplot(aes(x = Year, y = lifexp, group = Year, fill = Year)) +
  ggtitle("Boxplot of Life Expectancy Over Years") +
  xlab("Year") +
  ylab("Life Expectancy") +
  theme_minimal()

#conclusion: in general the life expentancy have been increasing. How does it loop over status?




data_dev = data %>%   filter(Status %in% c('Developed'))

data_deving = data %>%   filter(Status %in% c('Developing'))

par(mfrow = c(2, 1))

data_dev %>%
  ggplot() +
  geom_boxplot(aes(x = Year, y = lifexp, group = Year, fill = Year)) +
  ggtitle("Boxplot of Life Expectancy Over Years Developed") +
  xlab("Year") +
  ylab("Life Expectancy") +
  theme_minimal()


data_deving %>%
  ggplot() +
  geom_boxplot(aes(x = Year, y = lifexp, group = Year, fill = Year)) +
  ggtitle("Boxplot of Life Expectancy Over Years Developing") +
  xlab("Year") +
  ylab("Life Expectancy") +
  theme_minimal()

# Combine the datasets into a single dataframe with an additional 'Group' column
data_combined <- bind_rows(
  mutate(data, Group = "All Countries"),
  mutate(data_dev, Group = "Developed Countries"),
  mutate(data_deving, Group = "Developing Countries")
)

# Create a boxplot with facets
data_combined %>%
  ggplot() +
  geom_boxplot(aes(x = as.factor(Year), y = lifexp, fill = Group), position = position_dodge(width = 0.8)) +
  ggtitle("Boxplot of Life Expectancy Over Years") +
  xlab("Year") +
  ylab("Life Expectancy") +
  theme_minimal()


#Conclusion: It is clearly evident that life expectancy in developed countries is growing at a faster rate than in developing countries

# 3. Correlations
sapply(data, typeof)

corr <- round(cor(subset(data, select =-c(Status, Country))), 3)
ggcorrplot(corr,type = "lower" )
corr

### High correlation between thin1years, thinness.5.9.years
      #percexp and GDP
      #infmort and BMI

data_EDA <- data[, !(names(data) %in% c("thin1years", "BMI"))]

#### ------------- Feature selection

library(leaps)
#1. Forward inclusion using the BIC criterion

par(mfrow=c(1,1))

#BIC: is a statistical criterion used in model selection and feature selection. It is a penalized likelihood criterion that balances the goodness
#of fit of the model with the number of parameters used in the model. In the context of feature selection.

#In the context of feature selection, the idea is to compare different models with different subsets of features and select the model with the 
#lowest BIC. Lower BIC values indicate a better trade-off between model fit and complexity.

regfit.fwd <- regsubsets(lifexp~.,data=data_EDA,nvmax=16,method="forward")
fwd.summary <-summary(regfit.fwd)

# fwd.summary
plot(fwd.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
which.min(fwd.summary$bic)
points(12,fwd.summary$bic[12],col="red",cex=2,pch=20)

# -- Conclusion: 12 variables is the best subset
# What are the predicting variables actually affecting the life expectancy?

#List the variables

features <- rownames(as.data.frame(coef(regfit.fwd,12)))
coeffi<- data.frame(features)
coeffi$fwd_coef_value <-   coef(regfit.fwd,12)

ggplot(coeffi,
       aes(x=features, y=fwd_coef_value, fill=fwd_coef_value)) +
  geom_bar(stat="identity") +
  ggtitle("Features & coeffecients: [method Forward inclusion]") +
  xlab("Feature") + ylab("Coef value") +
  theme(axis.text.x=element_text(angle=20, hjust=1))+
  theme(text = element_text(size = 18))

#We have to remove the variable Country because it would be the main variable. We should also deleate the variable Year
#Now we are going to repeate the same process we did before for feature selection

data_EDA <- data_EDA[, !(names(data_EDA) %in% c("Country", "Year"))]

regfit.fwd <- regsubsets(lifexp~.,data=data_EDA,nvmax=16,method="forward")
fwd.summary <-summary(regfit.fwd)

# fwd.summary
plot(fwd.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
which.min(fwd.summary$bic)
points(12,fwd.summary$bic[12],col="red",cex=2,pch=20)

# -- Conclusion: 12 variables is the best subset

#List the variables

features <- rownames(as.data.frame(coef(regfit.fwd,12)))
coeffi<- data.frame(features)
coeffi$fwd_coef_value <-   coef(regfit.fwd,12)

ggplot(coeffi,
       aes(x=features, y=fwd_coef_value, fill=fwd_coef_value)) +
  geom_bar(stat="identity") +
  ggtitle("Features & coeffecients: [method Forward inclusion]") +
  xlab("Feature") + ylab("Coef value") +
  theme(axis.text.x=element_text(angle=20, hjust=1))+
  theme(text = element_text(size = 18))

#We should maybe try some transformation here..
data_FS <- subset(data_EDA, select=c(lifexp, admort, Diphtheria, GDP, hepb, HIV, incomp_res, Measles, Polio, Schooling, Status, thinness.5.9.years, totexp))

variable_list <- c('admort', 'Diphtheria', 'GDP', 'hepb', 'HIV', 'incomp_res', 'Measles', 'Polio', 'Schooling', 'Status', 'thinness.5.9.years', 'totexp')

# Create an empty list to store plots
plots_list <- list()

# Loop through the variables and create scatter plots
for (variable in variable_list) {
  # Scatter plot using ggplot2
  plot <- ggplot(data, aes_string(x = variable, y = "lifexp")) +
    geom_point(color = "blue", size = 3) +
    labs(title = paste("Scatter Plot of lifexp vs", variable), x = variable, y = "lifexp")
  
  # Append the plot to the list
  plots_list[[variable]] <- plot
}

# Combine all the plots into a single plot
grid.arrange(grobs = plots_list, ncol = 3)

#---- Linear regression:
# Does various predicting factors which has been chosen initially really affect the Life expectancy? 

set.seed(0)

sample <- sample(c(TRUE, FALSE), nrow(data_FS), replace=TRUE, prob=c(0.70,0.30))
train <- data_FS[sample, ]
x.test <-data_FS[!sample, ]
y.test <- data_FS[!sample, ]$lifexp
model<- lm(lifexp~., data = train)
summary(model)

#Probably we can delete totexp.. is in the limit

# -- Check Linearity assumptions for model

pred <- predict(model, newdata=x.test)
rmse(pred,y.test)
summary(model)$adj.r.squared
par(mfrow=c(2,2))
plot(model)

#*** There is no pattern in the residual plot
#***All the points spread along the reference line equally

## Test of Heteroskedasticity: Breusch-pagan test

bptest(model)
#Conclusion: reject the null hypothesis. This suggests evidence in favor of heteroscedasticity, indicating that the variance of errors is not constant 
# across all levels of the independent variables.


## Reset test:  check whether the functional form of a regression model is specified correctly.

resettest(model)

#conclusion: reject the null hypothesis. This suggests evidence in favor of a specification error in your regression model.



# Should a country having a lower life expectancy value(<5) increase its healthcare expenditure in order to improve its average lifespan?
cutoff_value = 5
lower_expenditure_group <- subset(data_FS, totexp < cutoff_value)
higher_expenditure_group <- subset(data_FS, totexp >= cutoff_value)

t_test_result <- t.test(lower_expenditure_group$lifexp, higher_expenditure_group$lifexp)

p_value <- t_test_result$p.value
if (p_value < 0.05) {
  print("There is a significant difference in life expectancy between the two groups.")
} else {
  print("There is no significant difference in life expectancy between the two groups.")
}


# How does Infant and Adult mortality rates affect life expectancy?
model <- lm(lifexp ~ infmort + admort, data = data_EDA)
summary(model)

coef_summary <- coef(summary(model))
p_values <- coef_summary[, "Pr(>|t|)"]
p_values
#Conclusion: Both are significants

# Does Life Expectancy has positive or negative correlation with drinking alcohol and life expectancy
corr <- round(cor(subset(data, select =c(Alcohol, lifexp))), 3)
ggcorrplot(corr,type = "lower"  )
corr
#Conclusion: The correlation is only 0.392 Not really high and is weird because according to this it suggests a positive relationship!


# Does Life Expectancy have positive or negative relationship with drinking alcohol?
# Assuming 'data' is your dataframe
model <- lm(lifexp ~ Alcohol, data = data)
summary(model) 
#Conclusion: same conclusion as before. Remember that correlation does not imply causation
# Do densely populated countries tend to have lower life expectancy?
correlation_coef <- cor(data$lifexp, data$Population)
correlation_coef
model <- lm(lifexp ~ Population, data = data)
summary(model) 
#Conclusion:  Densely populated countries tend to have lowe life expectancy 

