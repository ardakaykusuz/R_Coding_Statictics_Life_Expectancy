## Life Expactancy

The dataset provides information on life expectancy, health and economic factors of a large number of countries over a period of years. The objective of this analysis is to find the main factors that influence life expectancy. We will identify the variables that are most significantly associated with life expectancy and build a linear regression model to forecast the life expectancy of a country based on its health and economic factors. The description of each attribute can be find below.

*Life expectancy:* The average number of years a person can expect to live from birth, assuming that current mortality patterns will continue. *GDP per capita:* The total value of all goods and services produced in a country in a given year divided by the country's population. *Percentage Expenditure:* The total amount of money a country spends on healthcare as a percentage of its GDP. *Thinness (5-9 years):* The percentage of children aged 5-9 years who are below the median weight for their height. *Adult mortality rates:* The number of deaths per 1,000 people aged 15 and older in a given year. *Infant deaths:* The number of deaths of children under 1 year of age per 1,000 live births in a given year. *Under-five deaths:* The number of deaths of children under 5 years of age per 1,000 live births in a given year. *Access to improved sanitation:* The percentage of the population with access to improved sanitation facilities, such as flush toilets or latrines. *Status:* A categorical variable that indicates whether a country is developed or developing. *Country:* The name of the country. *Total expenditure on healthcare:* This variable measures the total amount of money that a country spends on healthcare. *Hepatitis B:* This variable measures the prevalence of hepatitis B among the population. *HIV/AIDS:* This variable measures the prevalence of HIV/AIDS among the population.

## Import of the Data Set

```         
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

life_exp_dt <- read.csv("/Users/burcuyesilyurt/Downloads/master/Statistics/project/Life Expectancy Data.csv")
View(life_exp_dt)
```

## Data Preparation

The data used for this analysis was obtained from the World Bank's World Development Indicators dataset. The dataset includes information on life expectancy, health and economic factors for a large number of countries over a period of years.

The dataset was first preprocessed to handle missing values and data types. Missing values were imputed using the median or mean, depending on the variable's distribution. Categorical variables (Status and Country) were converted into factors.

```         
                              feature counts
Population                 Population    652
hepb                             hepb    553
GDP                               GDP    448
totexp                         totexp    226
Alcohol                       Alcohol    194
incomp_res                 incomp_res    167
Schooling                   Schooling    163
BMI                               BMI     34
thin1years                 thin1years     34
thinness.5.9.years thinness.5.9.years     34
Polio                           Polio     19
Diphtheria                 Diphtheria     19
lifexp                         lifexp     10
admort                         admort     10
Country                       Country      0
Year                             Year      0
Status                         Status      0
infmort                       infmort      0
percexp                       percexp      0
Measles                       Measles      0
u5deaths                     u5deaths      0
HIV                               HIV      0
```

Around 44% of the total data had missing values. Variables such as population, GDP, and Hepatitis B showed the highest concentration of missing data. Imputation was performed using the median for variables with outliers and the mean for those without.After see the missing values we also decided to create visualization.

```         
data_combined %>%
  ggplot() +
  geom_boxplot(aes(x = as.factor(Year), y = lifexp, fill = Group), position = position_dodge(width = 0.8)) +
  ggtitle("Boxplot of Life Expectancy Over Years") +
  xlab("Year") +
  ylab("Life Expectancy") +
  theme_minimal()
```

## EDA

We created histogram, and it revealed a slightly left-skewed distribution of life expectancy. A boxplot comparison of life expectancy between developing and developed countries was created. Trend analysis was performed for life expectancy, adult mortality, and infant deaths, comparing a developing country (Colombia) with a developed country (Portugal). Boxplots illustrated the distribution of life expectancy over the years. Developed and developing countries' life expectancy trends were analyzed separately.

```         
par(mfrow = c(2, 7))
for (feature in features_with_missing) {
  boxplot(data[[feature]],
          ylab = feature,
          main = paste("Boxplot of", feature),
          col = "#FF6666",
          outcol = "#FF6666")
```

The initial EDA revealed that life expectancy is positively correlated with GDP, income composition of resources, schooling, and access to improved sanitation. Life expectancy is negatively correlated with adult mortality rates, infant deaths, and under-five deaths.

```         
corr <- round(cor(subset(data, select =-c(Status, Country))), 3)
ggcorrplot(corr,type = "lower" )
corr
```

## BIC

BIC is a statistical criterion used in model selection and feature selection. It is a penalized likelihood criterion that balances the goodness of fit of the model with the number of parameters used in the model. In the context of feature selection.

In the context of feature selection, the idea is to compare different models with different subsets of features and select the model with the Lowest BIC. Lower BIC values indicate a better trade-off between model fit and complexity.

12 variables were identified as the best subset. Variables such as 'Country' and 'Year' were excluded from consideration. A scatter plot matrix was created to visualize relationships.

```         
# fwd.summary
plot(fwd.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
which.min(fwd.summary$bic)
points(12,fwd.summary$bic[12],col="red",cex=2,pch=20)
```

After BIC analysis we did some transformation on our dataset and combined all plots in single plot.

```         
grid.arrange(grobs = plots_list, ncol = 3)
```

## Linear Regression

A linear regression model was built using the selected features. Diagnostic plots, including residual plots, were examined. Tests for heteroskedasticity and model specification errors were conducted. The model showed that GDP, percentage expenditure (percexp), and thinning (thinness. 5.9 years) were the most significant predictors of life expectancy.

```         
set.seed(0)

sample <- sample(c(TRUE, FALSE), nrow(data_FS), replace=TRUE, prob=c(0.70,0.30))
train <- data_FS[sample, ]
x.test <-data_FS[!sample, ]
y.test <- data_FS[!sample, ]$lifexp
model<- lm(lifexp~., data = train)
summary(model)
```

## Heterogeneity and Functional Form Issues

The model was tested for heterogeneity and functional form issues. The Breusch-Pagan test indicated that the variance of errors was not constant across levels of the independent variables. The Ramsey RESET test suggested a specification error in the model.

A t-test was performed to compare the life expectancy of countries with low and high healthcare expenditure. The results showed that there was no significant difference in life expectancy between the two groups.

Two separate linear regression models were built to assess the impact of infant mortality rates and adult mortality rates on life expectancy.

The results showed that both infant and adult mortality rates were significant predictors of life expectancy.

The correlation between life expectancy and alcohol consumption was found to be 0.392. A linear regression model indicated that alcohol consumption was not a significant predictor of life expectancy.

A linear regression model was built to assess the impact of population density on life expectancy. The results showed that densely populated countries tend to have lower life expectancy.

## Conclusions

The main findings of this analysis are that GDP, percentage expenditure, and thinning (thinness.5.9.years) are the most significant predictors of life expectancy. Additionally, adult mortality rates, infant deaths, and under-five deaths are all negatively correlated with life expectancy. Finally, densely populated countries tend to have lower life expectancy.
