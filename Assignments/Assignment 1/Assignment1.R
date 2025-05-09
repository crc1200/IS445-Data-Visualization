
#Libraries

library(Hmisc)
library(psych)
library(GGally)
library(ggplot2)
library(vioplot)
library(corrplot)
library(dplyr)
library(readr)
########################################################################################################################

# Read in Data set

Employee_Attrition <- read_csv("Employee Attrition Assignment 1.csv")
dim(Employee_Attrition)

View(Employee_Attrition)

# Check for Missing Values
round(colSums(is.na(Employee_Attrition)) / nrow(Employee_Attrition), 2)

sum(is.na(Employee_Attrition))


# Univariable Categorical
ggplot(Employee_Attrition, aes(x = dept)) +
  theme_classic() + 
  coord_flip() + 
  geom_bar(fill="cornflowerblue", color="black") + labs(x = "Department", y = "Frequency", title="Participants by Department")

# Univariable Quantitative
ggplot(Employee_Attrition, aes(x = time_spend_company)) +
  theme_classic() + 
  geom_histogram(fill="cornflowerblue", color="black", bins=10) + labs(x = "Years with Company", y = "Frequency", title="Participants by Years With Company") 

# Bivariable Graph -- Categorical- Categorical 
Employee_Attrition_clean <- Employee_Attrition_clean %>%
  mutate(salary = factor(salary, levels = c("high", "medium", "low")))

ggplot(Employee_Attrition_clean, aes(x = dept, fill=salary)) + geom_bar(position="fill") + coord_flip() + labs(y = "Proportion", x="Department", title="Participants by Department and Salary") + theme_classic()

# Bivariable Graph – Quantitative-Categorical
Employee_Attrition_clean <- Employee_Attrition_clean %>%
  mutate(salary = factor(salary, levels = c("low", "medium", "high")))

ggplot(Employee_Attrition_clean, aes(x = salary, y=average_montly_hours)) + geom_boxplot() + labs(title="Hours Worked by Salary", x="Salary Level", y="Average Monthly Hours") + theme_minimal()

