
# Libraries
library(Hmisc)
library(psych)
library(GGally)
library(ggplot2)
library(vioplot)
library(corrplot)
library(dplyr)
library(readr)
library(superheat)
library(GGally)
library(stringr)
library(scales)
library(ggradar)
library(waterfalls)
library(reshape2)
library(randomForest)
library(tibble)       
library(tidyr)
library(dplyr)
library(ggplot2)
library(knitr)
library(tidyverse)
library(gt)

########################################################################################################################

# Read in Data Set

loan_data <- read_csv("LoanPrediction.csv")

########################################################################################################################

# Random Forest 

set.seed(100)

loan_data_clean <- loan_data %>%
  mutate(Loan_Status_Numeric = ifelse(Loan_Status == "Y", 1, 0)) %>%
  select(-Loan_Status, -Loan_ID,)  # Remove Loan_Status as it's now numeric

loan_data_clean <- na.omit(loan_data_clean)

loan_data_clean$Loan_Status_Numeric <- as.factor(loan_data_clean$Loan_Status_Numeric)

rf_model <- randomForest(Loan_Status_Numeric ~ ., data = loan_data_clean, importance = TRUE)

importance_data <- as.data.frame(importance(rf_model)) %>%
  rownames_to_column(var = "Variable") %>%
  arrange(desc(MeanDecreaseAccuracy))

importance_data$Variable <- recode(importance_data$Variable,
                                   "Property_Area" = "Property Area",
                                   "Married" = "Marriage Status",
                                   "Education" = "Education Level",
                                   "Dependents" = "Number of Dependents",
                                   "Self_Employed" = "Self Employed (Y/N)",
                                   "Loan_Amount_Term" = "Loan Duration",
                                   "LoanAmount" = "Loan Amount",
                                   "CoapplicantIncome" = "Co-applicant Income",
                                   "ApplicantIncome" = "Applicant Income",
                                   "Credit_History" = "Credit History")

\ggplot(importance_data, aes(x = reorder(Variable, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(
    label = round(MeanDecreaseAccuracy, 2),
    hjust = ifelse(MeanDecreaseAccuracy < 0, 1.1, -0.04) # Adjust hjust based on value sign
  ),
  size = 5.5,      
  color = "black") +  
  coord_flip() +
  labs(
    title = "Feature Importance for Loan Prediction for Random Forest Model",
    subtitle = "Considering Credit History",
    x = "Applicant Descriptor",
    y = "Mean Decrease in Accuracy (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 26, face = "bold", color = "black"), 
    plot.subtitle = element_text(size = 20, color = "black"), 
    axis.title.x = element_text(size = 24, face = "bold", color = "black"), 
    axis.title.y = element_text(size = 24, face = "bold", color = "black"), 
    axis.text.x = element_text(size = 18, color = "black"), 
    axis.text.y = element_text(size = 18, color = "black")  
  )

########################################################################################################################

# Bubble Chart

loan_data_clean <- loan_data %>% mutate(Loan_Status_Numeric = ifelse(Loan_Status == "Y", 1, 0)) %>%
  mutate(
    log_applicant_income = log(ApplicantIncome),
    log_loan_amount = log(LoanAmount),
    total_income = ApplicantIncome + CoapplicantIncome,
    log_total_income = log(total_income)
  ) %>%
  na.omit() %>%
  filter(Credit_History == 1)

loan_data_clean$Loan_Status_Numeric <- as.factor(loan_data_clean$Loan_Status_Numeric)

ggplot(loan_data_clean, aes(x = log_total_income, y = log_loan_amount, shape = Loan_Status_Numeric, color = Loan_Status_Numeric)) +
  geom_point(size = 3.5, alpha = 0.5) +
  geom_smooth(method = "lm", aes(group = 1), color = "steelblue", se = FALSE, linetype = "solid", size = 1) + 
  scale_shape_manual(values = c(16, 17), labels = c("Rejected", "Approved")) +
  scale_color_manual(values = c("red", "green"), labels = c("Rejected", "Approved")) +
  labs(
    title = "Loan Amount vs. Total Applicant Income by Loan Status",
    subtitle = "For Applicants with Credit History",
    x = "Total Applicant Income (log)",
    y = "Loan Amount (log)",
    shape = "Loan Status",
    color = "Loan Status"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 22),         
    legend.title = element_text(size = 24),       
    legend.spacing.y = unit(0.3, "cm"),           
    legend.key.size = unit(1.25, "cm"),            
    plot.title = element_text(size = 26, face = "bold"), 
    plot.subtitle = element_text(size = 24),      
    axis.title.x = element_text(size = 22, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 24, face = "bold", margin = margin(r = 10)), 
    axis.text.x = element_text(size = 20),        
    axis.text.y = element_text(size = 20)         
  )

########################################################################################################################

# Bar Chart

gender_table <- loan_data_clean %>%
  filter(!is.na(Gender)) %>%  
  group_by(Gender) %>%
  summarise(
    Approved = sum(Loan_Status_Numeric == 1, na.rm = TRUE),
    Total = n(),
    Rejected = Total - Approved
  ) %>%
  rename(Group = Gender)

employed_table <- loan_data_clean %>%
  filter(!is.na(Self_Employed)) %>%  
  group_by(Self_Employed) %>%
  summarise(
    Total = n(),
    Approved = sum(Loan_Status_Numeric == 1, na.rm = TRUE),
    Rejected = Total - Approved
  ) %>%
  mutate(Group = ifelse(Self_Employed == "Yes", "Self-Employed", "Not Self-Employed")) %>%
  select(-Self_Employed)

combined_data <- bind_rows(gender_table, employed_table) %>%
  mutate(
    Approval_Rate = round((Approved / Total) * 100, 2),
    Rejection_Rate = round((Rejected / Total) * 100, 2)
  )

combined_data_long <- combined_data %>%
  pivot_longer(
    cols = c(Approved, Rejected),
    names_to = "Status",
    values_to = "Count"
  ) %>%
  mutate(
    Percentage_Label = case_when(
      Status == "Rejected" ~ paste0(Rejection_Rate, "%"),
      Status == "Approved" ~ paste0(Approval_Rate, "%")
    )
  )

combined_data_long <- combined_data_long %>%
  mutate(Status = factor(Status, levels = c("Rejected", "Approved")))

ggplot(combined_data_long, aes(x = Group, y = Count, fill = Status)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(
    aes(label = Percentage_Label, group = Status),
    position = position_fill(vjust = 0.5),
    color = "white",
    size = 5
  ) +
  scale_fill_manual(values = c("#D32F2F", "#2FD355"), labels = c("Rejected", "Approved")) +
  labs(
    title = "Approval vs Rejection Rates by Gender and Employment Status",
    x = "Candidate Description",
    y = "Approval vs Rejection Percentage",
    fill = "Loan Status"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right", 
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold"), 
    axis.title.x = element_text(size = 16, margin = ggplot2::margin(t = 15)),
    axis.title.y = element_text(size = 16, margin = ggplot2::margin(r = 15)), 
    axis.text.x = element_text(size = 14, margin = ggplot2::margin(t = 5)), 
    axis.text.y = element_text(size = 14, margin = ggplot2::margin(r = 5)) 
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 100))

########################################################################################################################

# Table

combined_data_table <- combined_data %>% select(Group, Approval_Rate, Rejection_Rate)

styled_table <- combined_data_table %>%
  gt() %>%
  tab_header(
    title = md("**Approval and Rejection Rates by Gender and Employment Status**")  # Make the title bold
  ) %>%
  fmt_number(
    columns = c("Approval_Rate", "Rejection_Rate"),
    decimals = 2,
    suffix = "%"
  ) %>%
  tab_options(
    table.font.size = 12,
    heading.align = "left"  
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold", size = 30, color = "black", align = "left")  # Bigger, black, bold, left-aligned
    ),
    locations = cells_title()
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#3569aa"),  # Keep header fill color
      cell_text(weight = "bold", size = 18, color = "black", align = "left")  # Left-align header text
    ),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#d7e0f2"),
                 cell_text(align = "left", color = "black")  # Left-align body text
    ),
    locations = cells_body(rows = seq(1, nrow(combined_data_table), 2))
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#bfcde3"),
                 cell_text(align = "left", color = "black")  # Left-align body text
    ),
    locations = cells_body(rows = seq(2, nrow(combined_data_table), 2))
  ) %>%
  tab_style(
    style = cell_borders(sides = c("top", "bottom"), color = "#bfcde3"),
    locations = cells_body()
  ) %>%
  cols_label(
    Approval_Rate = "Approval Rate (%)", 
    Rejection_Rate = "Rejection Rate (%)"
  )

styled_table

########################################################################################################################


