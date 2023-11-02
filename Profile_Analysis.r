#Loading the CSV file
profile_data = read.csv("ias-profile.csv")
head(profile_data)
View(profile_data)

#Dropping cloumns
profile_data <- subset(profile_data, select = -c(ID,Name,Source_of_Recruitment,Retired,Last_Education_Subject,Last_Education_Division))
profile_data <- subset(profile_data, select = -c(Last_Designation,Last_Level,Last_Office,Last_Field_of_Experience,Last_Category_of_Experience,Source,Gender_Source))
View(profile_data)

#find median dates for all columns and replace NA with median dates
median_allotment_year <- median(profile_data$Allotment_Year, na.rm = TRUE)
profile_data$Allotment_Year[is.na(profile_data$Allotment_Year)] <- median_allotment_year

#First Copy of the data
data1 <- subset(profile_data)
data1 <- na.omit(profile_data)
head(data1)

#Figuring out the genders of the bureaucrats
gender_counts <- table(data1$Gender)
print(gender_counts)
barplot(gender_counts, 
        main = "Gender Distribution",
        xlab = "Gender",
        ylab = "Count",
        col = c("blue", "pink"), # Specify colors for bars
        legend.text = rownames(gender_counts)) # Add legend
# Replace NA or empty "Languages Known" with "Mother Tongue" if "Mother Tongue" exists
profile_data$Languages_Known <- ifelse(is.na(profile_data$Languages_Known) | profile_data$Languages_Known == "",
                                       ifelse(!is.na(profile_data$Mother_Tongue) & profile_data$Mother_Tongue != "" & profile_data$Mother_Tongue != "N.A." & profile_data$Mother_Tongue != "-", 
                                              profile_data$Mother_Tongue, 
                                              profile_data$Languages_Known),
                                       profile_data$Languages_Known)
# Filter out the empty values and calculate the mode
mode_languages_known <- as.character(names(sort(table(profile_data$Languages_Known[profile_data$Languages_Known != "" ]), decreasing = TRUE)[1]))

# Replace empty values with the calculated mode value
profile_data$Languages_Known[profile_data$Languages_Known == ""] <- mode_languages_known
profile_data$Languages_Known[profile_data$Languages_Known == "N.A."] <- mode_languages_known

#Figuring out the count of people coming from the different domicile 
data1 <- data1[data1$Place_of_Domicile != "-", ]
state_counts <- table(data1$Place_of_Domicile)
print(state_counts)
barplot(state_counts,
        main = "Count of People by State",
        xlab = "State",
        ylab = "Count",
        col = "blue")
text(1:length(state_counts), state_counts, labels = state_counts, pos = 3)

#Figuring out how many belong the different cadre 
cadre_counts <- table(data1$Cadre)
print(cadre_counts)
barplot(cadre_counts,
        main = "Count of People by Cadre",
        xlab = "State",
        ylab = "Count",
        col = "red")
text(1:length(cadre_counts), cadre_counts, labels = cadre_counts, pos = 3)

#Figuring out the retirement reasons
retirement_counts <- table(data1$Retirement_Reason)
print(retirement_counts)
barplot(retirement_counts,
        main = "Retirement Reasons",
        xlab = "Reason",
        ylab = "Count",
        col = "purple")
text(1:length(retirement_counts), retirement_counts, labels = retirement_counts, pos = 3)


#Figuring out the Educational Qualification of the bureaucrats
educational_counts <- table(data1$Last_Education_Qualification)
print(educational_counts)
barplot(educational_counts,
        main = "Count of People based on the Educational Qualification",
        xlab = "Reason",
        ylab = "Count",
        col = "#009999")
text(1:length(educational_counts), educational_counts, labels = educational_counts, pos = 3)


# Count the number of people who belong to a state but are not working in that state
not_working_in_state <- subset(data1, data1$Place_of_Domicile != data1$Cadre)
state2_counts <- table(not_working_in_state$Place_of_Domicile)

print(state2_counts)
barplot(state2_counts,
        main = "People Who Belong to a State But Do Not Work in That State",
        xlab = "State",
        ylab = "Count",
        col = "pink")
text(1:length(state2_counts), state2_counts, labels = state2_counts, pos = 3)



# Count the number of people who belong to a state and are working in that state only
working_in_state <- subset(data1, data1$Place_of_Domicile == data1$Cadre)
state3_counts <- table(working_in_state$Place_of_Domicile)

print(state3_counts)
barplot(state3_counts,
        main = "People Who Belong to a State and Work in That State",
        xlab = "State",
        ylab = "Count",
        col = "yellow")
text(1:length(state3_counts), state3_counts, labels = state3_counts, pos = 3)








# Create a table of female counts by state
female_counts <- table(data1$Place_of_Domicile[data1$Gender == "Female"])
print(female_counts)

# Create a bar plot
barplot(female_counts,
        main = "Count of Females by State",
        xlab = "State",
        ylab = "Count",
        col = "chartreuse")
text(1:length(female_counts), female_counts, labels = female_counts, pos = 3)




#Count the number of women in each cadre
female_counts <- table(data1$Cadre[data1$Gender == "Female"])
print(female_counts)

# Create a bar plot
barplot(female_counts,
        main = "Count of Females by Cadre",
        xlab = "State",
        ylab = "Count",
        col = "#EB8410")
text(1:length(female_counts), female_counts, labels = female_counts, pos = 3)




# Calculate the ratio of female employees in each cadre
female_ratio <- with(data1, table(Cadre, Gender))[, "Female"] / table(data1$Cadre)
cadre_female_ratio <- data.frame(Cadre = names(female_ratio), Female_Ratio = female_ratio)
#pdf("cadre_pie_charts.pdf")


for (i in 1:nrow(cadre_female_ratio)) {
  cadre_name <- cadre_female_ratio$Cadre[i]
  cadre_data <- subset(data1, Cadre == cadre_name)
  if (nrow(cadre_data) > 0) {
    labels <- unique(cadre_data$Gender)
    counts <- table(cadre_data$Gender)
    colors <- rainbow(length(labels))
    pie(counts, labels = paste(labels, "(", round(counts / sum(counts) * 100, 1), "%)"), col = colors, main = paste("Ratio of Female Employees in", cadre_name))
    #if (i < nrow(cadre_female_ratio)) {
    #dev.new()
    #}
  }
}
#dev.off()
print(cadre_female_ratio)






#Second Copy of the data
profile_data <- subset(profile_data)
head(profile_data)


#Convert into Date data
profile_data$Date_of_Birth <- as.Date(profile_data$Date_of_Birth)
profile_data$Date_of_Joining <- as.Date(profile_data$Date_of_Joining)



# Find median dates for each Date column
date_columns <- colnames(profile_data)[sapply(profile_data, function(col) class(col) == "Date")]
median_dates <- sapply(profile_data[date_columns], function(x) median(x, na.rm = TRUE))

median_results <- data.frame(Column = date_columns, Median = median_dates)
median_results$Median <- as.Date(median_results$Median)
print(median_results)


# Replace NA values in date columns with median dates
for (col in date_columns) {
  profile_data[[col]][is.na(profile_data[[col]])] <- median_dates[col]
}



# Print the updated data frame
head(profile_data)
View(profile_data)

# Calculate age at joining
profile_data$Age_at_Joining <- as.numeric(difftime(profile_data$Date_of_Joining, profile_data$Date_of_Birth, units = "days")) / 365.25

# Calculate the average age at joining
average_age <- mean(profile_data$Age_at_Joining, na.rm = TRUE)

# Print the average age
cat("Average Age at Joining: ", round(average_age, 2), " years\n")
#profile_data$Age_at_Joining <- as.integer(profile_data$Age_at_Joining)

# Create a histogram of age groups at joining
hist(profile_data$Age_at_Joining, 
     main = "Age at Joining Histogram", 
     xlab = "Age at Joining", 
     ylab = "Frequency", 
     breaks = 10, 
     col = "lightblue")

head(profile_data$Age_at_Joining)



# Remove rows with Mother Tongue as '-', NA, or blank
profile_data <- profile_data[!(profile_data$Mother_Tongue == '-' | 
                                 is.na(profile_data$Mother_Tongue) |
                                 profile_data$Mother_Tongue == "N.A."
                               | profile_data$Mother_Tongue == ""), ]

# Optionally, reset the row names
rownames(profile_data) <- NULL

# Print the updated dataset
head(profile_data)
View(profile_data)


# Create a table of "Mother Tongue" counts
mother_tongue_counts <- table(profile_data$Mother_Tongue)
print(mother_tongue_counts)

# Convert counts to a data frame
mother_tongue_counts_df <- as.data.frame(mother_tongue_counts)

# Rename columns for clarity
colnames(mother_tongue_counts_df) <- c("Mother_Tongue", "Count")

# Create a bar plot
barplot(mother_tongue_counts_df$Count, names.arg = mother_tongue_counts_df$Mother_Tongue,
        main = "Mother Tongue Distribution",
        xlab = "Mother Tongue",
        ylab = "Count",
        col = "red")

text(x = barplot(mother_tongue_counts_df$Count, 
                 names.arg = mother_tongue_counts_df$Mother_Tongue,
                 col = "red") + 0.25,  # Adjust 0.25 to set the position of labels
     y = mother_tongue_counts_df$Count, 
     labels = mother_tongue_counts_df$Count,
     pos = 3, 
     cex = 1, 
     col = "black")


#Working on the Languages Known Column
profile_data$Languages_Known <- ifelse(profile_data$Languages_Known == "", 
                                       profile_data$Mother_Tongue, 
                                       profile_data$Languages_Known)
View(profile_data)







# Load necessary libraries
library(dplyr)
library(ggplot2)

# Split the space-separated values and count the number of languages known
profile_data <- profile_data %>%
  mutate(Languages_Count = sapply(strsplit(Languages_Known, " "), length))

# Count how many candidates know each count of languages
language_count_summary <- profile_data %>%
  group_by(Languages_Count) %>%
  summarize(Count = n())

print(language_count_summary)
# Create a bar plot to visualize the distribution
ggplot(language_count_summary, aes(x = factor(Languages_Count), y = Count)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Number of Candidates Knowing Different Numbers of Languages",
       x = "Number of Languages Known",
       y = "Number of Candidates") +
  theme_minimal()




#Performing chi-square test on Gender and Age_of_Joining
# Define age categories
age_bins <- cut(profile_data$Age_at_Joining, breaks = c(20, 25, 30, 35), labels = c("<=25", "26-30", ">30"), right = FALSE)

# Create a contingency table
contingency_table <- table(profile_data$Gender, age_bins)

# Perform the chi-square test
chi_square_test <- chisq.test(contingency_table)

# Print the chi-square test results
print(chi_square_test)



#Performing a chi-square test on CADRE and PLACE_OF_DOMICILE
contingency_table <- table(profile_data$Cadre, profile_data$Place_of_Domicile)

# Perform the chi-square test
chi_square_test <- chisq.test(contingency_table)

# Print the chi-square test results
print(chi_square_test)




#Figuring out the realtionship between gender and last designation held
profile_data2 = read.csv("ias-profile.csv")
head(profile_data2)
#Dropping cloumns
profile_data2 <- subset(profile_data2, select = -c(ID,Name,Source_of_Recruitment,Retired,Last_Education_Subject,Last_Education_Division))
profile_data2 <- subset(profile_data2, select = -c(Last_Office,Last_Field_of_Experience,Last_Category_of_Experience,Source,Gender_Source))
View(profile_data2)

#Third Copy of the data
data3 <- subset(profile_data2)
data3 <- na.omit(profile_data2)
head(data3)
View(data3)


#Logistic Regression
# Load necessary libraries
library(nnet)

# Perform multinomial logistic regression
multinom_model <- multinom(Last_Level ~ Gender, data = data3)

# Summarize the multinomial logistic regression model
summary(multinom_model)



# Loading the CSV file
my_data2 = read.csv("ias-profile.csv")
head(my_data2)

# Dropping columns
my_data2 <- subset(my_data2, select = -c(ID, Name, Source_of_Recruitment, Retired, Last_Education_Subject, Last_Education_Division))
my_data2 <- subset(my_data2, select = -c(Last_Office, Last_Field_of_Experience, Last_Category_of_Experience, Source, Gender_Source))
View(my_data2)

# Third Copy of the data
data3 <- subset(my_data2, select = -c(Languages_Known, Date_of_Birth, Date_of_Joining, Retirement_Reason, Last_Education_Qualification, Last_Designation, Last_Level, Last_Start_Date, Last_End_Date))
data3 <- na.omit(data3)
data3 <- data3[!(apply(data3, 1, function(row) any(row == '-' | row == 'N.A.'))), ]
head(data3)
View(data3)

# Load the necessary libraries
library(rpart)
library(rpart.plot)


# Sample 50 rows randomly from your dataset
set.seed(123)  # For reproducibility
sampled_data <- data3[sample(nrow(data3), 50), ]

# Fit a decision tree model on the sampled data
model <- rpart(Cadre ~ ., data = data3, method ="class")

# Print a summary of the model
summary(model)

# Plot the decision tree
rpart.plot(model, extra = 1)  # Use 'extra = 1' to display summary information on the nodes

# Print a summary of the model
summary(model)

# Make predictions on new data
new_data_point <- data.frame(
  Place_of_Domicile = "Gujarat",
  Allotment_Year = 1953,
  Gender="Male",
  Mother_Tongue = "Gujarati"
)

# Predict the Cadre using type = "class"
predicted_cadre <- predict(model, new_data_point, type = "class")
predicted_cadre
