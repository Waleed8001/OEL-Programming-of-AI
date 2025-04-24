library(tidyverse)

# Import the dataset and show its structure 
data <- read_csv("C:/Users/DUET/Downloads/student_scores.csv")
view(data)

# Replace all NA values in Attendance_Percentage with the average attendance of the class.


data <- read_csv("C:/Users/DUET/Downloads/student_scores.csv")
view(data)
c<- data %>% filter(Attendance_Percentage != 'na') %>% summarise(mean(Attendance_Percentage))
view(c)

mean

s <-  
view(s)
?is.na

# Create a new column Average_Score as the mean of the three subject scores (Math, Physics, Chemistry).
new_c <- data %>% mutate(Average_Score = (Math_Score + Chemistry_Score + Physics_Score) / 3)
view(new_c)

# Filter all female students with an Average_Score greater than 75 and Attendance_Percentage over 80%.
female_data <- new_c %>% filter((Average_Score > 75) & (Attendance_Percentage > 80))
view(female_data)

# Display the average scores in each subject for each department using group-wise summarization.
ave <- new_c %>% select(Department, Average_Score) %>% group_by(Department) %>% summarise(mean(Average_Score))
view(ave)

# Sort students in descending order based on their Average_Score and display the top 5 performers.
sort <- new_c %>% arrange(Average_Score, desc=TRUE) %>% head(5)
view(sort)

#Create a new column Performance_Category with the following logic:
 # Excellent if Average_Score ≥ 85, Good if 70 ≤ Average_Score < 85 and Needs
#Improvement if Average_Score < 70

new_column <- new_c %>% mutate(Performance_Category = ifelse((Average_Score >= 85), 'Excellent', ifelse((Average_Score >= 70) & (Average_Score < 70), 'Good', ifelse((Average_Score < 70), 'Needs', 'Improvement'))))
view(new_column)

# Count the number of students in each Performance_Category   
count <- new_column %>% group_by(Performance_Category) %>% summarise(n())
view(count)

# Display only Name, Department, and Performance_Category of students who have Attendance_Percentage below 60.

show <- new_column %>% select(Name, Department, Performance_Category) %>% filter(Attendance_Percentage < 60)
view(show)
