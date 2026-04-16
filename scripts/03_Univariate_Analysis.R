"Day 3: Task 1: Univariate & Bivariate Analysis & Visualization"

library(tidyverse) #loading tidyverse package 

nhis_clean <- read.csv(choose.files("C:/Users/her_n/OneDrive/Documents/MPH Program/Spring 2026/PUBH422 Statistical Planning for Health Data/Group Project 2/Spring26_Project2_Group6/data/nhis_clean.csv"), header = T) #loading nhis_clean data into R

"Quantitative Variables"

#AgeP_A (Age)
summary(nhis_clean$AGEP_A) #summary stats for age 
sd(nhis_clean$AGEP_A) #standard deviation for age 

#Base histogram AGEP_A
hist(nhis_clean$AGEP_A, main = "Age Distribution",xlab="Age")

#ggplot histogram AGEP_A
ggplot(nhis_clean,aes(x=AGEP_A)) + #x axis will be Age 
  geom_histogram(binwidth = 5) + #data will be distributed among 5 bins
  xlab("Ages") + #x axis title 
  ggtitle("Age Distribution of Sample Adults") #chart title  

#Base boxplot of AGEP_A
boxplot(nhis_clean$AGEP_A, main = "Age Distribution of Sample Adults") 

#ggplot boxplot of AGEP_A
ggplot(nhis_clean, aes(y=AGEP_A)) + #y axis will be age values 
  geom_boxplot() + #identifying boxplot
  ggtitle("Age Distribution of Sample Adults") + #chart title
  ylab("Ages") #y axis title

 
#WEIGHTLBTC_A (Weight w/out shoes)
summary(nhis_clean$WEIGHTLBTC_A) #Summary stat for weight w/o shoes
sd(nhis_clean$WEIGHTLBTC_A) #standard dev for weight w/o shoes 

basicStats(nhis_clean$WEIGHTLBTC_A) #summary stat for weight w/o shoes 

#Base histogram WEIGHTLBTC_A
hist(nhis_clean$WEIGHTLBTC_A, main = "Weight Distribution of Sample Adults w/out Shoes",xlab="Weight")

#ggplot histogram WEIGHTLBTC_A
ggplot(nhis_clean,aes(x=WEIGHTLBTC_A)) + #x axis will be weight 
  geom_histogram(binwidth = 10) + #data will be distributed among 10 bins
  xlab("Weight") + #x axis title 
  ggtitle("Weight Distribution of Sample Adults w/out Shoes") #chart title  

#Base boxplot of WEIGHTLBTC_A
boxplot(nhis_clean$WEIGHTLBTC_A, main = "Weight Distribution of Sample Adults w/out Shoes") 

#ggplot boxplot of WEIGHTLBTC_A
ggplot(nhis_clean, aes(y=WEIGHTLBTC_A)) + #y axis will be weight values
  geom_boxplot() + #identidying boxplot
  ggtitle("Weight Distribution of Sample Adults w/out Shoes") + #chart title
  ylab("Weight") #y axis title 


#HEIGHTTTC_A (Heigh in Inches)
basicStats(nhis_clean$HEIGHTTC_A) #summary stat for total height in inches  

#Base histogram HEIGHTTTC_A
hist(nhis_clean$HEIGHTTC_A, main = "Height Distribution of Sample Adults",xlab="Total Height in Inches")

#ggplot histogram HEIGHTTTC_A
ggplot(nhis_clean,aes(x=HEIGHTTC_A)) + #x axis will be total height in inches 
  geom_histogram(binwidth = 2) + #data will be distributed among 2 bins
  xlab("Total Height in Inches") + #x axis title 
  ggtitle("Height Distribution of Sample Adults") #chart title  

#Base boxplot of HEIGHTTTC_A
boxplot(nhis_clean$HEIGHTTC_A, main="Total Height Distribution in Inches of Sample Adults") 

#ggplot boxplot of HEIGHTTTC_A
ggplot(nhis_clean, aes(y=HEIGHTTC_A)) + #y axis will be height values
  geom_boxplot() + #identidying boxplot
  ggtitle("Height Distribution of Sample Adults") + #chart title
  ylab("Total Height in Inches") #y axis title 

"Qualitative Variables"

#SEX_A (Gender)

nhis_clean$SEX_A <- factor(nhis_clean$SEX_A, #creating categorical data to a factor 
                           levels = c(1,2), #identifying the values w/in data
                           labels = c("Males","Females")) #relabeling the values w/corresponding label

sex_a_freq_table <- table(nhis_clean$SEX_A) #creating frequency table of sex_a variable 

sex_a_freq_table #printing out the sex_a frequency table 

barplot(sex_a_freq_table, #indicating r base barplot of sex_a variable through freq table 
        main = "Distibution of Sex Among Adult Sample", #chart title
        xlab = "Sex", #x axis title
        ylab = "Frequency", #y axis title
        col = c("lightblue","lightpink")) #bar plot colors 

sex_a_freq_df <- as.data.frame(sex_a_freq_table) #converting freq table to a data frame for ggplot 

ggplot(sex_a_freq_df, aes(x=Var1,y=Freq)) + #using ggplot & the data frame, indicating x as the variables and y as the frequency
  geom_bar(stat = "identity",fill ="purple") + #indicating bar plot
  labs(title = "Distribution of Sex Among Adult Sample", #chart title
       x = "Sex", #x axis title
       y = "Frequency") #y axis title

#HISPALLP_A (Race/Ethnicity)

nhis_clean$HISPALLP_A <- factor(nhis_clean$HISPALLP_A, #creating categorical data to a factor 
                           levels = c(1,2,3,4,5,6,7), #identifying the values w/in data
                           labels = c("Hispanic","NHWhite","NHAA","NHAsian Only","NHAIAN","NHAIAN&Other","Other")) #relabeling the values w/corresponding label

hispallp_a_freq_table <- table(nhis_clean$HISPALLP_A) #creating frequency table of HISPALLPa variable 

hispallp_a_freq_table #printing out the hispallp_a frequency table 

barplot(hispallp_a_freq_table, #indicating r base barplot of hispallp_a variable through freq table 
        main = "Distibution of Race/Ethnicity Among Adult Sample", #chart title
        xlab = "Race/Ethniticy", #x axis title
        ylab = "Frequency", #y axis title
        col = terrain.colors(7), #bar plot colors 
        cex.names = 0.6) #label size

hispallp_a_freq_df <- as.data.frame(hispallp_a_freq_table) #converting freq table to a data frame for ggplot 

ggplot(hispallp_a_freq_df, aes(x=Var1,y=Freq)) + #using ggplot & the data frame, indicating x as the variables and y as the frequency
  geom_bar(stat = "identity",fill ="lightgreen") + #indicating bar plot
  labs(title = "Distribution of Race/Ethnicity Among Adult Sample", #chart title
       x = "Race/Ethnicity", #x axis title
       y = "Frequency") #y axis title

#EDUCP_A (Education Level)

"EDCUP_A was already recoded and turned into a factor in previous task"

educp_a_freq_table <- table(nhis_clean$EDUCP_A) #creating frequency table of EDUCP_A variable 

educp_a_freq_table #printing out the educp_a frequency table 

barplot(educp_a_freq_table, #indicating r base barplot of educp_a variable through freq table 
        main = "Distibution of Education Level Among Adult Sample", #chart title
        xlab = "Education Level", #x axis title
        ylab = "Frequency", #y axis title
        col = rainbow(4), #bar plot colors 
        cex.names = 0.45) #label size

educp_a_freq_df <- as.data.frame(educp_a_freq_table) #converting freq table to a data frame for ggplot 

ggplot(educp_a_freq_df, aes(x=Var1,y=Freq)) + #using ggplot & the data frame, indicating x as the variables and y as the frequency
  geom_bar(stat = "identity",fill ="blue") + #indicating bar plot
  labs(title = "Distribution of Education Level Among Adult Sample", #chart title
       x = "Race/Ethnicity", #x axis title
       y = "Frequency") + #yaxis title 
  theme(axis.text.x = element_text(size = 9, angle = 45, hjust = 1)) #adjusts labels on x axis

#PHSTAT_A (General Health)

nhis_clean$PHSTAT_A<- factor(nhis_clean$PHSTAT_A, #creating categorical data to a factor 
                                levels = c(1,2,3,4,5), #identifying the values w/in data
                                labels = c("Excellent","Very Good","Good","Fair","Poor")) #relabeling the values w/corresponding label

phstat_a_freq_table <- table(nhis_clean$PHSTAT_A) #creating frequency table of PHSTAT_A variable 

phstat_a_freq_table #printing out the phstat_a frequency table 

barplot(phstat_a_freq_table, #indicating r base barplot of phstat_a variable through freq table 
        main = "Distibution of Health Status Among Adult Sample", #chart title
        xlab = "General Health Status", #x axis title
        ylab = "Frequency", #y axis title
        col = topo.colors(5), #bar plot colors 
        cex.names = 0.8) #label size

phstat_a_freq_df <- as.data.frame(phstat_a_freq_table) #converting freq table to a data frame for ggplot 

ggplot(phstat_a_freq_df, aes(x=Var1,y=Freq)) + #using ggplot & the data frame, indicating x as the variables and y as the frequency
  geom_bar(stat = "identity",fill ="gray") + #indicating bar plot
  labs(title = "Distribution of Health Status Among Adult Sample", #chart title
       x = "General Health Status", #x axis title
       y = "Frequency") #y axis title

#LSATIS4R_A (Quality of Life)

nhis_clean$LSATIS4R_A<- factor(nhis_clean$LSATIS4R_A, #creating categorical data to a factor 
                             levels = c(1,2,3,4), #identifying the values w/in data
                             labels = c("Very Satisfied","Satisfied","Dissatisfied","Very Dissatisfied")) #relabeling the values w/corresponding label

lsatis4r_a_freq_table <- table(nhis_clean$LSATIS4R_A) #creating frequency table of LSATIS4R_A variable 

lsatis4r_a_freq_table #printing out the lsatis4r_a frequency table 

barplot(lsatis4r_a_freq_table, #indicating r base barplot of lsatis4r_a variable through freq table 
        main = "Distibution of Quality of Life Among Adult Sample", #chart title
        xlab = "Quality of Life", #x axis title
        ylab = "Frequency", #y axis title
        col = heat.colors(4), #bar plot colors 
        cex.names = 0.8) #label size

lstatis4r_a_freq_df <- as.data.frame(lsatis4r_a_freq_table) #converting freq table to a data frame for ggplot 

ggplot(lstatis4r_a_freq_df, aes(x=Var1,y=Freq)) + #using ggplot & the data frame, indicating x as the variables and y as the frequency
  geom_bar(stat = "identity",fill = "orange") + #indicating bar plot
  labs(title = "Distribution of Quality of Life Among Adult Sample", #chart title
       x = "Quality of Life", #x axis title
       y = "Frequency") #y axis title





