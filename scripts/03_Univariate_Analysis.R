"Day 3: Task 1: Univariate & Bivariate Analysis & Visualization"

library(tidyverse) #loading tidyverse package 
library(fBasics)

nhis_clean <- read_csv("data/nhis_clean.csv") #loading nhis_clean data into R

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

"Interpretation: The age distribution spans from approximately 18 to 80 years which demonstrates a wide range of ages in the sample. The graph shows a higher concentration of individuals in the middle-to-older age groups, mainly between 50-70 years with a peak around early 60s. Overall, the distribution demonstrates considerable variability with a small tendency toward older ages."

#Base boxplot of AGEP_A
boxplot(nhis_clean$AGEP_A, main = "Age Distribution of Sample Adults", ylab = "Age") 

#ggplot boxplot of AGEP_A
ggplot(nhis_clean, aes(y=AGEP_A)) + #y axis will be age values 
  geom_boxplot() + #identifying boxplot
  ggtitle("Age Distribution of Sample Adults") + #chart title
  ylab("Ages") #y axis title

"Interpretation: The boxplots demonstrate that the median age of participants is around mid-50s, resulting in a relatively older sample. The overall age range spans from slightly below 20 to around 80 years of age. The distribution appears slightly left-skewed, with a somewhat greater spread among older ages and no significant outliers are observed."
 
#WEIGHTLBTC_A (Weight w/out shoes)
summary(nhis_clean$WEIGHTLBTC_A) #Summary stat for weight w/o shoes
sd(nhis_clean$WEIGHTLBTC_A) #standard dev for weight w/o shoes 

basicStats(nhis_clean$WEIGHTLBTC_A) #summary stat for weight w/o shoes 

#Base histogram WEIGHTLBTC_A
hist(nhis_clean$WEIGHTLBTC_A, main = "Weight Distribution of Sample Adults w/out Shoes",xlab="Weight(lbs)")

#ggplot histogram WEIGHTLBTC_A
ggplot(nhis_clean,aes(x=WEIGHTLBTC_A)) + #x axis will be weight 
  geom_histogram(binwidth = 10) + #data will be distributed among 10 bins
  xlab("Weight(lbs)") + #x axis title 
  ggtitle("Weight Distribution of Sample Adults w/out Shoes") #chart title  

"Interpretation: The weight distribution ranges from approximately 100 to around 300 pounds, indicating a wide range of weights in the sample. The graph shows a positive skew, with a higher concentration of individuals weighing 130 to 200 pounds, mainly 140 to 190, peaking at 170. Overall, the distribution shows considerable variability with slight outliers."

#Base boxplot of WEIGHTLBTC_A
boxplot(nhis_clean$WEIGHTLBTC_A, main = "Weight Distribution of Sample Adults w/out Shoes", ylab = "Weight(lbs)") 

#ggplot boxplot of WEIGHTLBTC_A
ggplot(nhis_clean, aes(y=WEIGHTLBTC_A)) + #y axis will be weight values
  geom_boxplot() + #identidying boxplot
  ggtitle("Weight Distribution of Sample Adults w/out Shoes") + #chart title
  ylab("Weight(lbs)") #y axis title 

"Interpretation: The boxplots show that the median weight of participants is about 180 pounds, indicating this is the typical weight in this sample. Overall, weights range from 100 to about 300 pounds, but most adults cluster between 150 and 200 pounds. A few heavier individuals pull the distribution slightly to the right."

#HEIGHTTTC_A (Heigh in Inches)
basicStats(nhis_clean$HEIGHTTC_A) #summary stat for total height in inches  

#Base histogram HEIGHTTTC_A
hist(nhis_clean$HEIGHTTC_A, main = "Height Distribution of Sample Adults",xlab="Total Height in Inches")

#ggplot histogram HEIGHTTTC_A
ggplot(nhis_clean,aes(x=HEIGHTTC_A)) + #x axis will be total height in inches 
  geom_histogram(binwidth = 2) + #data will be distributed among 2 bins
  xlab("Total Height in Inches") + #x axis title 
  ggtitle("Height Distribution of Sample Adults") #chart title  

"Interpretation: The height distribution ranges from approximately 60 to about 75 inches, indicating substantial variability in the sample. The graph shows a higher concentration of individuals measuring 62 to 70 inches, with a peak at 63 inches. Overall, the distribution is slightly skewed to the right, but it appears fairly balanced."

#Base boxplot of HEIGHTTTC_A
boxplot(nhis_clean$HEIGHTTC_A, main="Total Height Distribution in Inches of Sample Adults", ylab = "Height(in.)") 

#ggplot boxplot of HEIGHTTTC_A
ggplot(nhis_clean, aes(y=HEIGHTTC_A)) + #y axis will be height values
  geom_boxplot() + #identidying boxplot
  ggtitle("Height Distribution of Sample Adults") + #chart title
  ylab("Total Height in Inches") #y axis title 

"Interpretation: Overall, heights range from about 60 to about 75 inches, but most adults cluster between 65 and 70 inches. The boxplots show that the median height is about 66 inches, indicating this is the typical height in this sample. The median is slightly closer to the lower quartile, and the upper whisker is a bit longer, suggesting a mild right skew; however, the distribution looks fairly balanced overall."

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

"Interpretation: The sample shows that there are more female participants than male participants, with 14,070 and 11,967, respectively." 

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

"Interpretation: The sample consists mostly of Non-Hispanic White individuals, representing 17,617 of the sample. Hispanic participants were the second most represented in the sample, comprising 3,533 participants. Non-Hispanic African Americans and Non-Hispanic Asians had similar representation, with 2645 and 1564, respectively. Lastly, Non-Hispanic American Indian and Alaska Native, Non-Hispanic American Indian and Alaska Native and Other, and Other were the least represented in the sample."

#EDUCP_A (Education Level)

"EDCUP_A was already recoded and turned into a factor in previous task"

educp_a_freq_table <- table(nhis_clean$EDUCP_A) #creating frequency table of EDUCP_A variable 

educp_a_freq_table #printing out the educp_a frequency table 

barplot(educp_a_freq_table, #indicating r base barplot of educp_a variable through freq table 
        main = "Distibution of Education Level Among Adult Sample", #chart title
        xlab = "Education Level", #x axis title
        ylab = "Frequency", #y axis title
        col = rainbow(4), #bar plot colors 
        cex.names = 0.6) #label size

educp_a_freq_df <- as.data.frame(educp_a_freq_table) #converting freq table to a data frame for ggplot 

ggplot(educp_a_freq_df, aes(x=Var1,y=Freq)) + #using ggplot & the data frame, indicating x as the variables and y as the frequency
  geom_bar(stat = "identity",fill ="blue") + #indicating bar plot
  labs(title = "Distribution of Education Level Among Adult Sample", #chart title
       x = "Race/Ethnicity", #x axis title
       y = "Frequency") + #yaxis title 
  theme(axis.text.x = element_text(size = 9, angle = 45, hjust = 1)) #adjusts labels on x axis

"Interpretation: The majority of the participants are educated because most of the sample are college graduates or better, and the second most represented group obtained some college education. High school graduates were the third most represented group, and the least represented group had less than a high school education."

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

"Interpretation: The majority of the participants report having a very good health status, with most rating their health as “very good” to “good.” The third most represented group reported an “excellent” health status, and “fair” and “poor” were reported to be fourth and fifth, respectively."

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

"Interpretation: The sample reports a high quality of life, with the majority of participants rating their lives as “very satisfied” or “satisfied.” Very few participants reported their lives as “dissatisfied” or “very dissatisfied."



