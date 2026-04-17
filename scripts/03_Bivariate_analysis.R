#Boxplots Age v Sex & Age v Education

"Age v Sex Boxplot"
boxplot(AGEP_A~SEX_A, data = nhis_clean,#boxplot
        main = "Boxplot of Age by Sex ", #chart title
        xlab = "Sex", #x axis title
        ylab = "Age") #y axis title

"Age v Education Boxplot"
boxplot(AGEP_A~EDUCP_A, data = nhis_clean,#boxplot
        main = "Boxplot of Age by Education ", #chart title
        xlab = "Education Level", #x axis title
        ylab = "Age") #y axis title

"Age v Sex GGPlot"
ggplot(nhis_clean,aes(x=SEX_A,y=AGEP_A)) + #indicating gglot, data set, x & y axis
  geom_boxplot(aes(fill = factor(SEX_A))) + #identifying boxplot chart & filling box with Sex values
  labs(title = "Boxplot of Age by Sex", #chart title
       x = "Sex", #x axis title
       y = "Age", #y axis title
       fill = "Sex") #title of legend 

"Age v Education ggplot"
ggplot(nhis_clean,aes(x=EDUCP_A,y=AGEP_A)) + #indicating gglot, data set, x & y axis
  geom_boxplot(aes(fill = factor(EDUCP_A))) + #identifying boxplot chart & filling box with Educ values
  labs(title = "Boxplot of Age by Education Level", #chart title
       x = "Education Level", #x axis title
       y = "Age", #y axis title
       fill = "Education Level") #title of legend 

#Clustered Bar Chart (General Health & Life Statisfaction)

ggplot(nhis_clean,aes(x=PHSTAT_A,fill = LSATIS4R_A)) + #identifying ggplot, x axis and y axis 
  geom_bar(position = "dodge") + #indicating bar chart 
  labs(title = "Relationship between General Health & Life Satisfaction", #chart title
       x = "General Health Status", #x axis title
       y = "Count", #y axis title
       fill = "Life Satisfaction") #legend title 

#Scatterplot of Height vs Weight 

plot(nhis_clean$HEIGHTTC_A,nhis_clean$WEIGHTLBTC_A, #scatterplot usingt plot() for height vs weight 
     main = "Height(in.) vs Weight (lbs.) Scatterplot", #chart title
     xlab = "Height (in.)", #x axis title
     ylab = "Weight(lbs.)") #y axis title

ggplot(nhis_clean,aes(x=HEIGHTTC_A,y=WEIGHTLBTC_A)) + #using ggplot for scatterplot, indicating x and y values 
  geom_point() + #indicating points for each observation 
  labs(title = "Height(in.) vs Weight (lbs.) Scatterplot", #chart title
       x = "Height (in.)", #x axis title
       y = "Weight(lbs.)") #y axis title

cor(nhis_clean$HEIGHTTC_A,nhis_clean$WEIGHTLBTC_A) #calculating & reporting correlation coefficient; results 0.5158, medium correlation 
      
