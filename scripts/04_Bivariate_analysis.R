#Boxplots Age v Sex 

"Age v Sex Boxplot"
boxplot(AGEP_A~SEX_A, data = nhis_clean,#boxplot
        main = "Boxplot of Age by Sex ", #chart title
        xlab = "Sex", #x axis title
        ylab = "Age") #y axis title

"Age v Sex GGPlot"
ggplot(nhis_clean,aes(x=SEX_A,y=AGEP_A)) + #indicating gglot, data set, x & y axis
  geom_boxplot(aes(fill = factor(SEX_A))) + #identifying boxplot chart & filling box with Sex values
  labs(title = "Boxplot of Age by Sex", #chart title
       x = "Sex", #x axis title
       y = "Age", #y axis title
       fill = "Sex") #title of legend 

"Interpretation: Overall, both groups cover nearly the same age range, from slightly younger than 20 to the mid-80s. Additionally, both groups have very similar median ages, with females slightly higher; therefore, ~50 years of age is the typical age across the sexes. The distributions appear symmetrical and comparable. There is no strong skew, as there are no outliers or noticeable differences in variability between the two groups."

#Boxplot Age v Education

"Age v Education Boxplot"
boxplot(AGEP_A~EDUCP_A, data = nhis_clean,#boxplot
        main = "Boxplot of Age by Education ", #chart title
        xlab = "Education Level", #x axis title
        ylab = "Age", #y axis title
        cex.axis = 0.75) #size of x axis text

"Age v Education ggplot"
ggplot(nhis_clean,aes(x=EDUCP_A,y=AGEP_A)) + #indicating gglot, data set, x & y axis
  geom_boxplot(aes(fill = factor(EDUCP_A))) + #identifying boxplot chart & filling box with Educ values
  labs(title = "Boxplot of Age by Education Level", #chart title
       x = "Education Level", #x axis title
       y = "Age", #y axis title
       fill = "Education Level") + #title of legend
  theme(axis.text.x = element_text(size = 9, angle = 45, hjust = 1)) #adjusts labels on x axis 

"Interpretation: The broad ranges across all boxes indicate substantial age diversity across all education levels. Less than High School attainment has the highest ages and the greatest variability compared to the other education levels, as indicated by the Q1, mean, Q3, and max. Overall, age appears inversely related to education level, as individuals with lower education levels tend to be older, potentially because younger generations are achieving higher education levels more often. This sample demonstrates a generational shift in educational attainment, with younger people more likely to have higher levels of education."

#Clustered Bar Chart (General Health & Life Statisfaction)

ggplot(nhis_clean,aes(x=PHSTAT_A,fill = LSATIS4R_A)) + #identifying ggplot, x axis and y axis 
  geom_bar(position = "dodge") + #indicating bar chart 
  labs(title = "Relationship between General Health & Life Satisfaction", #chart title
       x = "General Health Status", #x axis title
       y = "Count", #y axis title
       fill = "Life Satisfaction") #legend title 

"Interpretation: As general health improves, life satisfaction tends to increase as well. People who reported “excellent” or “very good health” are much more likely to report being “very satisfied” with their lives. Participants who reported “fair” or “poor” health showed a sharp decline in the share, reporting “very satisfied,” suggesting that poorer health is strongly associated with lower life satisfaction. Interestingly, people with “good” health have the highest count of “satisfied” individuals, possibly reflecting that moderate health still allows a good degree of life satisfaction, though less enthusiasm than with “very good” or “excellent” health. Among those with “excellent” or “very good” health status, only a very small number reported being “dissatisfied” or “very dissatisfied”; this reinforces the pattern that good health contributes to positive life evaluations. Overall, the graph shows a clear, strong relationship between health and happiness because people who perceive their health as better tend to report higher life satisfaction. Conversely, as health status declines, life satisfaction drops correspondingly."

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

cor(nhis_clean$HEIGHTTC_A,nhis_clean$WEIGHTLBTC_A) #calculating & reporting correlation coefficient; 0.5023 results, medium correlation 

"Interpretation: The scatterplot shows heights ranging roughly from 58 to 76 inches and weights from 100 to 300 pounds. There is a positive association between height and weight, meaning that, on average, weight increases with height. This is supported by a correlation coefficient of 0.5023, indicating a moderate positive linear relationship, given the noticeable spread of weights at each height. This wide range suggests that height alone is not a precise predictor of weight and that other factors likely play a significant role in determining weight. The vertical stripes indicate that the height data is discrete rather than continuous. Furthermore, a few observations fall at the extreme high end of weight across multiple heights; although they do not disrupt the overall trend, they may warrant further investigation. Overall, while taller individuals generally weigh more, height alone is insufficient to accurately predict weight due to high variability."
