

# Load the MERGED2022_23_PP dataset
data <- read.csv("MERGED2022_23_PP.csv")

# Create data frame
table1 <- data.frame(data$UNITID, data$LOCALE, data$CCSIZSET, data$UGDS,
                     data$GRADS, data$SAT_AVG_ALL, data$COSTT4_A, data$AVGFACSAL,
                     data$IRPS_MEN, data$C100_4, data$RET_FT4, data$PCTFLOAN ,
                     data$DEBT_MDN, data$MEDIAN_HH_INC, stringsAsFactors = TRUE)

# Eliminating completely N/A columns
table2 <- data.frame(data$UNITID, data$LOCALE, data$CCSIZSET, data$UGDS, data$GRADS,
                     data$SAT_AVG_ALL, data$COSTT4_A, data$AVGFACSAL, data$IRPS_MEN,
                     data$C100_4, data$RET_FT4, data$PCTFLOAN , stringsAsFactors = TRUE)

table.noNA <-na.omit(table2)



##################################################################
```{r}
install.packages("sos")
install.packages("poorman")
install.packages('ggplot2')
install.packages('AICcmodavg')
install.packages('Metrics')
library("sos")
library('poorman')
library('ggplot2')
library(AICcmodavg)
library(Metrics)
#R.versionfindFn("na_if()")

# Load the MERGED2022_23_PP dataset
data <- read.csv("MERGED2022_23_PP.csv")

# Create inital data frame, note that some columns comletely Zero or N/A. Must be addressed
table1 <- data.frame(data$UNITID, data$INSTNM, data$LOCALE, data$CCSIZSET, data$UGDS,
                     data$GRADS, data$SAT_AVG_ALL, data$COSTT4_A, data$AVGFACSAL,
                     data$IRPS_MEN, data$C100_4, data$RET_FT4, data$PCTFLOAN ,
                     data$DEBT_MDN, data$MEDIAN_HH_INC, data$ADM_RATE_ALL, stringsAsFactors = FALSE)

# Creating TUITONFEE column, which is an average of in-state and out-of-state tuition costs.
TUITIONFEE <- data.frame(data$TUITIONFEE_IN, data$TUITIONFEE_OUT)
TUITIONFEE <- rowMeans(TUITIONFEE)


# Create updated data frame, removing completely N/A columns from Table 1, adding TUITFTE and TUITIONFEE (TUITONFEE_IN, TUITIONFEE_OUT, TUITONFEE)
table2 <- data.frame(data$UNITID,  data$LOCALE, data$CCSIZSET, data$UGDS,
                     data$GRADS, data$SAT_AVG_ALL, data$COSTT4_A, data$AVGFACSAL,
                     data$IRPS_MEN, data$C100_4, data$RET_FT4, data$PCTFLOAN, data$ADM_RATE_ALL, data$TUITFTE, data$TUITIONFEE_IN, data$TUITIONFEE_OUT, TUITIONFEE, data$ACTCM75, stringsAsFactors = TRUE)

summary(table2)

# Table 2 still has rows consisting of Zero or N/A columns. School size and Number of graduates are also providing null values (<1). These entries must be converted to N/A and be removed -> tableNONA
tableNONA <- na_if(table2,0)
tableNONA$data.CCSIZSET[tableNONA$data.CCSIZSET < 1] <- NA 
tableNONA$data.GRADS[tableNONA$data.GRADS < 1] <- NA
#na_if(tableNONA$data.CCSIZSET, -2) %>% 
#Following line is ignored if we want to keep all N/A data
tableNONA <-na.omit(tableNONA)

summary(tableNONA)

#Creating training training and testing data (Cutting total data in half: even and odd entries)
#train <- seq(2,918,2)
#test <- seq(1,918,2)
train <- seq(2,848,2)
test <- seq(1,848,2)


TRAIN <- data.frame(tableNONA$data.UNITID[train], tableNONA$data.LOCALE[train], tableNONA$data.CCSIZSET[train], tableNONA$data.UGDS[train], tableNONA$data.GRADS[train], tableNONA$data.SAT_AVG_ALL[train], tableNONA$data.COSTT4_A[train], tableNONA$data.AVGFACSAL[train],  tableNONA$data.IRPS_MEN[train], tableNONA$data.C100_4[train], tableNONA$data.RET_FT4[train], tableNONA$data.PCTFLOAN[train], tableNONA$data.TUITFTE[train], tableNONA$TUITIONFEE[train], tableNONA$data.ADM_RATE_ALL[train], tableNONA$data.ACTCM75[train]   )

TEST <- data.frame(tableNONA$data.UNITID[test],  tableNONA$data.LOCALE[test], tableNONA$data.CCSIZSET[test], tableNONA$data.UGDS[test], tableNONA$data.GRADS[test], tableNONA$data.SAT_AVG_ALL[test], tableNONA$data.COSTT4_A[test], tableNONA$data.AVGFACSAL[test],  tableNONA$data.IRPS_MEN[test], tableNONA$data.C100_4[test], tableNONA$data.RET_FT4[test], tableNONA$data.PCTFLOAN[test], tableNONA$data.TUITFTE[test], tableNONA$TUITIONFEE[test], tableNONA$data.ADM_RATE_ALL[test], tableNONA$data.ACTCM75[test]  )

# Display the first few rows
#head(tableNONA,10)
# Display the dimensions of the dataset
#dim(tableNONA)

# Summary of training and testing data
summary(TRAIN)
summary(TEST)
```


Now that our data is cleaned an split into training an testing data, we can start analyzing with visuals how these obtained variables relate to one another.
```{r}
# Density plot of Cost of Attendance. Note that the highest density occurs for $25,000, $50,000, and $75,000: Multimodal for three peaks.
ggplot(tableNONA, aes(x=data.COSTT4_A)) + geom_density()

# Density plot of Tuition Revenue. NOte that highest density occurs at around $10,000. Then trends downward.
ggplot(tableNONA, aes(x=data.TUITFTE)) + geom_density()

# Density plot of Cost of Attendance. Note that the highest density occurs for $15,000, $35,000, and $60,000: Multimodal for three peaks.
ggplot(tableNONA, aes(x=TUITIONFEE)) + geom_density()

# Box plot comparing the three variables considered to for measuring tuition cost.
boxplot(tableNONA$TUITIONFEE, tableNONA$data.TUITFTE, tableNONA$data.COSTT4_A,
        main = 'Boxplots for Measurements of Tuition Costs',
        at = c(1,2,3),
        names = c('TUITIONFEE','TUITFTE','COSTT4_A'),
        ylab = "Tuition Cost ($)",
        xlab = 'Variables for Tuition Cost') +
  theme_minimal()


# Box plot showing distribution of school sizes
boxplot(tableNONA$data.CCSIZSET,
        main = 'Boxplot for School Sizes'
)

# Box plot showing distribution of admission rates
boxplot(tableNONA$data.ADM_RATE_ALL,
        main = 'Boxplot for Admission Rates',
        at = c(1),
        names = c('Admission Rates')
)

# Scatter plot of Tuition Cost vs Level of Urbanization, with added trendline, note the lack of trends
ggplot(tableNONA, aes(x=data.LOCALE, y=TUITIONFEE)) + geom_point() + geom_smooth() + ggtitle("Tuition vs Level of Urbanization")

# Scatter plot of Tuition Cost vs Retention Rate, with added trendline, note the somewhat exponential trendline
ggplot(tableNONA, aes(x=data.RET_FT4, y=TUITIONFEE)) + geom_point() + geom_smooth() + ggtitle("Tuition vs Retention Rate")

# Scatter plot of Tuition Revenue vs SAT Scores, with added trendline
ggplot(tableNONA, aes(x=data.SAT_AVG_ALL, y=data.TUITFTE)) + geom_point() + geom_smooth() + ggtitle("Tuition vs Tuition Revenue")

# Scatter plot of Tuition Cost vs SAT Scores, with added trendline
ggplot(tableNONA, aes(y=TUITIONFEE, x=data.SAT_AVG_ALL)) + geom_point() + geom_smooth() + ggtitle("Tuition Cost vs SAT Scores")

# Scatter plot of Cost of Attendance vs SAT Scores, with added trendline
ggplot(tableNONA, aes(y=data.COSTT4_A, x=data.SAT_AVG_ALL)) + geom_point() + geom_smooth() + ggtitle("Cost of Attendance vs SAT Scores")

# Scatter plot of Cost of Attendance vs Number of Undergraduates
ggplot(tableNONA, aes(x=data.COSTT4_A, y=data.UGDS)) + geom_point() + geom_smooth() + ggtitle("Cost of Attendance vs Number of Undergrads")

# Scatter plot of Cost of Attendance vs School Size
ggplot(tableNONA, aes(y=data.COSTT4_A, x=data.CCSIZSET)) + geom_point() + geom_smooth() + ggtitle("Cost of Attendance vs School Size")

#Relationship between School Size and Tuition Cost
ggplot(tableNONA, aes(x = factor(data.CCSIZSET), y = TUITIONFEE)) +
  geom_boxplot(fill = "green") +
  labs(title = "Tuition Cost by School Size",
       x = "School Size",
       y = "Tuition Cost") +
  theme_minimal()


# Histogram showing distribution of school sizes, note that smallest tschool size is 6 (scale starts)
ggplot(tableNONA, aes(x = data.CCSIZSET)) +
  geom_histogram(binwidth = 2, fill = "lightblue", color = "black") +
  labs(title = "Distribution of School Sizes",
       x = "School Size",
       y = "Frequency") 

# Bar Chart showing distribution of school sizes, note that smallest school size is 6 (scale starts at 1), theres several small schools that are being omitted.
ggplot(tableNONA, aes(x = data.CCSIZSET) ) +
  geom_bar(binwidth = 2, fill = "lightblue", color = "black") +
  labs(title = "Distribution of School Sizes",
       x = "School Size",
       y = "Frequency") 

# Correlation matrix showing how much each variable relates to another
cor_matrix <- cor(tableNONA)
cor_matrix
```


Now lets start modeling; taking a look at linear regression to predict Tuition Cost (TUITONFEE) based on independent variables.
```{r}
# Creating a linear model for Tuition Cost, based on SAT Scores
tuition_model1 <- lm(tableNONA.TUITIONFEE.train. ~ tableNONA.data.SAT_AVG_ALL.train., data = TRAIN)

summary(tuition_model1)

# Linear model for Tuition Cost, with multiple variables
tuition_model2 <- lm(tableNONA.TUITIONFEE.train. ~ tableNONA.data.SAT_AVG_ALL.train.+ tableNONA.data.UGDS.train., data = TRAIN)

summary(tuition_model2)

# Linear model for Tuition Cost, all desired variables - BASED ON CORR MATRIX
tuition_model3 <- lm(tableNONA.TUITIONFEE.train. ~ tableNONA.data.C100_4.train. + tableNONA.data.SAT_AVG_ALL.train. + tableNONA.data.ACTCM75.train. + tableNONA.data.RET_FT4.train. + tableNONA.data.ADM_RATE_ALL.train. + tableNONA.data.AVGFACSAL.train. +  tableNONA.data.UGDS.train., data = TRAIN)

summary(tuition_model3)


# Linear model for Tuition Cost, except: ACTCM75
tuition_model4 <- lm(tableNONA.TUITIONFEE.train. ~ tableNONA.data.C100_4.train. + tableNONA.data.SAT_AVG_ALL.train.  + tableNONA.data.RET_FT4.train. + tableNONA.data.ADM_RATE_ALL.train. + tableNONA.data.AVGFACSAL.train. +  tableNONA.data.UGDS.train., data = TRAIN)

summary(tuition_model4)

# Linear model for Tuition Cost, except: ACTCM75, ADM_RATE_ALL
tuition_model5 <- lm(tableNONA.TUITIONFEE.train. ~ tableNONA.data.C100_4.train. + tableNONA.data.SAT_AVG_ALL.train.  + tableNONA.data.RET_FT4.train.  + tableNONA.data.AVGFACSAL.train. +  tableNONA.data.UGDS.train., data = TRAIN)

summary(tuition_model5)

# Linear model for Tuition Cost, except: ACTCM75, ADM_RATE_ALL, RET_FT4
tuition_model6 <- lm(tableNONA.TUITIONFEE.train. ~ tableNONA.data.C100_4.train. + tableNONA.data.SAT_AVG_ALL.train.  + tableNONA.data.AVGFACSAL.train. +  tableNONA.data.UGDS.train., data = TRAIN)

summary(tuition_model6)


# FINAL Linear model for Tuition Cost, Except: ACTCM75, ADM_RATE_ALL, RET_FT4, AVGFACSAL 
tuition_model7 <- lm(tableNONA.TUITIONFEE.train. ~ tableNONA.data.C100_4.train. + tableNONA.data.SAT_AVG_ALL.train. +  tableNONA.data.UGDS.train., data = TRAIN)

summary(tuition_model7)



# Linear model for Tuition Cost, all desired variables, Except: C100_4
tuition_model8 <- lm(tableNONA.TUITIONFEE.train. ~  tableNONA.data.SAT_AVG_ALL.train. + tableNONA.data.ACTCM75.train. + tableNONA.data.RET_FT4.train. + tableNONA.data.ADM_RATE_ALL.train. + tableNONA.data.AVGFACSAL.train. +  tableNONA.data.UGDS.train., data = TRAIN)

summary(tuition_model8)

# Linear model for Tuition Cost, all desired variables, Except: C100_4, ACTCM75
tuition_model9 <- lm(tableNONA.TUITIONFEE.train. ~  tableNONA.data.SAT_AVG_ALL.train. + tableNONA.data.RET_FT4.train. + tableNONA.data.ADM_RATE_ALL.train. + tableNONA.data.AVGFACSAL.train. +  tableNONA.data.UGDS.train., data = TRAIN)

summary(tuition_model9)

# Linear model for Tuition Cost, all desired variables, Except: C100_4, ACTCM75, ADM_RATE_ALL
tuition_model10 <- lm(tableNONA.TUITIONFEE.train. ~  tableNONA.data.SAT_AVG_ALL.train. + tableNONA.data.RET_FT4.train. +  tableNONA.data.AVGFACSAL.train. +  tableNONA.data.UGDS.train., data = TRAIN)

summary(tuition_model10)

# Linear model for Tuition Cost, all desired variables, Except: C100_4, ACTCM75, ADM_RATE_ALL, AVGFACSAL
tuition_model11 <- lm(tableNONA.TUITIONFEE.train. ~  tableNONA.data.SAT_AVG_ALL.train. + tableNONA.data.RET_FT4.train. +   tableNONA.data.UGDS.train., data = TRAIN)

summary(tuition_model11)

# Linear model for Tuition Cost, all desired variables, Except: ACTCM75, ADM_RATE_ALL, AVGFACSAL
tuition_model12 <- lm(tableNONA.TUITIONFEE.train. ~  tableNONA.data.SAT_AVG_ALL.train. + tableNONA.data.RET_FT4.train. + tableNONA.data.C100_4.train. +  tableNONA.data.UGDS.train., data = TRAIN)

summary(tuition_model12)



# Linear model for Tuition Cost, ALL VARIABLES - IGNORING CORR MATIX
tuition_model13 <- lm(tableNONA.TUITIONFEE.train. ~ tableNONA.data.UNITID.train. + tableNONA.data.LOCALE.train. + tableNONA.data.CCSIZSET.train. + tableNONA.data.UGDS.train. + tableNONA.data.GRADS.train. + tableNONA.data.SAT_AVG_ALL.train.  + tableNONA.data.AVGFACSAL.train. +  tableNONA.data.IRPS_MEN.train. + tableNONA.data.C100_4.train. + tableNONA.data.RET_FT4.train. + tableNONA.data.PCTFLOAN.train. + + tableNONA.data.ADM_RATE_ALL.train. + tableNONA.data.ACTCM75.train., data = TRAIN   )

summary(tuition_model13)

# Linear model for Tuition Cost, ALL VARIABLES - IGNORING CORR MATIX, Except: ACTCM75
tuition_model14 <- lm(tableNONA.TUITIONFEE.train. ~ tableNONA.data.UNITID.train. + tableNONA.data.LOCALE.train. + tableNONA.data.CCSIZSET.train. + tableNONA.data.UGDS.train. + tableNONA.data.GRADS.train. + tableNONA.data.SAT_AVG_ALL.train.  + tableNONA.data.AVGFACSAL.train. +  tableNONA.data.IRPS_MEN.train. + tableNONA.data.C100_4.train. + tableNONA.data.RET_FT4.train. + tableNONA.data.PCTFLOAN.train. + + tableNONA.data.ADM_RATE_ALL.train., data = TRAIN   )

summary(tuition_model14)

# Linear model for Tuition Cost, ALL VARIABLES - IGNORING CORR MATIX, Except: ACTCM75, GRADS
tuition_model15 <- lm(tableNONA.TUITIONFEE.train. ~ tableNONA.data.UNITID.train. + tableNONA.data.LOCALE.train. + tableNONA.data.CCSIZSET.train. + tableNONA.data.UGDS.train.  + tableNONA.data.SAT_AVG_ALL.train.  + tableNONA.data.AVGFACSAL.train. +  tableNONA.data.IRPS_MEN.train. + tableNONA.data.C100_4.train. + tableNONA.data.RET_FT4.train. + tableNONA.data.PCTFLOAN.train. + + tableNONA.data.ADM_RATE_ALL.train., data = TRAIN   )

summary(tuition_model15)


# Linear model for Tuition Cost, ALL VARIABLES - IGNORING CORR MATIX, Except: ACTCM75, GRADS, UNITID
tuition_model16 <- lm(tableNONA.TUITIONFEE.train. ~  tableNONA.data.LOCALE.train. + tableNONA.data.CCSIZSET.train. + tableNONA.data.UGDS.train.  + tableNONA.data.SAT_AVG_ALL.train.  + tableNONA.data.AVGFACSAL.train. +  tableNONA.data.IRPS_MEN.train. + tableNONA.data.C100_4.train. + tableNONA.data.RET_FT4.train. + tableNONA.data.PCTFLOAN.train. + + tableNONA.data.ADM_RATE_ALL.train., data = TRAIN   )

summary(tuition_model16)


# Linear model for Tuition Cost, ALL VARIABLES - IGNORING CORR MATIX, Except: ACTCM75, GRADS, UNITID, RET_FT4
# Model Equation: f(x) = 
tuition_model17 <- lm(tableNONA.TUITIONFEE.train. ~  tableNONA.data.LOCALE.train. + tableNONA.data.CCSIZSET.train. + tableNONA.data.UGDS.train.  + tableNONA.data.SAT_AVG_ALL.train.  + tableNONA.data.AVGFACSAL.train. +  tableNONA.data.IRPS_MEN.train. + tableNONA.data.C100_4.train. + tableNONA.data.PCTFLOAN.train.  + tableNONA.data.ADM_RATE_ALL.train., data = TRAIN   )

summary(tuition_model17)


# Linear model for Tuition Cost, ALL VARIABLES - IGNORING CORR MATIX, Except: ACTCM75, GRADS, UNITID, RET_FT4, ADM_RATE_ALL
# Model Equation: f(x) = 
tuition_model18 <- lm(tableNONA.TUITIONFEE.train. ~  tableNONA.data.LOCALE.train. + tableNONA.data.CCSIZSET.train. + tableNONA.data.UGDS.train.  + tableNONA.data.SAT_AVG_ALL.train.  + tableNONA.data.AVGFACSAL.train. +  tableNONA.data.IRPS_MEN.train. + tableNONA.data.C100_4.train. + tableNONA.data.PCTFLOAN.train., data = TRAIN   )

summary(tuition_model18)

# Linear model for Tuition Cost, ALL VARIABLES - IGNORING CORR MATIX, Except: ACTCM75, GRADS, UNITID, RET_FT4, ADM_RATE_ALL, IRPS_MEN.
# Model Equation: f(x) = -134.6(x1) - 1032(x2) - 0.564(x3) + 36.92(x4) + 1.5(x5) + ... ...33130(x6) + 18060(x7) - 32130
tuition_model19 <- lm(tableNONA.TUITIONFEE.train. ~  tableNONA.data.LOCALE.train. + tableNONA.data.CCSIZSET.train. + tableNONA.data.UGDS.train.  + tableNONA.data.SAT_AVG_ALL.train.  + tableNONA.data.AVGFACSAL.train. + tableNONA.data.C100_4.train. + tableNONA.data.PCTFLOAN.train., data = TRAIN   )

summary(tuition_model19)


#EXP: Linear Model without losing the NA's from SAT Scores - Result is very similar to tuition_model 20 results: Better R-squared value by 3%.  Larger F-stat of 378 on 1045 DOF. RSE = 8563 on 1045 DOF. This can all be attributed to the 2x increase in sample size.
tuition_modelNA1 <- lm(TUITIONFEE ~ data.LOCALE + data.CCSIZSET + data.UGDS + data.SAT_AVG_ALL + data.AVGFACSAL + data.C100_4 + data.PCTFLOAN, data = tableNONA)

summary(tuition_modelNA1)

```



Let's start testing this final model (tuition_model17) with the test data
```{r}
#Creating model data to compare with Testing data set(tableNONA.test) - Model19
#Only run the TUITIONFEEguess equation for the desired Model Version
TUITIONFEEguess <- -134.6*(TEST$tableNONA.data.LOCALE.test.) - 1032*(TEST$tableNONA.data.CCSIZSET.test.) - 0.564*(TEST$tableNONA.data.UGDS.test.) + 36.92*(TEST$tableNONA.data.SAT_AVG_ALL.test.) + 1.5*(TEST$tableNONA.data.AVGFACSAL.test.) + 33130*(TEST$tableNONA.data.C100_4.test.) + 18060*(TEST$tableNONA.data.PCTFLOAN.test.) - 32130

#Creating model data to compare with Testing data set(tableNONA.test) - Model17
TUITIONFEEguess <- -131.5*(TEST$tableNONA.data.LOCALE.test.) - 932.8*(TEST$tableNONA.data.CCSIZSET.test.) - 0.549*(TEST$tableNONA.data.UGDS.test.) + 35.34*(TEST$tableNONA.data.SAT_AVG_ALL.test.) + 1.4*(TEST$tableNONA.data.AVGFACSAL.test.) + 32170*(TEST$tableNONA.data.C100_4.test.) + 17890*(TEST$tableNONA.data.PCTFLOAN.test.) - 10560*(TEST$tableNONA.data.IRPS_MEN.test.) - 4709*(TEST$tableNONA.data.ADM_RATE_ALL.test.)   - 21480

#Creating model data to compare with Testing data set(tableNONA.test) - Model18
TUITIONFEEguess <- -127.2*(TEST$tableNONA.data.LOCALE.test.) - 1041*(TEST$tableNONA.data.CCSIZSET.test.) - 0.553*(TEST$tableNONA.data.UGDS.test.) + 38.4*(TEST$tableNONA.data.SAT_AVG_ALL.test.) + 1.5*(TEST$tableNONA.data.AVGFACSAL.test.) + 32710*(TEST$tableNONA.data.C100_4.test.) + 17020*(TEST$tableNONA.data.PCTFLOAN.test.) - 9679*(TEST$tableNONA.data.IRPS_MEN.test.)   - 28280

#Creating model data to compare with Testing data set(tableNONA.test) - Model16
TUITIONFEEguess <- -133*(TEST$tableNONA.data.LOCALE.test.) - 926.2*(TEST$tableNONA.data.CCSIZSET.test.) - 0.538*(TEST$tableNONA.data.UGDS.test.) + 36.65*(TEST$tableNONA.data.SAT_AVG_ALL.test.) + 1.45*(TEST$tableNONA.data.AVGFACSAL.test.) + 33670*(TEST$tableNONA.data.C100_4.test.) + 17720*(TEST$tableNONA.data.PCTFLOAN.test.) - 10240*(TEST$tableNONA.data.IRPS_MEN.test.) - 4602*(TEST$tableNONA.data.ADM_RATE_ALL.test.) - 5812*(TEST$tableNONA.data.RET_FT4.test.)   - 19820


#Prediction Summary - Note that the minimum contains a negative value: Clear error in model estimation
summary(TUITIONFEEguess)
#Actual Summary - Note the similar distribution and mean compared to Prediction Summary
TUITIONFEEtest <- TEST$tableNONA.TUITIONFEE.test.
summary(TUITIONFEEtest)

#Calculating Mean Squared Error (MSE) - Specify tuition_model version here
tuition_modelsumm <- summary(tuition_model17)
#Calculation 1
mse1 <- mean(tuition_modelsumm$residuals^2)
mse1
#Calculation 2
mse2 <- mean((TUITIONFEEtest - TUITIONFEEguess)^2)
mse2

#Calculating Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((TUITIONFEEtest - TUITIONFEEguess)^2))
rmse

#Calculating Mean Absolute Error (MAE)
MAE1 <- mae(TUITIONFEEtest, TUITIONFEEguess)
MAE1
# MAE By hand using for-loop
n = 424
sum = 0
for (i in 1:n){
  sum = abs(TUITIONFEEtest[i] - TUITIONFEEguess[i]) + sum
}
MAE2 <- sum/n
MAE2


#Calculating AIC value
models <- list(tuition_model3, tuition_model4, tuition_model5, tuition_model6, tuition_model7, tuition_model8, tuition_model9, tuition_model10, tuition_model11, tuition_model12, tuition_model13, tuition_model14, tuition_model15, tuition_model16, tuition_model17, tuition_model18, tuition_model19)

modelnames <- c('model3','model4','model5','model6','model7','model8','model9','model10','model11','model12','model13','model14','model15','model16','model17','model18','model19')

aictab(cand.set = models, modnames = modelnames )
# From the AIC table, it's seen that Model version 17 performs better than Model 19, and the same can be said for Model 16 and 18: These models will be examined also to compare and verify if Model 19 is most optimal.
```



Now that we have our multiple linear regression model for tuition cost. Let's now create some single linear regression models to allow us to estimate future independent variables over time. This will in turn allow us to predict future tuition costs.

We must first create our data tables for the important variables dating back to 2010.
```{r}
# creating data sets of previous years
#2021-2022 Data
data22 <- read.csv("MERGED2021_22_PP.csv")
table22 <- data.frame(data22$UNITID, data22$INSTNM, data22$UGDS, data22$SAT_AVG_ALL, data22$AVGFACSAL, data22$C100_4, data22$PCTFLOAN, data22$ADM_RATE_ALL, data22$IRPS_MEN, stringsAsFactors = FALSE)

table22 <- na_if(table22,0)
table22 <-na.omit(table22)

#2020-2021 Data
data21 <- read.csv("MERGED2020_21_PP.csv")
table21 <- data.frame(data21$UNITID, data21$INSTNM, data21$UGDS, data21$SAT_AVG_ALL, data21$AVGFACSAL, data21$C100_4, data21$PCTFLOAN, data21$ADM_RATE_ALL, data21$IRPS_MEN, stringsAsFactors = FALSE)

table21 <- na_if(table21,0)
table21 <-na.omit(table21)


#2019-2020 Data
data20 <- read.csv("MERGED2019_20_PP.csv")
table20 <- data.frame(data20$UNITID, data20$INSTNM, data20$UGDS, data20$SAT_AVG_ALL, data20$AVGFACSAL, data20$C100_4, data20$PCTFLOAN, data20$ADM_RATE_ALL, data20$IRPS_MEN, stringsAsFactors = FALSE)

table20 <- na_if(table20,0)
table20 <-na.omit(table20)

#2018-2019 Data
data19 <- read.csv("MERGED2018_19_PP.csv")
table19 <- data.frame(data19$UNITID, data19$INSTNM, data19$UGDS, data19$SAT_AVG_ALL, data19$AVGFACSAL, data19$C100_4, data19$PCTFLOAN, data19$ADM_RATE_ALL, data19$IRPS_MEN, stringsAsFactors = FALSE)

table19 <- na_if(table19,0)
table19 <-na.omit(table19)


#2017-2018 Data
data18 <- read.csv("MERGED2017_18_PP.csv")
table18 <- data.frame(data18$UNITID, data18$INSTNM, data18$UGDS, data18$SAT_AVG_ALL, data18$AVGFACSAL, data18$C100_4, data18$PCTFLOAN, data18$ADM_RATE_ALL, data18$IRPS_MEN, stringsAsFactors = FALSE)

table18 <- na_if(table18,0)
table18 <-na.omit(table18)


#2016-2017 Data
data17 <- read.csv("MERGED2016_17_PP.csv")
table17 <- data.frame(data17$UNITID, data17$INSTNM, data17$UGDS, data17$SAT_AVG_ALL, data17$AVGFACSAL, data17$C100_4, data17$PCTFLOAN, data17$ADM_RATE_ALL, data17$IRPS_MEN, stringsAsFactors = FALSE)

table17 <- na_if(table17,0)
table17 <-na.omit(table17)



#2015-2016 Data
data16 <- read.csv("MERGED2015_16_PP.csv")
table16 <- data.frame(data16$UNITID, data16$INSTNM, data16$UGDS, data16$SAT_AVG_ALL, data16$AVGFACSAL, data16$C100_4, data16$PCTFLOAN, data16$ADM_RATE_ALL, data16$IRPS_MEN, stringsAsFactors = FALSE)

table16 <- na_if(table16,0)
table16 <-na.omit(table16)



#2014-2015 Data   Notice: IRPS_MEN is exclusively N/A -> Must remove column
data15 <- read.csv("MERGED2014_15_PP.csv")
table15 <- data.frame(data15$UNITID, data15$INSTNM, data15$UGDS, data15$SAT_AVG_ALL, data15$AVGFACSAL, data15$C100_4, data15$PCTFLOAN, data15$ADM_RATE_ALL, stringsAsFactors = FALSE)

table15 <- na_if(table15,0)
table15 <- na.omit(table15)



#2013-2014 Data
data14 <- read.csv("MERGED2013_14_PP.csv")
table14 <- data.frame(data14$UNITID, data14$INSTNM, data14$UGDS, data14$SAT_AVG_ALL, data14$AVGFACSAL, data14$C100_4, data14$PCTFLOAN, data14$ADM_RATE_ALL, data14$IRPS_MEN, stringsAsFactors = FALSE)

table14 <- na_if(table14,0)
table14 <-na.omit(table14)



#2012-2013 Data Notice: Same as 2015, IRPS_MEN is exclusively N/A -> Must remove column
data13 <- read.csv("MERGED2012_13_PP.csv")
table13 <- data.frame(data13$UNITID, data13$INSTNM, data13$UGDS, data13$SAT_AVG_ALL, data13$AVGFACSAL, data13$C100_4, data13$PCTFLOAN, data13$ADM_RATE_ALL, stringsAsFactors = FALSE)

table13 <- na_if(table13,0)
table13 <-na.omit(table13)



#2011-2012 Data
data12 <- read.csv("MERGED2011_12_PP.csv")
table12 <- data.frame(data12$UNITID, data12$INSTNM, data12$UGDS, data12$SAT_AVG_ALL, data12$AVGFACSAL, data12$C100_4, data12$PCTFLOAN, data12$ADM_RATE_ALL, data12$IRPS_MEN, stringsAsFactors = FALSE)

table12 <- na_if(table12,0)
table12 <-na.omit(table12)



#2010-2011 Data  Notice: Same as 2015, IRPS_MEN is exclusively N/A -> Must remove column
data11 <- read.csv("MERGED2010_11_PP.csv")
table11 <- data.frame(data11$UNITID, data11$INSTNM, data11$UGDS, data11$SAT_AVG_ALL, data11$AVGFACSAL, data11$C100_4, data11$PCTFLOAN, data11$ADM_RATE_ALL, stringsAsFactors = FALSE)

table11 <- na_if(table11,0)
table11 <-na.omit(table11)


#2009-2010 Data
data10 <- read.csv("MERGED2009_10_PP.csv")
table10 <- data.frame(data10$UNITID, data10$INSTNM, data10$UGDS, data10$SAT_AVG_ALL, data10$AVGFACSAL, data10$C100_4, data10$PCTFLOAN, data10$ADM_RATE_ALL, data10$IRPS_MEN, stringsAsFactors = FALSE)

table10 <- na_if(table10,0)
table10 <-na.omit(table10)

```



Now that tables are made for each year from 2010-2022, we can start creating lists for each variable. Looking at tables 10 through 22, we can see that the smallest range is 1021 entries: We will limit each column to this amount.
```{r}
# Creating variable set for level of urbanization, it was noticed that all entries besides 2023 are logical N/A. This is okay, as data such as level of urbanization and school size wouldn't change over time. These variables can be ignored for now

UNITIDlist <- data.frame(table10$data10.UNITID[1:1021], table11$data11.UNITID[1:1021], table12$data12.UNITID[1:1021], table13$data13.UNITID[1:1021], table14$data14.UNITID[1:1021], table15$data15.UNITID[1:1021], table16$data16.UNITID[1:1021], table17$data17.UNITID[1:1021], table18$data18.UNITID[1:1021], table19$data19.UNITID[1:1021], table20$data20.UNITID[1:1021], table21$data21.UNITID[1:1021], table22$data22.UNITID[1:1021])

INSTNMlist <- data.frame(table10$data10.INSTNM[1:1021], table11$data11.INSTNM[1:1021], table12$data12.INSTNM[1:1021], table13$data13.INSTNM[1:1021], table14$data14.INSTNM[1:1021], table15$data15.INSTNM[1:1021], table16$data16.INSTNM[1:1021], table17$data17.INSTNM[1:1021], table18$data18.INSTNM[1:1021], table19$data19.INSTNM[1:1021], table20$data20.INSTNM[1:1021], table21$data21.INSTNM[1:1021], table22$data22.INSTNM[1:1021])


UGDSlist <- data.frame(table10$data10.UGDS[1:1021], table11$data11.UGDS[1:1021], table12$data12.UGDS[1:1021], table13$data13.UGDS[1:1021], table14$data14.UGDS[1:1021], table15$data15.UGDS[1:1021], table16$data16.UGDS[1:1021], table17$data17.UGDS[1:1021], table18$data18.UGDS[1:1021], table19$data19.UGDS[1:1021], table20$data20.UGDS[1:1021], table21$data21.UGDS[1:1021], table22$data22.UGDS[1:1021])


SAT_AVG_ALLlist <- data.frame(table10$data10.SAT_AVG_ALL[1:1021], table11$data11.SAT_AVG_ALL[1:1021], table12$data12.SAT_AVG_ALL[1:1021], table13$data13.SAT_AVG_ALL[1:1021], table14$data14.SAT_AVG_ALL[1:1021], table15$data15.SAT_AVG_ALL[1:1021], table16$data16.SAT_AVG_ALL[1:1021], table17$data17.SAT_AVG_ALL[1:1021], table18$data18.SAT_AVG_ALL[1:1021], table19$data19.SAT_AVG_ALL[1:1021], table20$data20.SAT_AVG_ALL[1:1021], table21$data21.SAT_AVG_ALL[1:1021], table22$data22.SAT_AVG_ALL[1:1021])


AVGFACSALlist <- data.frame(table10$data10.AVGFACSAL[1:1021], table11$data11.AVGFACSAL[1:1021], table12$data12.AVGFACSAL[1:1021], table13$data13.AVGFACSAL[1:1021], table14$data14.AVGFACSAL[1:1021], table15$data15.AVGFACSAL[1:1021], table16$data16.AVGFACSAL[1:1021], table17$data17.AVGFACSAL[1:1021], table18$data18.AVGFACSAL[1:1021], table19$data19.AVGFACSAL[1:1021], table20$data20.AVGFACSAL[1:1021], table21$data21.AVGFACSAL[1:1021], table22$data22.AVGFACSAL[1:1021])


C100_4list <- data.frame(table10$data10.C100_4[1:1021], table11$data11.C100_4[1:1021], table12$data12.C100_4[1:1021], table13$data13.C100_4[1:1021], table14$data14.C100_4[1:1021], table15$data15.C100_4[1:1021], table16$data16.C100_4[1:1021], table17$data17.C100_4[1:1021], table18$data18.C100_4[1:1021], table19$data19.C100_4[1:1021], table20$data20.C100_4[1:1021], table21$data21.C100_4[1:1021], table22$data22.C100_4[1:1021])


PCTFLOANlist <- data.frame(table10$data10.PCTFLOAN[1:1021], table11$data11.PCTFLOAN[1:1021], table12$data12.PCTFLOAN[1:1021], table13$data13.PCTFLOAN[1:1021], table14$data14.PCTFLOAN[1:1021], table15$data15.PCTFLOAN[1:1021], table16$data16.PCTFLOAN[1:1021], table17$data17.PCTFLOAN[1:1021], table18$data18.PCTFLOAN[1:1021], table19$data19.PCTFLOAN[1:1021], table20$data20.PCTFLOAN[1:1021], table21$data21.PCTFLOAN[1:1021], table22$data22.PCTFLOAN[1:1021])


ADM_RATE_ALLlist <- data.frame(table10$data10.ADM_RATE_ALL[1:1021], table11$data11.ADM_RATE_ALL[1:1021], table12$data12.ADM_RATE_ALL[1:1021], table13$data13.ADM_RATE_ALL[1:1021], table14$data14.ADM_RATE_ALL[1:1021], table15$data15.ADM_RATE_ALL[1:1021], table16$data16.ADM_RATE_ALL[1:1021], table17$data17.ADM_RATE_ALL[1:1021], table18$data18.ADM_RATE_ALL[1:1021], table19$data19.ADM_RATE_ALL[1:1021], table20$data20.ADM_RATE_ALL[1:1021], table21$data21.ADM_RATE_ALL[1:1021], table22$data22.ADM_RATE_ALL[1:1021])

#IRPS_MENlist isn't declared since IRPS_MEN data is not offered before the 2022-2023 academic year.

#Creating TUITIONFEElist to compare final estimation to
TUITIONFEElist <- tableNONA$TUITIONFEE
TUITIONFEElist


summary(UNITIDlist)

#SchoolByID <- list(TEST$tableNONA.data.UNITID, TEST$tableNONA.data.INSTNM)
SchoolByID <- list(UNITIDlist$table22.data22.UNITID.1.1021., INSTNMlist$table22.data22.INSTNM.1.1021.)
SchoolByID

```


We've now created list for each important variable. We've also tabled every school with its University ID. We can now select a school, and create the linear models that will predict each independent variable over time.
```{r}

# Initializing the time vector used to track trends in each variable.
#TIMElist <- c(1,2,3,4,5,6,7,8,9,10,11,12,13)
TIMElist <- c(1,2,3,4)

# From the SchoolByID table of Test data, let's analyze Alabama State University. We see that this is entry 4 of the table, with the corresponding ID of 100724

#For selected school, declare entry number
entry <- 4


#UGDSvec <- c(UGDSlist[entry,1], UGDSlist[entry,2], UGDSlist[entry,3], UGDSlist[entry,4], UGDSlist[entry,5], UGDSlist[entry,6], UGDSlist[entry,7], UGDSlist[entry,8], UGDSlist[entry,9], UGDSlist[entry,10], UGDSlist[entry,11], UGDSlist[entry,12], UGDSlist[entry,13])


# For vector of UGDS over past 13 years, the trend is not accurrate or linear, with an increasing and decreasing slope: This can be remedied by only taking the past 4 years into account, to only analyze recent trends. The TIMElist vecotr will be altered to reflect this. 
UGDSvec <- c(UGDSlist[entry,10], UGDSlist[entry,11], UGDSlist[entry,12], UGDSlist[entry,13])

UGDStable <- data.frame(UGDSvec, TIMElist)
UGDSmodel <- lm(UGDSvec ~ TIMElist, data = UGDStable)


# Plot of UGDS data over the past 4 years, with added trendline. We can see the clear downward linear trend. Our simple model will accurately predict the next year's data.
ggplot(UGDStable, aes(x=TIMElist, y=UGDSvec)) + geom_point() + geom_smooth() + ggtitle("UGDS Data for Past 4 Years")

# In the model summary, we see that we have a low RSE of 16, which is low considering the values of UGDS are around 3500. An R-squared value of 0.99 also indicates an extremely accurate model result. It can be assumed that taking account the previous 4 years is ideal for tracking recent trends in independent variables
summary(UGDSmodel)



# Creating vector UNITID
UNITIDvec <- c( UNITIDlist[entry,10], UNITIDlist[entry,11], UNITIDlist[entry,12], UNITIDlist[entry,13])   
# Creating model UNITID
UNITIDtable <- data.frame(UNITIDvec, TIMElist)
UNITIDmodel <- lm(UNITIDvec ~ TIMElist, data = UNITIDtable)

summary(UNITIDmodel)


# Creating vector SAT_AVG_ALL
SAT_AVG_ALLvec <- c(SAT_AVG_ALLlist[entry,10], SAT_AVG_ALLlist[entry,11], SAT_AVG_ALLlist[entry,12], SAT_AVG_ALLlist[entry,13])  
# Creating Model SAT_AVG_ALL
SAT_AVG_ALLtable <- data.frame(SAT_AVG_ALLvec, TIMElist)
SAT_AVG_ALLmodel <- lm(SAT_AVG_ALLvec ~ TIMElist, data = SAT_AVG_ALLtable)

# Plot of SAT Score data over the past 4 years, with added trendline. We can't see the any linear trend, but the four data point stay waithin the same relatively small range. Our simple model will still predict an accurate score for the following year.
ggplot(SAT_AVG_ALLtable, aes(x=TIMElist, y=SAT_AVG_ALLvec)) + geom_point() + geom_smooth() + ggtitle("SAT SCores for Past 4 Years")

# In the model summary, the RSE is 15, which is very low for data values around 950. However, the R-squared value is low, at around 0.27, this is also paired with a low F-statistic of .75 and high P-value of 0.478. These undesired values are interesting because despite them, the RSE value is still very low, with predictions for the next year being accurate.
summary(SAT_AVG_ALLmodel)


# Creating vector AVGFACSAL
AVGFACSALvec <- c(AVGFACSALlist[entry,10], AVGFACSALlist[entry,11], AVGFACSALlist[entry,12], AVGFACSALlist[entry,13]) 
# Creating model AVGFACSAL
AVGFACSALtable <- data.frame(AVGFACSALvec, TIMElist)
AVGFACSALmodel <- lm(AVGFACSALvec ~ TIMElist, data = AVGFACSALtable)

summary(AVGFACSALmodel)


# Creating vector C100_4
C100_4vec <- c( C100_4list[entry,10], C100_4list[entry,11], C100_4list[entry,12], C100_4list[entry,13]) 
# Creating model C100_4
C100_4table <- data.frame(C100_4vec, TIMElist)
C100_4model <- lm(C100_4vec ~ TIMElist, data = C100_4table)

summary(C100_4model)


# Creating vector PCTFLOAN
PCTFLOANvec <- c(PCTFLOANlist[entry,10], PCTFLOANlist[entry,11], PCTFLOANlist[entry,12], PCTFLOANlist[entry,13])  
# Creating model PCTFLOAN
PCTFLOANtable <- data.frame(PCTFLOANvec, TIMElist)
PCTFLOANmodel <- lm(PCTFLOANvec ~ TIMElist, data = PCTFLOANtable)

summary(PCTFLOANmodel)


# Creating vector ADM_RATE_ALL
ADM_RATE_ALLvec <- c(ADM_RATE_ALLlist[entry,10], ADM_RATE_ALLlist[entry,11], ADM_RATE_ALLlist[entry,12], ADM_RATE_ALLlist[entry,13])  
# Creating model ADM_RATE_ALL
ADM_RATE_ALLtable <- data.frame(ADM_RATE_ALLvec, TIMElist)
ADM_RATE_ALLmodel <- lm(ADM_RATE_ALLvec ~ TIMElist, data = ADM_RATE_ALLtable)

summary(ADM_RATE_ALLmodel)
```

From each of the single linear regression models, we can estimate each independent variable for the following year. 
```{r}
# For School name entry = 4
# Enter TIMElist value for year wish to predict: 5 for 2023, 6 for 2024, etc...

#For LOCALE prediction - Use Previous year data, value doesn't vary with time
LOCALEguess = TEST$tableNONA.data.LOCALE.test.[entry]
LOCALEguess

#For CCSIZSET prediction - Use Previous year data, values doesn't vary w time
CCSIZSETguess = TEST$tableNONA.data.CCSIZSET.test.[entry]
CCSIZSETguess

#For UGDS prediction
summary(UGDSmodel)
UGDSguess = -137*(5) + 4030
UGDSguess

#For SAT_AVG_ALL prediction
summary(SAT_AVG_ALLmodel)
SAT_AVG_ALLguess = -5.9*(5) + 976.5
SAT_AVG_ALLguess

#For AVGFACSAL prediction
summary(AVGFACSALmodel)
AVGFACSALguess = -18.6*(5) + 7514
AVGFACSALguess

#For IRPS_MEN prediction - Use Previous year data, as only 2023 is provided.
IRPS_MENguess = TEST$tableNONA.data.IRPS_MEN.test.[entry]
IRPS_MENguess

#For C100_4 prediction
summary(C100_4model)
C100_4guess = -0.00069*(5) + 0.12920
C100_4guess

#For PCTFLOAN prediction
summary(PCTFLOANmodel)
PCTFLOANguess = -0.01051*(5) + 0.81425
PCTFLOANguess

#For ADM_RATE_ALL prediction
summary(ADM_RATE_ALLmodel)
ADM_RATE_ALLguess = 0.002734*(5) + 0.971944
ADM_RATE_ALLguess


```

We can now plug these estimated variables into the final model selection of 17, 18, and 19: 
  ```{r}

# For Model 17
TUITIONguess <- -131.5*(LOCALEguess) - 932.8*(CCSIZSETguess) - 0.549*(UGDSguess) + 35.34*(SAT_AVG_ALLguess) + 1.4*(AVGFACSALguess) + 32170*(C100_4guess) + 17890*(PCTFLOANguess) - 10560*(IRPS_MENguess) - 4709*(ADM_RATE_ALLguess)   - 21480

# For Model 18
TUITIONguess <- -127.2*(LOCALEguess) - 1041*(CCSIZSETguess) - 0.553*(UGDSguess) + 38.4*(SAT_AVG_ALLguess) + 1.5*(AVGFACSALguess) + 32710*(C100_4guess) + 17020*(PCTFLOANguess) - 9679*(IRPS_MENguess)  - 28280

#Notice that Model 19 has the smallest Error of 1295, smaller than Model 17 and 18, which were 1719 and 1857 respectfully.
# For Model 19
TUITIONguess <- -134.6*(LOCALEguess) - 1032*(CCSIZSETguess) - 0.564*(UGDSguess) + 36.92*(SAT_AVG_ALLguess) + 1.5*(AVGFACSALguess) + 33130*(C100_4guess) + 18060*(PCTFLOANguess) - 32130
TUITIONguess

TUITIONtest = TUITIONFEElist[entry]
TUITIONtest

error = abs(TUITIONguess-TUITIONtest)
error
```