

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
library("sos")
library('poorman')
library('ggplot2')
R.versionfindFn("na_if()")

# Load the MERGED2022_23_PP dataset
data <- read.csv("MERGED2022_23_PP.csv")

# Create data frame
table1 <- data.frame(data$UNITID, data$LOCALE, data$CCSIZSET, data$UGDS,
                     data$GRADS, data$SAT_AVG_ALL, data$COSTT4_A, data$AVGFACSAL,
                     data$IRPS_MEN, data$C100_4, data$RET_FT4, data$PCTFLOAN ,
                     data$DEBT_MDN, data$MEDIAN_HH_INC, stringsAsFactors = TRUE)

# Creating TUITONFEE column, which is an average of in-state and out-of-state tuition costs.
TUITIONFEE <- data.frame(data$TUITIONFEE_IN, data$TUITIONFEE_OUT)
TUITIONFEE <- rowMeans(TUITIONFEE)


# Create updated data frame, removing comletely N/A columns from Table 1, adding TUITFTE and TUITIONFEE
table2 <- data.frame(data$UNITID, data$LOCALE, data$CCSIZSET, data$UGDS,
                     data$GRADS, data$SAT_AVG_ALL, data$COSTT4_A, data$AVGFACSAL,
                     data$IRPS_MEN, data$C100_4, data$RET_FT4, data$PCTFLOAN, data$TUITFTE, data$TUITIONFEE_IN, data$TUITIONFEE_OUT, TUITIONFEE, stringsAsFactors = TRUE)

summary(table2)

tableNONA <- na_if(table2,0)
tableNONA$data.CCSIZSET[tableNONA$data.CCSIZSET < 1] <- NA 
tableNONA$data.GRADS[tableNONA$data.GRADS < 1] <- NA
#na_if(tableNONA$data.CCSIZSET, -2) %>% 
tableNONA <-na.omit(tableNONA)

summary(tableNONA)

#Creating training training and testing data
train <- seq(2,918,2)
test <- seq(1,918,2)


TRAIN <- data.frame(tableNONA$data.UNITID[train],tableNONA$data.LOCALE[train], tableNONA$data.CCSIZSET[train], tableNONA$data.UGDS[train], tableNONA$data.GRADS[train], tableNONA$data.SAT_AVG_ALL[train], tableNONA$data.COSTT4_A[train], tableNONA$data.AVGFACSAL[train],  tableNONA$data.IRPS_MEN[train], tableNONA$data.C100_4[train], tableNONA$data.RET_FT4[train], tableNONA$data.PCTFLOAN[train], tableNONA$data.TUITFTE[train], tableNONA$TUITIONFEE[train]  )

TEST <- data.frame(tableNONA$data.UNITID[test],tableNONA$data.LOCALE[test], tableNONA$data.CCSIZSET[test], tableNONA$data.UGDS[test], tableNONA$data.GRADS[test], tableNONA$data.SAT_AVG_ALL[test], tableNONA$data.COSTT4_A[test], tableNONA$data.AVGFACSAL[test],  tableNONA$data.IRPS_MEN[test], tableNONA$data.C100_4[test], tableNONA$data.RET_FT4[test], tableNONA$data.PCTFLOAN[train], tableNONA$data.TUITFTE[test], tableNONA$TUITIONFEE[test]  )

# Display the first few rows
#head(tableNONA,10)
# Display the dimensions of the dataset
#dim(tableNONA)

# Summary of training and testing data
summary(TRAIN)
summary(TEST)
```


Now that our data is cleaned an split into training an testing data, we can start analyzing how these obtained variables relate to one another.
```{r}
# Density plot of Cost of Attendance. Note that the highest density occurs for $25,000, $50,000, and $75,000: Multimodal for three peaks.
ggplot(tableNONA, aes(x=data.COSTT4_A)) + geom_density()

# Density plot of Tuition Revenue. NOte that highest density occurs at around $10,000. Then trends downward.
ggplot(tableNONA, aes(x=data.TUITFTE)) + geom_density()

# Density plot of Cost of Attendance. Note that the highest density occurs for $15,000, $35,000, and $60,000: Multimodal for three peaks.
ggplot(tableNONA, aes(x=TUITIONFEE)) + geom_density()

boxplot(tableNONA$TUITIONFEE, tableNONA$data.TUITFTE, tableNONA$data.COSTT4_A,
        main = 'Boxplots for Measurements of Tuition Costs',
        at = c(1,2,3),
        names = c('TUITIONFEE','TUITFTE','COSTT4_A')
)

boxplot(tableNONA$data.CCSIZSET,
        meain = 'Boxplot for School Sizes'
)


# Scatter plot of Tuition Cost vs Level of Urbanization, with added trendline, note the lack of trends
ggplot(tableNONA, aes(x=data.LOCALE, y=TUITIONFEE)) + geom_point() + geom_smooth() + ggtitle("Tuition vs SAT Scores")

# Scatter plot of Tuition Cost vs Retention Rate, with added trendline, note the somewhat exponential trendline
ggplot(tableNONA, aes(x=data.RET_FT4, y=TUITIONFEE)) + geom_point() + geom_smooth() + ggtitle("Tuition vs SAT Scores")

# Scatter plot of Tuition Revenue vs SAT Scores, with added trendline
ggplot(tableNONA, aes(x=data.SAT_AVG_ALL, y=data.TUITFTE)) + geom_point() + geom_smooth() + ggtitle("Tuition vs SAT Scores")

# Scatter plot of Cost of Attendance vs SAT Scores, with added trendline
ggplot(tableNONA, aes(y=data.COSTT4_A, x=data.SAT_AVG_ALL)) + geom_point() + geom_smooth() + ggtitle("Cost of Attendance vs SAT Scores")

# Scatter plot of Tuition Cost vs SAT Scores, with added trendline
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
```


Now lets start modeling; taking a look at linear regression to predict Tuition Cost (TUITONFEE) based on independent variables.
```{r}
# Creating a linear model for Tuition Cost, based on SAT Scores
tuition_model1 <- lm(tableNONA.TUITIONFEE.train. ~ tableNONA.data.SAT_AVG_ALL.train., data = TRAIN)

summary(tuition_model1)

# Linear model for Tuition Cost, with multiple variables
tuition_model2 <- lm(tableNONA.TUITIONFEE.train. ~ tableNONA.data.SAT_AVG_ALL.train.+ tableNONA.data.UGDS.train., data = TRAIN)

summary(tuition_model2)
```