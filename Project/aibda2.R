## Correlation Analysis 

library(MASS)
str(cats)

plot(cats$Hwt ~ cats$Bwt,
     col="green", pch=5,
     xlab="Body Weight (kg)", ylab="Heart Weight (g)",
     main="Body Weight and Heart Weight of Cats")

cor(cats$Bwt, cats$Hwt)


## Simple Linear Regression Analysis

install.packages("car")
library(car)
str(Prestige)

Prestige.lm <- lm(income ~ education, data=Prestige)
Prestige.lm

plot(Prestige$income ~ Prestige$education,
     col="cornflowerblue", pch=19,
     xlab="Education (years)", ylab="Income (dollars)",
     main="Education and Income")
abline(Prestige.lm, col="salmon")

summary(Prestige.lm)


## Multiple Linear Regression Analysis

data(mtcars)
str(mtcars)
mtcars <- mtcars[c("mpg", "hp", "wt", "disp", "drat")]

summary(mtcars)
cor(mtcars)

mtcars.lm <- lm(mpg ~ hp + wt + disp + drat, data=mtcars)
summary(mtcars.lm)
cor.test(~ Bwt + Hwt, data=cats)


## Logistic Regression

bank.df <- read.csv("./datasets/UniversalBank.csv")
bank.df <- bank.df[ , -c(1, 5, 8)]  # Drop ID and zip code, Education columns.

set.seed(123)
train_index <- sample(5000, 4000)
train.df <- bank.df[train_index, ]
valid.df <- bank.df[-train_index, ]

# run logistic regression
# use glm() (general linear model) with family = "binomial" to fit a logistic 
# regression.
logit.reg <- glm(Personal.Loan ~ ., data = train.df, family = "binomial") 
summary(logit.reg)

# use predict() with type = "response" to compute predicted probabilities. 
logit.reg.pred <- predict(logit.reg, valid.df[, -7], type = "response")

# first 5 actual and predicted records
data.frame(actual = valid.df$Personal.Loan[1:30], predicted = logit.reg.pred[1:30])

# create confusion matrix and statistics
install.packages("caret")
library(caret)
confusionMatrix(as.factor(ifelse(logit.reg.pred > 0.5,1,0)), as.factor(valid.df$Personal.Loan))

## Naive Bayes and Word Cloud
# read the sms data into the sms data frame
sms_raw <- read.csv("./datasets/sms_spam.csv", stringsAsFactors = FALSE)

# examine the structure of the sms data
str(sms_raw)

# convert spam/ham to factor.
sms_raw$type <- factor(sms_raw$type)

# examine the type variable more carefully
str(sms_raw$type)
table(sms_raw$type)

# build a corpus using the text mining (tm) package
library(tm)
sms_corpus <- VCorpus(VectorSource(sms_raw$text))

# examine the sms corpus
print(sms_corpus)
inspect(sms_corpus[1:2])

as.character(sms_corpus[[1]])
lapply(sms_corpus[1:2], as.character)

# clean up the corpus using tm_map()
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))

# show the difference between sms_corpus and corpus_clean
as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])

# remove numbers
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)
# remove stop words
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords()) 
# remove punctuation
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)

# return the same vector of terms in its root form
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
# eliminate unneeded whitespace
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)

# examine the final clean corpus
lapply(sms_corpus[1:3], as.character)
lapply(sms_corpus_clean[1:3], as.character)

# create a document-term sparse matrix
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
sms_dtm
inspect(sms_dtm)

# creating training and test datasets
sms_raw_train <- sms_raw[1:4169,]
sms_raw_test <- sms_raw[4170:5559,]

sms_dtm_train <- sms_dtm[1:4169,]
sms_dtm_test <- sms_dtm[4170:5559,]

sms_corpus_train <- sms_corpus_clean[1:4169]
sms_corpus_test <- sms_corpus_clean[4170:5559]

# check that the proportion of spam is similar
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

# word cloud visualization
library(wordcloud)
wordcloud(sms_corpus_train, 
          min.freq = 40, 
          random.order = FALSE)

# subset the training data into spam and ham groups
spam <- subset(sms_raw_train, type == "spam")
ham <- subset(sms_raw_train, type == "ham")
wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

# indicator features for frequent words
sms_dict <- findFreqTerms(sms_dtm_train,5)
sms_train <- sms_dtm_train[ ,sms_dict]
sms_test <- sms_dtm_test[ ,sms_dict]

# convert counts to a factor
convert_counts <- function(x){
        x <- ifelse(x > 0, "Yes", "No")
        return(x)
}

# apply() convert_counts() to columns of train/test data
sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_test, MARGIN = 2, convert_counts)

# training a model on the data ----
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)
sms_classifier

# evaluating model performance ----
sms_test_pred <- predict(sms_classifier, sms_test)

library(gmodels)
CrossTable(sms_test_pred, sms_raw_test$type,
           prop.chisq = FALSE, prop.r = FALSE, prop.t = FALSE, 
           dnn = c('predicted', 'actual'))


## Neural Networks
# Example: Modeling the Strength of Concrete  ----

# read in data and examine structure
concrete <- read.csv("./datasets/concrete.csv")
str(concrete)

# custom normalization function
normalize <- function(x) { 
        return((x - min(x)) / (max(x) - min(x)))
}

# apply normalization to entire data frame
concrete_norm <- as.data.frame(lapply(concrete, normalize))

# confirm that the range is now between zero and one
summary(concrete_norm$strength)

# compared to the original minimum and maximum
summary(concrete$strength)

# create training and test data
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]

# train the neuralnet model
library(neuralnet)

# simple ANN with only a single hidden neuron
set.seed(12345) # to guarantee repeatable results
concrete_model <- neuralnet(formula = strength ~ cement + slag +
                                    ash + water + superplastic + 
                                    coarseagg + fineagg + age,
                            data = concrete_train)

# visualize the network topology
plot(concrete_model)

# obtain model results
model_results <- compute(concrete_model, concrete_test[1:8])
# obtain predicted strength values
predicted_strength <- model_results$net.result
# examine the correlation between predicted and actual values
cor(predicted_strength, concrete_test$strength)

# a more complex neural network topology with 5 hidden neurons
set.seed(12345) # to guarantee repeatable results
concrete_model2 <- neuralnet(strength ~ cement + slag +
                                     ash + water + superplastic + 
                                     coarseagg + fineagg + age,
                             data = concrete_train, hidden = 5)

# plot the network
plot(concrete_model2)

# evaluate the results as we did before
model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)


## Support Vector Machines 
# read in data and examine structure
letters <- read.csv("./datasets/letterdata.csv", stringsAsFactors = TRUE)
str(letters)

# divide into training and test data
letters_train <- letters[1:16000, ]
letters_test  <- letters[16001:20000, ]

# begin by training a simple linear SVM
install.packages("kernlab")
library(kernlab)
letter_classifier <- ksvm(letter ~ ., data = letters_train,
                          kernel = "vanilladot")

# look at basic information about the model
letter_classifier

# predictions on testing dataset
letter_predictions <- predict(letter_classifier, letters_test)

head(letter_predictions)

table(letter_predictions, letters_test$letter)

# construct a vector of TRUE/FALSE indicating correct/incorrect predictions
agreement <- letter_predictions == letters_test$letter
table(agreement)
prop.table(table(agreement))

# change to a RBF kernel
set.seed(12345)
letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train, kernel = "rbfdot")
letter_predictions_rbf <- predict(letter_classifier_rbf, letters_test)

agreement_rbf <- letter_predictions_rbf == letters_test$letter
table(agreement_rbf)
prop.table(table(agreement_rbf))

