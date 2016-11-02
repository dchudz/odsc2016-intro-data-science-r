
# Read data and see what we have
#
df <- read.csv("~/Google Drive/ODSC/lending_club.csv")

head(df)
str(df)
summary(df)

View(df)


# Exploratory Plots
library(ggplot2)

# make histogram of revolving balance 
# "aes" stands for "aesthetic", specifying mapping of data attribute to chart attribute
ggplot(df, aes(x=revol_bal)) + geom_histogram()

# ggplot elements can be added to charts
ggplot(df, aes(x=revol_bal)) + geom_histogram() + scale_x_log10()

# Let's look into that warning message a little
df$revol_bal == 0
sum(df$revol_bal == 0)
mean(df$revol_bal == 0)

# facet
ggplot(df, aes(x=revol_bal)) + geom_histogram() + 
  scale_x_log10() +
  facet_grid(loan_is_bad ~ .)

# geom_point for scatter plots
ggplot(df, aes(x=annual_inc, y=revol_bal)) + geom_point()

# need better scale
ggplot(df, aes(x=annual_inc, y=revol_bal)) + geom_point() + 
  scale_x_log10() + 
  scale_y_log10() 

# "alpha" controls opacity
ggplot(df, aes(x=annual_inc, y=revol_bal)) + geom_point(alpha=.1) + 
  scale_x_log10() + 
  scale_y_log10()

# smoother
ggplot(df, aes(x=annual_inc, y=revol_bal)) + geom_point(alpha=.1) + 
  scale_x_log10() + 
  scale_y_log10() +
  geom_smooth()


# can use color too
ggplot(df, aes(x=annual_inc, y=revol_bal, color=loan_is_bad)) + geom_point(alpha=.1) + 
  scale_x_log10() + 
  scale_y_log10()


# Model Fitting: Vignettes
# 
library(party)

browseVignettes()
browseVignettes(package = "party")

# Model Fitting: example model
# 
model <- ctree(loan_is_bad ~ annual_inc + revol_bal, data=df)
plot(model)

# Predictions
example_prediction_data <- data.frame(annual_inc = c(40000, 40000), 
                                      revol_bal = c(10000, 40000))
example_prediction_data
predict(model, example_prediction_data)
predict(model, example_prediction_data, type = "prob")


# Train / Test Split
# 
set.seed(0)
is_train <- sample(c(TRUE, FALSE), replace=TRUE, prob=c(.6, .4))
train <- subset(df, is_train)
test <- subset(df, !is_train)

# Fit Model
#
model1 <- ctree(loan_is_bad ~ annual_inc + dti + delinq_2yrs + inq_last_6mths + open_acc + pub_rec + revol_bal + total_acc + collections_12_mths_ex_med + acc_now_delinq,
                data = train)

plot(model1)

# Make prediction
#
prediction <- predict(model1, test)
prediction

mean(prediction == test$loan_is_bad)

table(test$loan_is_bad, prediction, dnn=c("is bad?", "prediction"))

# What does the model mean for our business?
#
# Pretend $100 loans
#
# Correctly predicted as good loan: +$10
# Wrongly predicted as good loan: -$60
# Correctly predicted as bad loan: $0
# Wrongly predicted as bad loan: $0

bad_loan_value <- -60
good_loan_value <- 10

accept_these <- test[prediction == "No", ]
nrow(test)
nrow(accept_these)
num_good <- sum(accept_these$loan_is_bad == "No")
num_good
num_bad <- sum(accept_these$loan_is_bad == "Yes")

profit <- bad_loan_value*num_bad + good_loan_value*num_good
profit

# Ask the group: What should we do?
#
# answer: get probabilities, use a different cutoff
# Try another cutoff:
#

probabilities <- predict(model1, test, type="prob")

probability_bad <- rep(NA, nrow(test))
for (i in seq_along(probabilities)) {
  probability_bad[i] <- probabilities[[i]][2]
}

get_probability_bad <- function(model, df) {
  probabilities <- predict(model, df, type="prob")
  probability_bad <- rep(NA, nrow(df))
  for (i in seq_len(nrow(df))) {
    probability_bad[i] <- probabilities[[i]][2]
  }
  return(probability_bad)
}

test$probability_bad <- probability_bad
# Visualize Prediction
ggplot(test) + geom_histogram(aes(x=probability_bad)) 
ggplot(test) + geom_histogram(aes(x=probability_bad)) + 
  facet_grid(loan_is_bad ~ .) 

# What happens if we try another cutoff?
#
accept_these <- test[probability_bad < 0.1, ]
nrow(test)
nrow(accept_these)
num_good <- sum(accept_these$loan_is_bad == "No")
num_good
num_bad <- sum(accept_these$loan_is_bad == "Yes")

profit <- bad_loan_value*num_bad + good_loan_value*num_good
profit
profit/nrow(test)

# Plot curve of cutoffs
#
cutoffs <- seq(0, 1, by = .01)
cutoff_df <- data.frame(cutoff = cutoffs, 
                        num_good = rep(NA, length(cutoffs)),
                        num_bad = rep(NA, length(cutoffs)),
                        profit = rep(NA, length(cutoffs)))

cutoff_df

prediction <- get_probability_bad(model1, test)

for (i in seq_along(cutoffs)) {
  cat("\014")
  print(cutoff_df)
  print(i)
  cutoff <- cutoff_df$cutoff[i]
  accept_these <- test[prediction < cutoff, ]
  cutoff_df$num_good[i] <- sum(accept_these$loan_is_bad == "No")
  cutoff_df$num_bad[i] <- sum(accept_these$loan_is_bad == "Yes")
  cutoff_df$profit[i] <- bad_loan_value*cutoff_df$num_bad[i] + good_loan_value*cutoff_df$num_good[i]
}

cutoff_df

ggplot(cutoff_df) + geom_line(aes(x=cutoff, y=num_good, color="num_good"))
ggplot(cutoff_df) + geom_line(aes(x=cutoff, y=num_bad, color="num_bad"))
ggplot(cutoff_df) + geom_line(aes(x=cutoff, y=profit)) + ggtitle("Profit")



# Make it a function so we can apply to other models
#
get_profit_curve <- function(cutoffs, test_data, model) {
  num_good <- rep(NA, length(cutoffs))
  num_bad <- rep(NA, length(cutoffs))
  profit <- rep(NA, length(cutoffs))
  prediction <- get_probability_bad(model, test_data)
  for (i in seq_along(cutoffs)) {
    cutoff <- cutoffs[i]
    accept_these <- test_data[prediction < cutoff, ]
    num_good[i] <- sum(accept_these$loan_is_bad == "No")
    num_bad[i] <- sum(accept_these$loan_is_bad == "Yes")
    profit[i] <- bad_loan_value*num_bad[i] + good_loan_value*num_good[i]
  }
  return(profit)
}

get_profit_curve(cutoff_df$cutoff, test, model1)

# New variable
#
model2 <- ctree(loan_is_bad ~ grade + annual_inc + dti + delinq_2yrs + inq_last_6mths + open_acc + pub_rec + revol_bal + total_acc + collections_12_mths_ex_med + acc_now_delinq,
                data = train)

cutoff_df1 <- data.frame(cutoff = cutoffs,
                         profit = get_profit_curve(cutoff_df$cutoff, test, model1),
                         model = "model1")

cutoff_df2 <- data.frame(cutoff = cutoffs,
                         profit = get_profit_curve(cutoff_df$cutoff, test, model2),
                         model = "model2")

cutoff_df_two_models <- rbind(cutoff_df1, cutoff_df2)
ggplot(cutoff_df_two_models) + geom_line(aes(x=cutoff, y=profit, color=model))

