library(dplyr)
library(tidyr)

pred_df <- data_frame(loan_is_bad = test$loan_is_bad, prediction = prediction)

expanded <- crossing(data.frame(cutoff = seq(0, 1, by=.01)), pred_df)
expanded
only_accepted <- filter(expanded, prediction < cutoff)
only_accepted$profit <- ifelse(only_accepted$loan_is_bad == "Yes", -60, 10)
grouped <- group_by(only_accepted, cutoff)
profit_df <- summarise(grouped, profit = sum(profit))


ggplot(profit_df) + geom_line(aes(x=cutoff, y=profit))
