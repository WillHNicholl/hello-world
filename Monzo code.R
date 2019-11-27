setwd("~/R")

Monzo_data <- read_csv("Monzo Data Export.csv")


library(tidyverse)
library(lubridate)

#Experimenting

by_created2 <- Monzo_data %>%
  mutate(month = month(created), year = year(created)) %>%
  group_by(year, month, category) %>%
  summarise(total = sum(amount), mean = mean(amount)) %>%
  drop_na()


#Groups Monzo data by month and category of spending, sums the amount spent, removes rows containing NA.

by_created <- Monzo_data %>%
  mutate(month = month(created), year = year(created)) %>%
  group_by(month, year, category) %>%
  summarise(total = sum(amount), mean = mean(amount)) %>%
  drop_na()


#Adds new columns containing absolute value of amount spent and mean spend

by_created$abs_amount <- abs(by_created$total)
by_created$Mean_spend <- abs(by_created$mean)


#Creates new data frame for 2019 data

Monzo_2019 <- by_created %>%
  filter(year == 2019)


#Plots data

ggplot(Monzo_2019, aes(x = month, y = abs_amount)) +
    geom_point(aes(size = abs_mean, color = category)) +
  facet_wrap(~ category) +
  labs(title="Monzo 2019 Spending", x="Amount Spent Per Month", y="Month")

