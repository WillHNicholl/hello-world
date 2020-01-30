getwd()
setwd("C:/Users/331180/Documents/R")
library(tidyverse)
library(shiny)
library(readxl)
library(ggplot2)

World_Bank <- read_excel("October2019globalfinancialdevelopmentdatabase.xlsx", sheet = "Data October 2019")

#Selects relevant columns and changes titles

World_Bank2 <- World_Bank %>%
  select("iso2", "country", "year", "income", "gfddai04", "gfddai27", "gfddai28", "gfddai29", "gfddai30", "gfddai31", "gfddai33", "gfddai36", "gfdddi14", "gfddoi01", "gfddoi04", "gfddoi05", "gfddom01") %>%
  rename(Income_Group = income, SF_w_Bank_Loan = gfddai04, Firms_w_savings_account = gfddai27, Firms_banks_finance_fix = gfddai28, Firms_banks_finance_working = gfddai29, Loans_requiring_capital = gfddai30, Value_col_loan = gfddai31, Firms_loan_rejected = gfddai33, Access_finance_constraint = gfddai36, Domestic_credit_to_private_sector = gfdddi14, Bank_concentration = gfddoi01, Lerner_index = gfddoi04, Boone_indicator = gfddoi05, No_listed_companies_per_1m = gfddom01)


#Selects specific colums and filters by relevant countries

World_Bank3 <- World_Bank2 %>%
  select("country", "year", "Bank_concentration", "Domestic_credit_to_private_sector", "Income_Group") %>%
  drop_na(Bank_concentration, Domestic_credit_to_private_sector) %>%
  filter(year > 2010)

  
World_Bank3$Income_Group <- factor(World_Bank3$Income_Group, levels = c("High income", "Upper middle income", "Lower middle income", "Low income"))

#Plots results

ggplot(World_Bank3, aes(Bank_concentration, Domestic_credit_to_private_sector)) +
    geom_point(aes(colour = Income_Group)) +
    facet_grid(year~Income_Group, scales = "free") +
    geom_smooth(method = lm) +
    labs(title = "Correlation Between Countries' Banking Sector Concentration and Domestic Credit to Private Sector as % GDP", x = "Banking Sector Concentration % (Assets three largest commercial banks as share of total commercial banking assets)", y = "Domestic credit to private sector (% of GDP)")


?drop_na
#Plots Access Finance Constraint

ggplot(World_Bank3, aes(Bank_concentration, Access_finance_constraint)) +
  geom_point(aes(size = Firms_loan_rejected, colour = Income_Group)) +
  facet_wrap(~Income_Group, scales = "free") +
  geom_smooth(method = lm) +
  labs(title = "Correlation Between Banking Sector Concentration and Firms identifying access to finance as a major constraint (%)", x = "Banking Sector Concentration % (Assets three largest commercial banks as share of total commercial banking assets)", y = "Firms identifying access to finance as a major constraint (%)")

