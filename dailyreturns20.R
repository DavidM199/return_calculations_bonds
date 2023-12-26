library(dplyr)
library(tidyverse)
library(lubridate)


bond_data_new <- df.merged.cpp_coup_
#mid_price
bond_data_new <- bond_data_new %>% mutate(
  mid_price = (bid_price+offer_price)/2
)


#1.01
#creating ncoups 

bond_data_new <- bond_data_new %>%  mutate(interest_frequency = as.numeric(interest_frequency),
  ncoups = case_when(
  interest_frequency < 14 ~ interest_frequency,
  interest_frequency == 14 ~ 6,
  interest_frequency > 14 ~ 0,
  is.na(interest_frequency) ~ 0
))


#1.02, 1.03, 1.04
bond_data_new <- bond_data_new %>%  mutate( 
  #dated_date = if_else(is.na(dated_date), offering_date, dated_date),
  first_interest_date = if_else(is.na(first_interest_date), offering_date, first_interest_date),
  last_interest_date = if_else(is.na(last_interest_date), maturity, last_interest_date),
  coupmonth = interval(first_interest_date, date) %/% months(1)
)



#1.05
library(DescTools)

#1.06 I added this in a function form
func_days <- function(Date1 , Date2){
  D2 <- day(Date2)
  D1 <- day(Date1)
  
  M2 <- month(Date2)
  M1 <- month(Date1)
  
  Y2 <- year(Date2)
  Y1 <- year(Date1)
  
  D1 <- if_else(D1==31 , 30 , D1 , NA)
  D2 <- if_else(D2==31 & D1==30 , 30 , D2 , NA)
  D1 <- if_else(M1==2 & (D1==28 & D1==29) , 30 , D1 ,  NA)
  
  days <- 360*(Y2 - Y1) + 30*(M2 - M1) + (D2 - D1)
}


bond_data_new <- bond_data_new %>% mutate(date_of_last_coup = 
                                                      if_else(
                                                        date < first_interest_date, offering_date, 
                                                        first_interest_date %m+% months(floor(coupmonth / (12/ncoups)) * (12/ncoups))),
                                                    days = func_days(date_of_last_coup , date))



#1.07, 1.08, 1.09
bond_data_new <- bond_data_new %>% mutate(
  coupacc = if_else((date >= offering_date) & (date <= maturity), coupon * days/360, 0),
  coupamt = if_else(date == date_of_last_coup, coupon/ncoups, 0)
)
bond_data_new <- bond_data_new %>% mutate(
  coupacc = if_else(ncoups == 0, 0, coupacc),
  coupamt = if_else(ncoups == 0, 0, coupamt)
)

#1.10
bond_data_new <- bond_data_new %>% group_by(cusip) %>% arrange(date) %>%  mutate(
  cumsum_coupamt = cumsum(coupamt)
)

#I rewrote my code and now I use offering_date instead of dated_date for better comparison
#coupacc appears to be same in both dataframes - my dataframe and the original one
bond_data_new$results <- ifelse(bond_data_new$coupacc == df.merged.cpp_coup_$coupacc, "Good", "NotGood")
result <-  filter(bond_data_new, results == "NotGood")

#Calculating 20 daily returns
day_count <- 20
returns20 <-  function(df.bond) {
  for (i in 1:day_count) {
    df.bond <- df.bond %>% mutate("return_d_{i}" := lead((mid_price - lag(mid_price) 
                                              + coupacc - lag(coupacc)
                                              +cumsum_coupamt - lag(cumsum_coupamt)
    )/lag(mid_price + coupacc), n=i))
  }
  return(df.bond)
}


bond_data_returns <- bond_data_new %>%  group_by(cusip) %>%  returns20() %>% select(-results)
write_csv(bond_data_returns, "bond_data_20returns.csv")


