---
title: "Geography 176A"
author: "[Stephanie Hurtado] (https://github.com/shurtadogonzalez/shurtadogonzalez.github.io)"
subtitle: 'Lab 03: COVID-19 Pandemic'
output:
  html_document:
  theme: journal
---
  
install.packages("tidyverse")
library(tidyverse)

install.packages("knitr")
library(knitr)

install.packages("readxl")
library(readxl)

install.packages("zoo")
library(zoo)


url = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
covid = read_csv(url) 
head(covid)


# Question 1 

set_up = covid %>%
  filter(state %in% "California") %>% 
  group_by(county) %>% 
  mutate(NewCases = cases - lag(cases)) %>% 
  mutate(Cumul_Cases = sum(cases)) %>% 
  ungroup() %>% 
  filter(date == max(date))

(most_new_cases = set_up %>% 
  slice_max(NewCases, n = 5) %>% 
  select(county, NewCases))

(most_cumul_cases = set_up %>% 
    slice_max(Cumul_Cases, n = 5) %>% 
    select(county, Cumul_Cases))

#having the parentheses in the front will allow you to see table in the console instead of having to click on it in the environment tab


install.packages("kableExtra")
library(kableExtra)


knitr::kable(most_new_cases,
             caption = "Most New Cases: California Counties",
             col.names = c("County", "New Cases"),
             format.args = list(big.mark = ",")) %>% 
  kableExtra:: kable_styling("basic", full_width =  TRUE, font_size = NULL)


knitr::kable(most_cumul_cases,
             caption = "Most Cumulative Cases: California Counties",
             col.names = c("County", "Cumulative Cases"),
             format.args = list(big.mark = ",")) %>% 
  kableExtra:: kable_styling("basic", full_width =  TRUE, font_size = NULL)

#Step 4-6
library(readxl)
PopulationEstimates <- read_excel("data/PopulationEstimates.xls", skip = 2) %>% 
  dplyr::select(fips = FIPStxt, state = State, county= Area_Name, pop_2019 = POP_ESTIMATE_2019)


#Step 7 

pop = PopulationEstimates %>% 
  dplyr::right_join(covid, by = "fips") %>% 
  dplyr::filter(state.y == "California") %>% 
  dplyr::group_by(county.y) %>% 
  mutate(cumul_cases_per_cap = sum(cases) / pop_2019) %>% 
  mutate(new_cases_per_cap = ((cases - lag(cases)) / pop_2019)) %>% 
  filter(date == max(date)) %>% 
  ungroup()

  

(most_new_cases_per_cap = pop %>% 
    slice_max(new_cases_per_cap, n = 5) %>% 
    select(county.y, new_cases_per_cap))


(most_cumul_per_cap = pop %>% 
    slice_max(cumul_cases_per_cap, n = 5) %>% 
    select(county.y, cumul_cases_per_cap))


install.packages("formattable")
library(formattable)

knitr::kable(most_new_cases_per_cap,
             caption = "Most New Cases Per Capita: California Counties",
             col.names = c("County", "New Cases Per Capita"),
             format.args = list(big.mark = ",")) %>% 
  kableExtra:: kable_styling("basic", full_width =  TRUE, font_size = NULL)
 


knitr::kable(most_cumul_per_cap,
             caption = "Most Cumulative Cases Per Capita: California Counties",
             col.names = c("County", "Cumulative Cases Per Capita"),
             format.args = list(big.mark = ",")) %>% 
  kableExtra:: kable_styling("basic", full_width =  TRUE, font_size = NULL)

#Step 9

last_14_days = covid %>% 
  right_join(PopulationEstimates, by = "fips") %>% 
  filter(date >= max(date, na.rm = TRUE) - 13, state.x == "California") %>% 
  group_by(county.x) %>% 
  summarize(new_cases_per_cap = sum(cases - lag(cases), na.rm = TRUE) / (max(pop_2019) / 100000)) %>% 
  filter(new_cases_per_cap <= 100) %>% 
  ungroup()

view(last_14_days)
table(last_14_days$county.x)

#As of 8/17/2020, there are a total of 14 safe counties in California. This means they comply with the California Department of Public Health’s criteria of having less than 100 new cases per 100,000 residents over the past 14 days.


# Question 2 
four_states = covid %>% 
  filter (state %in% c("California", "Louisiana", "New York", "Florida")) %>% 
  group_by(state, date) %>%
  summarize(cases = sum(cases)) %>% 
  ungroup() %>% 
  group_by(state) %>% 
  mutate(daily_new_cases= cases-lag(cases), 
         seven_day_roll= zoo::rollmean(daily_new_cases, 7, fill= NA, align= 'right')) %>% 
  ungroup() %>% 
  filter(daily_new_cases > 0)


  ggplot(four_states, aes( x = date)) +
  geom_col(aes(y = daily_new_cases, col= state)) +
  geom_line(aes(y = seven_day_roll, col = "darkred", size = 1)) +
  facet_wrap(~state, scale = "free_y") +
  labs(title = "Daily New Covid Cases at the State Level", x= "Date", y= "Cases", color = "", caption = "Lab 02", subtitle = "COVID-19 Data: NY Times" ) +
  theme_light() +
  theme(legend.position = 'NA')

cap_state= PopulationEstimates %>% 
  select(state, pop_2019) %>% 
  right_join(four_states, by = "state") %>% 
  mutate(state_per_cap = ((cases - lag(cases))/ pop_2019), 
        seven_day_roll= zoo::rollmean(daily_new_cases, 7, fill= NA, align= 'right'))
  

ggplot(cap_state, aes(x=date)) +
  geom_col(aes(y= state_per_cap), col= NA, fill= "#F5B8B5") +
  geom_line(aes(y= seven_day_roll), col= "darkred", size= 1) +
  theme_light() +
  labs(title = "States' New Covid Cases Per Capita", x= "", y= "",  caption = "Lab 02", subtitle = "COVID-19 Data: NY Times" ) +
  theme(legend.position = 'NA')


  
  

            
  

  
  

