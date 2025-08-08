#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title: waterLevel Gap Filling
# Coder: Nate Jones and Lidia Molina Serpas
# Date: 8/8/2025
# Purpose: Fill gaps in data in waterLevel record for the floodpulse project
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Steps
#   1) load water level data
#   2) load USGS data
#   3) create relationship between USGS gage and water level
#   4) Gap fill

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1.0 Setup workspace ----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Clear memory (because we're old school)
remove(list = ls())

#load libraries of interest
library(tidyverse)      #join the cult
library(lubridate)      #date formatting
library(readxl)         #read excell file
library(dataRetrieval)  #gather USGS gage data
library(zoo)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.0 Load waterLevel data -----------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create download function
download_fun <- function(wetland_id){
  #load data of interest
  df <- read_xlsx('data/UpdatedDataSheet_allwells_jlm.xlsx', sheet = wetland_id, skip=5, col_types = c("numeric"))
  
  #list number of cols
  ncols <- ncol(df)
  
  #Create vector for col types
  col_types <- c("date", rep("numeric", ncols-1))
  
  #load data of interest (but control coltypes this time)
  df <- read_xlsx('data/UpdatedDatasheet_allwells_jlm.xlsx', sheet = wetland_id, skip=5, col_types = col_types)
  #tidy data and convert to mean daily
  df <- df %>% 
    #tidy data
    select(datetime, water_level_cm_corrected) %>% 
    rename(wL = water_level_cm_corrected) %>% 
    drop_na() %>% 
    #Convert to daily mean data
    mutate(day = date(datetime)) %>% 
    group_by(day) %>% 
    summarise(wL = mean(wL, na.rm=T)) %>% 
    mutate(wetland_id = wetland_id)
  df  
}


#Create vector of wetlands of interest
wetlands <- c("HS1-1", "HS2-1", "HS3-1", 
              "IT1-6", "TBS26", "IT3-1", 
              "FP1-2", "FP2-1", "FP3-1")

# Water level metrics for all 9 wetlands
df <-lapply(X = wetlands, FUN = download_fun) %>% bind_rows() 
df

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.0 Load USGS gage data ------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Download daily flow data from NWIS (nwis.usgs.gov)
df_usgs<-readNWISdv(siteNumbers = '02465493', 
                    parameterCd = '00060')

#Tidy data
df_usgs<-df_usgs %>% 
  select(day = Date, 
         wL = X_00060_00003) %>% 
  mutate(wetland_id = "USGS") %>% 
  mutate(day = ymd(day)) %>% 
  as_tibble()

#Add to master df
df <- df %>% bind_rows(., df_usgs)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4.0 Visually identify gaps in data -------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Define sampling events
sampling_events <- c("4/25/2023", "6/6/2023", "2/16/2024", 
                     "8/1/2023", "10/2/2023", "12/4/2023") %>% 
  as_tibble() %>% rename(date = value) %>%  
  mutate(date = mdy(date))

#Clip to period interest
df <- df %>% filter(day >= mdy("4/1/2023")) %>% filter(day <= mdy("3/1/2024"))

#Plot for funzies
df %>% 
  ggplot(aes(x=day, y=wL, group = wetland_id)) + 
  geom_line() +
  facet_wrap(wetland_id~., scales='free') + 
  geom_vline(xintercept = sampling_events$date[1:3], lty=2, alpha = .7, col="steelblue4", lwd=1.1) + 
  geom_vline(xintercept = sampling_events$date[4:6], lty=2, alpha = .7, col="darkorange4", lwd=1.1) + 
  theme_bw()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5.0 Gap fill wetland FP3-1 ---------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Filter to just USGS gage and FP wells
ts <- df %>% 
  filter(wetland_id %in% c("USGS", "FP1-2", "FP2-1", "FP3-1")) %>% 
  pivot_wider(names_from = wetland_id, 
              values_from = wL) %>% 
  rename(
    FP1 = `FP1-2`,
    FP2 = `FP2-1`, 
    FP3 = `FP3-1`)

# Complete correlation analysis
ts %>% 
  select(FP1, FP2, FP3, USGS) %>% 
  cor(use = "complete.obs") %>% 
  round(3)

#FP2 and FP3 have the strongest corellation
ggplot(ts, aes(x = FP2, y = FP3)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  theme_bw() +
  labs(title = "FP2 vs FP3 Water Levels",
       x = "FP2 Water Level",
       y = "FP3 Water Level")

# First, create the linear model using complete cases
lm_model <- lm(FP3 ~ FP2, data = ts, na.action = na.exclude)

# Then use the model to predict missing FP3 values
ts <- ts %>%
  mutate(FP3_filled = ifelse(is.na(FP3), 
                             predict(lm_model, newdata = .) + 25,
                             FP3))

# Check the model summary
summary(lm_model)

# Visualize original vs filled data
ts_original <- ts %>% filter(!is.na(FP3))
ts_filled <- ts %>% mutate(FP3_gaps_only = ifelse(is.na(FP3), FP3_filled, NA))

#Plot
ggplot(ts, aes(x = day)) +
  geom_line(aes(y = FP3_filled), color = "red", alpha = 0.7, linewidth = 0.75, lty=2) +
  geom_line(aes(y = FP3), color = "black", lwd=1.15) +
  theme_bw() +
  labs(title = "FP3 Time Series with Gap-filled Data",
       x = "Date",
       y = "FP3 Water Level",
       subtitle = "Black: Original data, Red: Complete series with gap-fill")

#Wrangle data into format to merge with master
ts <- ts %>% 
  select(
    day, 
    wL = FP3_filled) %>% 
  mutate(wetland_id = 'FP3-1')

#Insert into master
df <- df %>% 
  filter(wetland_id != 'FP3-1') %>% 
  bind_rows(., ts)

#Plot for funzies
df %>% 
  ggplot(aes(x=day, y=wL, group = wetland_id)) + 
  geom_line() +
  facet_wrap(wetland_id~., scales='free') + 
  geom_vline(xintercept = sampling_events$date[1:3], lty=2, alpha = .7, col="steelblue4", lwd=1.1) + 
  geom_vline(xintercept = sampling_events$date[4:6], lty=2, alpha = .7, col="darkorange4", lwd=1.1) + 
  theme_bw()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 6.0 Gap fill wetland IT3-1 ---------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Filter to just USGS gage and FP wells
ts <- df %>% 
  filter(wetland_id %in% c("USGS", "IT1-6", "IT3-1", "TBS26")) %>% 
  pivot_wider(names_from = wetland_id, 
              values_from = wL) %>% 
  rename(
    IT1 = "IT1-6",
    IT2 = "TBS26",
    IT3 = "IT3-1")

#Moving average fill
# Create LOESS model between IT2 and IT3
loess_model <- loess(IT3 ~ IT2, data = ts, na.action = na.exclude, span = 0.75)

# Fill gaps in IT3 using LOESS predictions from IT2
ts <- ts %>%
  mutate(IT3_filled = ifelse(is.na(IT3), 
                             predict(loess_model, newdata = .),
                             IT3))

# Plot the relationship with LOESS curve
ggplot(ts, aes(x = IT2, y = IT3)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", span = 0.75, se = TRUE) +
  theme_bw() +
  labs(title = "IT2 vs IT3 with LOESS fit")

# Plot ts
ggplot(ts, aes(x = day)) +
  geom_line(aes(y = IT3_filled), color = "red", alpha = 0.7, linewidth = 1) +
  geom_line(aes(y = IT3), color = "black") +
  theme_bw() +
  labs(title = "IT3 Time Series with LOESS Gap-fill",
       x = "Date",
       y = "IT3 Water Level",
       subtitle = "Black: Original data, Red: Complete series with LOESS gap-fill")

#Wrangle data into format to merge with master
ts <- ts %>% 
  select(
    day, 
    wL = IT3_filled) %>% 
  mutate(wetland_id = 'IT3-1')

#Insert into master
df <- df %>% 
  filter(wetland_id != 'IT3-1') %>% 
  bind_rows(., ts)

#Plot for funzies
df %>% 
  ggplot(aes(x=day, y=wL, group = wetland_id)) + 
  geom_line() +
  facet_wrap(wetland_id~., scales='free') + 
  geom_vline(xintercept = sampling_events$date[1:3], lty=2, alpha = .7, col="steelblue4", lwd=1.1) + 
  geom_vline(xintercept = sampling_events$date[4:6], lty=2, alpha = .7, col="darkorange4", lwd=1.1) + 
  theme_bw()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#7.0 Export dataframe for metric analysis --------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write_csv(df, 'data/waterLevel.csv')
