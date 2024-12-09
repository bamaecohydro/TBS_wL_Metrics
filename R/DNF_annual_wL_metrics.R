#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Name: DNF Annual Water Level Metrics
#Date: 10/18/2024
#Coder: Nate Jones (natejones@ua.edu)
#PUrpose: Develop wetland water level metrics for Floodpulse DNF/DNRA experiment 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup workspace environment------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#clear memory
remove(list=ls())

#load required libraries
library(tidyverse)
library(readxl)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Create function to download data ------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
download_fun <- function(wetland_id){
  #load data of interest
  df <- read_xlsx('data//waterLevel.xlsx', sheet = wetland_id, skip=5, col_types = c("numeric"))
  
  #list number of cols
  ncols <- ncol(df)
  
  #Create vector for col types
  col_types <- c("date", rep("numeric", ncols-1))
  
  #load data of interest (but control coltypes this time)
  df <- read_xlsx('data//waterLevel.xlsx', sheet = wetland_id, skip=5, col_types = col_types)
  
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Apply function and wrangle data -------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create vector of wetlands of interest
wetlands <- c("HS1-1", "HS2-1", "HS3-1", 
              "IT1-6", "TBS26", "IT3-1", 
              "FP1-2", "FP2-1", "FP3-1")

#Define sampling events
sampling_events <- c("4/25/2023", "6/6/2023", "2/16/2024", 
                     "8/1/2023", "10/2/2023", "12/4/2023") %>% 
  as_tibble() %>% rename(date = value) %>%  
  mutate(date = mdy(date))

#donwload data
df <- lapply(X = wetlands, FUN = download_fun) %>% bind_rows()

#Clip to period interest
df <- df %>% filter(day >= mdy("4/1/2023")) %>% filter(day <= mdy("3/1/2024"))

#Plot for funzies
df %>% 
  ggplot(aes(x=day, y=wL, group = wetland_id)) + 
  geom_line() +
  facet_wrap(wetland_id~., scales='free') + 
  geom_vline(xintercept = sampling_events$date, lty=2, alpha = .7, col="steelblue4", lwd=1.1) + 
  theme_bw()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 Create function to estimate annual metrics --------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#start function
metrics_fun <- function(wetland){

  #Create tibble for analysis
  df <- df %>% filter(wetland_id == wetland)
    
  #average water level
  mean_waterLevel <- mean(df$wL, na.rm=T)
  
  #variation of inundation
  cv_waterLevel <- mean(df$wL, na.rm=T)/sd(df$wL, na.rm=T)
  
  #duration of inundation
  annual_inundation_duration <- df %>% filter(wL>0) %>% nrow()
  
  #Estimate the number of inundation events
  n_events<-df %>% 
    #Identify when waterLevel goes from dry to wet
    mutate(dry_wet_transition = if_else(lag(wL)<=0 & wL>0, 1, 0)) %>% 
    #If waterLevel ts starts "inundated", then add dry_wet_transition
    mutate(dry_wet_transition = if_else(row_number()==1 & wL>0, 1, dry_wet_transition)) %>% 
    #sum number of individual events
    summarise(sum(dry_wet_transition, na.rm=T)) %>% 
    pull()
    
  
  #Estimate the frequency of inundation events
  event_stats<-df %>% 
    #Identify when waterLevel goes from dry to wet
    mutate(dry_wet_transition = if_else(lag(wL)<=0 & wL>0, 1, 0)) %>% 
    #If waterLevel ts starts "inundated", then add dry_wet_transition
    mutate(dry_wet_transition = if_else(row_number()==1 & wL>0, 1, dry_wet_transition)) %>% 
    #define individual events
    mutate(
      inundation_event_id = cumsum(dry_wet_transition),
      inundation_event_id = if_else(wL<=0, 0, inundation_event_id)) %>% 
    #estimate inundation event duration
    group_by(inundation_event_id) %>% 
    summarise(event_inundation_days = n()) %>% 
    filter(inundation_event_id>0) %>% 
    #estimate median inundation event duration
    summarise(
      mean_event_inundation_days = mean(event_inundation_days, na.rm=T), 
      sd_event_inundation_days = sd(event_inundation_days, na.rm=T)) %>% 
    mutate(
      cv_event_inundation_days = sd_event_inundation_days/mean_event_inundation_days)
  
  #export annual_metrics
  annual_metrics <- tibble(
    wetland_id = wetland,
    mean_waterLevel, 
    cv_waterLevel, 
    annual_inundation_duration, 
    n_events, 
    mean_event_inundation_days = event_stats$mean_event_inundation_days,
    cv_event_inundation_days =event_stats$cv_event_inundation_days
  )
  
  #print to global environment
  annual_metrics
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5.0 Apply function to estimate inundation metrics ---------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Apply metrics_fun to wetlands of interest
output <- lapply(X= wetlands, FUN=metrics_fun) %>% bind_rows()
output

#write to folder
write_csv(output, "output//DNF_annual_wL_metrics.csv")
  

