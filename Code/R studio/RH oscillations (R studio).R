##                        ##
## ICOS - CO2 dye sensor  ##
##  Test Data Analysis    ##
##                        ##

## This file contains all data collected oscillating environmental parameter humidity keeping all other parameters including CO2 constant


rm(list = ls())

## My hhmmss to hour decimal. 
myHour.f <- function(hhmmss) {
  hhmmss %>% as.character %>% mySplit.f(1, ":") %>% as.numeric +
    hhmmss %>% as.character %>% mySplit.f(2, ":") %>% as.numeric/60 +
    hhmmss %>% as.character %>% mySplit.f(3, ":") %>% as.numeric/60/60
}
# My split function
mySplit.f <- function(names_from, values_from, split, ...){
  strsplit(names_from, split) %>% do.call(rbind, .) %>% .[, values_from]
}

require(dplyr)
require(ggplot2)



# 2025-05-08: humidity oscilations | CO2 400 ----

## I'm testing changes brought by repeded huminity oscilations
## The dye was applied directly on the tip of the fiber optic cable
## second try for test on 2025-05-06

## I. Load Licor CO2_r data ----
co2Cycle.20250506.RHosc.400co2 <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/LicorDataCSV/2025-05-08-1707_ICOS_RHoscillations70-0-30_licor+tape+directApply+multispec.csv",
                                           fileEncoding="latin1", sep = ";", dec = ".", skip = 13) %>% .[-1,] %>% 
  mutate(SamplingDate = as.Date("2025-05-08"),
         Protocol = "humidity oscilations at CO2 400",
         elapsed = as.numeric(sub(",", ".", as.character(elapsed), fixed = TRUE))/60+2.008333, #added +2 to match python data
         # substituding , for . when going from character to numeric.
        CO2_r = as.numeric(sub(",", ".", as.character(CO2_r), fixed = TRUE)),
        CO2_r = ifelse(CO2_r < 0, 0, CO2_r %>% round),
        Qin = as.numeric(sub(",", ".", as.character(Qin), fixed = TRUE)),
        Flow = as.numeric(sub(",", ".", as.character(Qin), fixed = TRUE)),
        Fan_speed = as.numeric(sub(",", ".", as.character(Qin), fixed = TRUE)),
        RHcham = as.numeric(sub(",", ".", as.character(RHcham), fixed = TRUE)),
        RH_f = round(RHcham, digits = 0) %>% as.factor(),
        Tair = as.numeric(sub(",", ".", as.character(Tair), fixed = TRUE)),
        Tair_f = round(Tair, digits = 0) %>% as.factor(),
        shareTime = round(elapsed, digits = 0),
        Licor.Hour = hhmmss %>% myHour.f()) %>% 
    select(SamplingDate, Protocol, Licor.Hour, hhmmss, elapsed, obs, CO2_r, RHcham, RH_f, Tair, Tair_f, Qin, Flow, Fan_speed, shareTime) 






co2Cycle.20250506.RHosc.400co2 %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = RHcham, col = CO2_r)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid", size = 1) +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "Relative Hunidity") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))

co2Cycle.20250506.RHosc.400co2 %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = CO2_r, col = RHcham)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "CO2_r (umol/min)") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))



# co2Cycle.20250506.RHosc.400co2 %>% select(obs, CO2_r)


## II. Load python data ----
## this data is measured with the multispec recorded by python code AM- CO2 dye 5-kiek
## Here files are in different folders for each protocol.-->???


##redo/loop for every measurment##
#loads python data
python.data.20250506.RHosc.400co2 <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-08-1707_ICOS_RHoscillations_licor+tape+directApply+multispec.csv",
                                              sep = ",") 

# Rename PulseNumber to obs for alignment
python.data.20250506.RHosc.400co2 <- python.data.20250506.RHosc.400co2 %>%
  rename(obs = PulseNumber)

#add metadata
python.data.20250506.RHosc.400co2 <- python.data.20250506.RHosc.400co2 %>% mutate(
  SamplingDate = as.Date("2025-05-08"),
  Protocol = "humidity oscilations at CO2 400",  
  DyeType = "Purple",
  ApplicationMethod = "directApply+tape",
  Filename = basename("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-08-1707_ICOS_RHoscillations_licor+tape+directApply+multispec.csv")
)

# python.data.20250506.RHosc.400co2 %>% head()
##end of one measurment##


python.data.20250506.RHosc.400co2 %>% 
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_point() +
  geom_line()



## III. Merge Licor and python data ----
## need to do the pairing by time
## Create PairingID -> Need to create a PairingID column that would stick to the LR1 and licor data based on the .Hour

## __ i. data pairing ----

# licor data time pairing
licor.time.20250506.RHosc.400co2 <- co2Cycle.20250506.RHosc.400co2 %>%
  select(SamplingDate, Protocol, obs, elapsed) %>%
  arrange(obs)

# licor.time.20250506.RHosc.400co2 %>% head()

# python data time pairing
python.time.20250506.RHosc.400co2 <- python.data.20250506.RHosc.400co2 %>%
  arrange(obs) %>%
  select(SamplingDate, Protocol, obs, Time..min.,)

# python.min.20250506.RHosc.400co2 %>%  head()


# Join licor_data and python.data by the common 'obs' column
TimePaired.data.20250506.RHosc.400co2 <- licor.time.20250506.RHosc.400co2 %>%
  inner_join(python.time.20250506.RHosc.400co2, by = "obs") %>%
  arrange(obs)  

# TimePaired.data.20250506.RHosc.400co2 %>% head()




## Visualize Pairing of measurements
rbind(
  licor.time.20250506.RHosc.400co2 %>% 
    mutate(Instrument = "Licor") %>% dplyr::rename(Hour = elapsed),
  python.time.20250506.RHosc.400co2 %>% 
    mutate(Instrument = "python") %>% dplyr::rename(Hour = Time..min.)
) %>% as.data.frame() %>% 
  ggplot(aes(x = Hour, y = 1)) + 
  facet_wrap(Protocol ~., scales = "free_x", ncol = 1) +
  geom_line() +
  geom_point(size = 3, aes(, col = Instrument)) 


TimePaired.data.20250506.RHosc.400co2 %>% summarise(across(c(elapsed, Time..min.), ~sum(is.na(.))))
## changed licor.hour to elapsed



## __ i. Merge ----
merged.df.20250506.RHosc.400co2 <- left_join(python.data.20250506.RHosc.400co2, co2Cycle.20250506.RHosc.400co2)

merged.df.20250506.RHosc.400co2 %>% head() %>% select(DyeType, Protocol, SamplingDate, Licor.Hour, elapsed, Time..min., obs, Reflectance_590, CO2_r, RHcham, RH_f, Tair, Tair_f, Qin, Flow)


# merged.df.20250506.RHosc.400co2 %>% tail()

merged.df.20250506.RHosc.400co2 %>% 
  ggplot(aes(x = elapsed, y = Time..min., col = Protocol)) +
  geom_abline(slope = 1, intercept =  0) + geom_point(size = 3) +  theme_bw()

merged.df.20250506.RHosc.400co2 %>% with(table(Reflectance_590, CO2_r, RH_f))


## IV. Data analysis ----

## __ iv) estimate hysteresis
merged.df.20250506.RHosc.400co2 %>%
  ggplot(aes(x = RHcham, y = Reflectance_590)) +
  # facet_grid( ~ RH_f) + #, scales = "free_y"
  geom_point(size = 3) + #geom_line(aes(group = Cycle), lwd = 1) +
  geom_path() +
  theme_bw() + theme(panel.grid = element_blank())



## colour gradiant


merged.df.20250506.RHosc.400co2 %>%
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_line() +
  geom_point(aes(col = RHcham), size = 3) +
  scale_color_gradient(low = "lightblue", high = "darkblue")+
    scale_y_continuous(name = "Reflectance (590 nm)", limits = c(9250, 10250)) +
    scale_x_continuous(name = "Time (min)") 





  geom_point(data = . %>% filter(!is.na(Tair)),
             aes(col = Tair), size = 3.5) +
  geom_step(data = . %>% filter(!is.na(CO2_r)), direction = "vh",
            aes(y = CO2_r + 10000), col = "slategray") +
  #geom_point(data = . %>% filter(!is.na(CO2_r)), 
  #           aes(y = CO2_r + 10000), col = "slategray", size = 0.5) +
  
  geom_step(data = . %>% filter(!is.na(RHcham)), direction = "vh",
            aes(y = RHcham + 10000), col = "steelblue") +
  #geom_point(data = . %>% filter(!is.na(RH_f)), 
  #           aes(y = RHcham + 10000), col = "steelblue", size = 0.5) 


