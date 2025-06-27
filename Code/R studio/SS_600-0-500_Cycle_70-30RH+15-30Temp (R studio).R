
##                        ##
## ICOS - CO2 dye sensor  ##
##  Test Data Analysis    ##
##                        ##

## in side the licor. The fiber optic is connected to a multispec 
## for reading out the signal.
##
## data analyse of baseline tests for humidity and temperature analyse


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



# 2025-04-24: CO2 cycles under humidities 70% and 30% ----

## I'm testing changes in sensor hysteresis to co2 up/down cycles
## The dye was applied directly on the tip of the fiber optic cable
## This measuremtn has as end goal to create a base line test

## I. Load Licor CO2_r data ----
co2Cycle20250424 <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/LicorDataCSV/2025-04-24-1437_ICOS_CO2cycle_basetest_licor+tape+directApply+multispec.csv",
                             fileEncoding="latin1", sep = ";", dec = ".", skip = 13) %>% .[-1,] %>% 
  mutate(SamplingDate = as.Date("2025-04-24"),
         Protocol = "SS_600-0-500_Cycle_30-70RH",
         elapsed = as.numeric(sub(",", ".", as.character(elapsed), fixed = TRUE))/60+2, #added +2 to match python data
         # substituding , for . when going from character to numeric.
         CO2_r = as.numeric(sub(",", ".", as.character(CO2_r), fixed = TRUE)),
         CO2_r = ifelse(CO2_r < 0, 0, CO2_r %>% round),
         RHcham = as.numeric(sub(",", ".", as.character(RHcham), fixed = TRUE)),
         RH_f = round(RHcham, digits = 0) %>% as.factor(),
         
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



co2Cycle20250424 %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = CO2_r, col = RH_f)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "CO2_r (umol/min)") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))


co2Cycle20250424 %>% head()

## II. Load python data ----
## this data is measured with the multispec recorded by python code AM- CO2 dye 5-kiek
## Here files are in different folders for each protocol.-->???


##redo/loop for every measurment##
#loads python data
python.data20250424 <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-04-24-1437_ICOS_CO2cycle_basetest_licor+tape+directApply+multispec.csv",
                                sep = ";") 

# Rename PulseNumber to obs for alignment
python.data20250424 <- python.data20250424 %>%
  rename(obs = PulseNumber)

#add metadata
python.data20250424 <- python.data20250424 %>% mutate(
  SamplingDate = as.Date("2025-04-24"),
  Protocol = "SS_600-0-500_Cycle_30-70RH",  
  DyeType = "Purple",
  ApplicationMethod = "directApply+tape",
  Filename = basename("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-04-24-1437_ICOS_CO2cycle_basetest_licor+tape+directApply+multispec.csv")
)

# python.data20250424 %>% head()
##end of one measurment##


python.data20250424 %>% 
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_point() +
  geom_line()


## III. Merge Licor and python data ----
## need to do the pairing by time
## Create PairingID -> Need to create a PairingID column that would stick to the LR1 and licor data based on the .Hour

## __ i. data pairing ----

# licor data time pairing
licor.time20250424 <- co2Cycle20250424 %>%
  select(SamplingDate, Protocol, obs, elapsed) %>%
  arrange(obs)

# licor.time20250424 %>% head()

# python data time pairing
python.time20250424 <- python.data20250424 %>%
  arrange(obs) %>%
  select(SamplingDate, Protocol, obs, Time..min.,)

# python.min20250424 %>%  head()


# Join licor_data and python.data by the common 'obs' column
TimePaired.data20250424 <- licor.time20250424 %>%
  inner_join(python.time20250424, by = "obs") %>%
  arrange(obs)  

# TimePaired.data20250424 %>% head()




## Visualize Pairing of measurements
rbind(
  licor.time20250424 %>% 
    mutate(Instrument = "Licor") %>% dplyr::rename(Hour = elapsed),
  python.time20250424 %>% 
    mutate(Instrument = "python") %>% dplyr::rename(Hour = Time..min.)
) %>% as.data.frame() %>% 
  ggplot(aes(x = Hour, y = 1)) + 
  facet_wrap(Protocol ~., scales = "free_x", ncol = 1) +
  geom_line() +
  geom_point(size = 3, aes(, col = Instrument)) 


TimePaired.data20250424 %>% summarise(across(c(elapsed, Time..min.), ~sum(is.na(.))))
## changed licor.hour to elapsed



## __ i. Merge ----
merged.df20250424 <- left_join(python.data20250424, co2Cycle20250424)

merged.df20250424 %>% head() %>% select(DyeType, Protocol, SamplingDate, Licor.Hour, elapsed, Time..min., obs, Reflectance_590, CO2_r, RHcham, RH_f, Tair, Tair_f, Qin, Flow)



merged.df20250424 %>% 
  ggplot(aes(x = elapsed, y = Time..min., col = Protocol)) +
  geom_abline(slope = 1, intercept =  0) + geom_point(size = 3) +  theme_bw()

merged.df20250424 %>% with(table(Reflectance_590, CO2_r, RH_f))


## IV. Data analysis ----

## __ iv) estimate hysteresis
merged.df20250424 %>%
  ggplot(aes(x = CO2_r, y = Reflectance_590,
             col = RH_f)) +
  facet_grid( ~ RH_f) + #, scales = "free_y"
  geom_point(size = 3) + #geom_line(aes(group = Cycle), lwd = 1) +
  geom_path() +
  theme_bw() + theme(panel.grid = element_blank())



## colour gradiant
merged.df20250424 %>%   
  filter(RH_f %in% c(70, 30)) %>% 
  mutate(RH_f = factor(RH_f, levels = c(70, 30))) %>%  
  ggplot(aes(x = obs, y = Reflectance_590)) +
  scale_color_gradient(low = "darkorchid4", high = "thistle1") +
  
  #  facet_grid(~RH_f, scales = "free_x") +
  geom_line() +
  geom_point(aes(col = CO2_r), size = 3) + 
  theme_bw() + theme(panel.grid = element_blank()) +
  
  geom_step(data = . %>% filter(!is.na(CO2_r)), direction = "vh",
            aes(y = CO2_r*1 + 13000), col = "slategray") +
  geom_point(data = . %>% filter(!is.na(CO2_r)), 
             aes(y = CO2_r*1 + 13000), col = "slategray", size = 0.5) +
  
  geom_step(data = . %>% filter(!is.na(Qin)), direction = "vh",
            aes(y = Qin + 13900), col = "goldenrod") +
  geom_point(data = . %>% filter(!is.na(Qin)), 
             aes(y = Qin + 13900), col = "goldenrod", size = 0.5) +
  
  geom_step(data = . %>% filter(!is.na(Flow)), direction = "vh",
            aes(y = Flow + 12000), col = "darkgreen") +
  geom_point(data = . %>% filter(!is.na(Flow)), 
             aes(y = Flow + 12000), col = "darkgreen", size = 0.5) +
  
  geom_step(data = . %>% filter(!is.na(Tair)), direction = "vh",
            aes(y = Tair + 11400), col = "firebrick") +
  geom_point(data = . %>% filter(!is.na(Tair_f)), 
             aes(y = Tair + 11400), col = "firebrick", size = 0.5) +
  
  geom_step(data = . %>% filter(!is.na(RHcham)), direction = "vh",
            aes(y = RHcham*5 + 12400), col = "steelblue") +
  geom_point(data = . %>% filter(!is.na(RH_f)), 
             aes(y = RHcham*5 + 12400), col = "steelblue", size = 0.5) 





merged.df20250424 %>% 
  ggplot(aes(x = Time..min., y = Reflectance_590, col = RH_f)) +
  geom_point() +
  geom_line() +
  geom_step(data = . %>% filter(!is.na(CO2_r)), direction = "vh",
            aes(y = CO2_r*1 + 13000), col = "slategray") +
  geom_point(data = . %>% filter(!is.na(CO2_r)), 
             aes(y = CO2_r*1 + 13000), col = "slategray", size = 0.5) +
  
  geom_step(data = . %>% filter(!is.na(Qin)), direction = "vh",
            aes(y = Qin + 13900), col = "goldenrod") +
  geom_point(data = . %>% filter(!is.na(Qin)), 
             aes(y = Qin + 13900), col = "goldenrod", size = 0.5) +
  
  geom_step(data = . %>% filter(!is.na(Flow)), direction = "vh",
            aes(y = Flow + 12000), col = "darkgreen") +
  geom_point(data = . %>% filter(!is.na(Flow)), 
             aes(y = Flow + 12000), col = "darkgreen", size = 0.5) +
  
  geom_step(data = . %>% filter(!is.na(Tair)), direction = "vh",
            aes(y = Tair + 11400), col = "firebrick") +
  geom_point(data = . %>% filter(!is.na(Tair_f)), 
             aes(y = Tair + 11400), col = "firebrick", size = 0.5) +
  
  geom_step(data = . %>% filter(!is.na(RHcham)), direction = "vh",
            aes(y = RHcham*5 + 12400), col = "steelblue") +
  geom_point(data = . %>% filter(!is.na(RH_f)), 
             aes(y = RHcham*5 + 12400), col = "steelblue", size = 0.5) 




merged.df20250424 %>% 
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_point() +
  geom_line() +
  geom_step(data = . %>% filter(!is.na(CO2_r)), direction = "vh",
            aes(y = CO2_r*1 + 13000), col = "slategray") +
  geom_point(data = . %>% filter(!is.na(CO2_r)), 
             aes(y = CO2_r*1 + 13000), col = "slategray", size = 0.5) +
  
  geom_step(data = . %>% filter(!is.na(Qin)), direction = "vh",
            aes(y = Qin + 13900), col = "goldenrod") +
  geom_point(data = . %>% filter(!is.na(Qin)), 
             aes(y = Qin + 13900), col = "goldenrod", size = 0.5) +
  
  geom_step(data = . %>% filter(!is.na(Flow)), direction = "vh",
            aes(y = Flow + 12000), col = "darkgreen") +
  geom_point(data = . %>% filter(!is.na(Flow)), 
             aes(y = Flow + 12000), col = "darkgreen", size = 0.5) +
  
  geom_step(data = . %>% filter(!is.na(Tair)), direction = "vh",
            aes(y = Tair + 11400), col = "firebrick") +
  geom_point(data = . %>% filter(!is.na(Tair_f)), 
             aes(y = Tair + 11400), col = "firebrick", size = 0.5) +
  
  geom_step(data = . %>% filter(!is.na(RHcham)), direction = "vh",
            aes(y = RHcham*5 + 12400), col = "steelblue") +
  geom_point(data = . %>% filter(!is.na(RH_f)), 
             aes(y = RHcham*5 + 12400), col = "steelblue", size = 0.5) 










# 2025-05-06: temperature changes | CO2 400 ----

## I'm testing changes brought by temperature change
## The dye was applied directly on the tip of the fiber optic cable

## I. Load Licor CO2_r data ----
co2Cycle.20250506.TempCha.400co2 <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/LicorDataCSV/2025-05-06-1646_ICOS_tempChange+Oscillations_licor+tape+directApply+multispec.csv",
                                             fileEncoding="latin1", sep = ";", dec = ".", skip = 13) %>% .[-1,] %>% 
  mutate(SamplingDate = as.Date("2025-05-06"),
         Protocol = "SS_600-0-500_Cycle_15-30Temp",
         elapsed = as.numeric(sub(",", ".", as.character(elapsed), fixed = TRUE))/60+2, #added +2 to match python data
         # substituding , for . when going from character to numeric.
         CO2_r = as.numeric(sub(",", ".", as.character(CO2_r), fixed = TRUE)),
         CO2_r = ifelse(CO2_r < 0, 0, CO2_r %>% round),
         RHcham = as.numeric(sub(",", ".", as.character(RHcham), fixed = TRUE)),
         RH_f = round(RHcham, digits = 0) %>% as.factor(),
         Tair = as.numeric(sub(",", ".", as.character(Tair), fixed = TRUE)),
         Tair_f = round(Tair, digits = 0) %>% as.factor(),
         Licor.Hour = hhmmss %>% myHour.f()) %>% 
  select(SamplingDate, Protocol, Licor.Hour, hhmmss, elapsed, obs, CO2_r, RHcham, RH_f, Tair, Tair_f) 

co2Cycle.20250506.TempCha.400co2 %>% tail
mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = RHcham, col = CO2_r)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "RHcham") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))

co2Cycle.20250506.TempCha.400co2 %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = Tair)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "Tair (degrees)") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))


co2Cycle.20250506.TempCha.400co2 %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = CO2_r, col = Tair_f)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "CO2_r (umol/min)") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))



# co2Cycle.20250506.TempCha.400co2 %>% select(obs, Tair)


## II. Load python data ----## II. Load python CO2_rdata ----
## this data is measured with the multispec recorded by python code AM- CO2 dye 5-kiek
## Here files are in different folders for each protocol.-->???


##redo/loop for every measurment##
#loads python data
python.data.20250506.TempCha.400co2 <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-06-1646_ICOS_tempChange+Oscillations_licor+tape+directApply+multispec.csv",
                                                sep = ",") 

# Rename PulseNumber to obs for alignment
python.data.20250506.TempCha.400co2 <- python.data.20250506.TempCha.400co2 %>%
  rename(obs = PulseNumber)

#add metadata
python.data.20250506.TempCha.400co2 <- python.data.20250506.TempCha.400co2 %>% mutate(
  
  
  # python.data.20250506.TempCha.400co2 %>% head()
  ##end of one measurment##
  
  
  python.data.20250506.TempCha.400co2 %>% 
    ggplot(aes(x = Time..min., y = Reflectance_590)) +
    geom_point() +
    geom_line()
  
  
  ## III. Merge Licor and python data ----
  ## need to do the pairing by time
  ## Create PairingID -> Need to create a PairingID column that would stick to the LR1 and licor data based on the .Hour
  
  ## __ i. data pairing ----
  
  # licor data time pairing
  licor.time.20250506.TempCha.400co2 <- co2Cycle.20250506.TempCha.400co2 %>%
    select(SamplingDate, Protocol, obs, elapsed) %>%
    arrange(obs)
  
  # licor.time.20250506.TempCha.400co2 %>% head()
  
  # python data time pairing
  python.time.20250506.TempCha.400co2 <- python.data.20250506.TempCha.400co2 %>%
    arrange(obs) %>%
    select(SamplingDate, Protocol, obs, Time..min.,)
  
  # python.min.20250506.TempCha.400co2 %>%  head()
  
  
  # Join licor_data and python.data by the common 'obs' column
  TimePaired.data.20250506.TempCha.400co2 <- licor.time.20250506.TempCha.400co2 %>%
    inner_join(python.time.20250506.TempCha.400co2, by = "obs") %>%
    arrange(obs)  
  
  # TimePaired.data.20250506.TempCha.400co2 %>% head()
  
  
  
  
  ## Visualize Pairing of measurements
  rbind(
    licor.time.20250506.TempCha.400co2 %>% 
      mutate(Instrument = "Licor") %>% dplyr::rename(Hour = elapsed),
    python.time.20250506.TempCha.400co2 %>% 
      mutate(Instrument = "python") %>% dplyr::rename(Hour = Time..min.)
  ) %>% as.data.frame() %>% 
    ggplot(aes(x = Hour, y = 1)) + 
    facet_wrap(Protocol ~., scales = "free_x", ncol = 1) +
    geom_line() +
    geom_point(size = 3, aes(, col = Instrument)) 
  
  
  TimePaired.data.20250506.TempCha.400co2 %>% summarise(across(c(elapsed, Time..min.), ~sum(is.na(.))))
  ## changed licor.hour to elapsed
  
  
  
  ## __ i. Merge ----
  merged.df.20250506.TempCha.400co2 <- left_join(python.data.20250506.TempCha.400co2, co2Cycle.20250506.TempCha.400co2)
  
  merged.df.20250506.TempCha.400co2 %>% head() %>% select(DyeType, Protocol, SamplingDate, Licor.Hour,RH_f, CO2_r, elapsed, Time..min., obs, Reflectance_590, RHcham)
  
  
  merged.df.20250506.TempCha.400co2 %>% 
    ggplot(aes(x = elapsed, y = Time..min., col = Protocol)) +
    geom_abline(slope = 1, intercept =  0) + geom_point(size = 3) +  theme_bw()
  
  merged.df.20250506.TempCha.400co2 %>% with(table(Reflectance_590, Tair, RH_f))
  
  
  ## IV. Data analysis ----
  
  ## __ iv) estimate hysteresis
  merged.df.20250506.TempCha.400co2 %>%
    ggplot(aes(x = CO2_r, y = Reflectance_590)) +
    # facet_grid( ~ RH_f) + #, scales = "free_y"
    geom_point(size = 3) + #geom_line(aes(group = Cycle), lwd = 1) +
    geom_path() +
    theme_bw() + theme(panel.grid = element_blank())
  
  
  
  ## colour gradiant
  merged.df.20250506.TempCha.400co2 %>% 
    filter(Tair_f %in% c(15, 30)) %>% 
    ggplot(aes(x = obs, y = Reflectance_590)) +
    scale_color_gradient(low = "darkorchid4", high = "thistle1") +
    
    facet_grid(~Tair_f, scales = "free_x") +
    geom_line() +
    geom_point(aes(col = CO2_r), size = 3) + 
    theme_bw() + theme(panel.grid = element_blank()) 
  
  
  
  
  