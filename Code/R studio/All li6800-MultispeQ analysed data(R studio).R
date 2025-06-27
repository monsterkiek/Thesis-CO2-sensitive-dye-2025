##                        ##
## ICOS - CO2 dye sensor  ##
##  Test Data Analysis    ##
##                        ##

## In this round the CO2 sensor film is placed on a fiber optic cable
## in side the licor. The fiber optic is connected to a multispec 
## for reading out the signal.
##
## this file containes all analysed data sets regardles of eventual inclution into the report
## some more complex data sets ware further developed in seperate files
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











# 2025-04-25: CO2 cycles under humidities 70% and 30% ----

## I'm testing changes in sensor hysteresis to co2 up/down cycles
## The dye was applied directly on the tip of the fiber optic cable
## This measuremtn has as end goal to create a base line test

## I. Load Licor CO2_r data ----
co2Cycle20250425 <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/LicorDataCSV/2025-04-25-0843_ICOS_CO2cycle_basetest_licor+tape+directApply+multispec.csv",
                             fileEncoding="latin1", sep = ";", dec = ".", skip = 13) %>% .[-1,] %>% 
  mutate(SamplingDate = as.Date("2025-04-25"),
         Protocol = "SS_600-0-500_Cycle_30-70RH",
         elapsed = as.numeric(sub(",", ".", as.character(elapsed), fixed = TRUE))/60+2, #added +2 to match python data
         # substituding , for . when going from character to numeric.
         CO2_r = as.numeric(sub(",", ".", as.character(CO2_r), fixed = TRUE)),
         CO2_r = ifelse(CO2_r < 0, 0, CO2_r %>% round),
         RHcham = as.numeric(sub(",", ".", as.character(RHcham), fixed = TRUE)),
         RH_f = round(RHcham, digits = 0) %>% as.factor(),
         Licor.Hour = hhmmss %>% myHour.f()) %>% 
  select(SamplingDate, Protocol, Licor.Hour, hhmmss, elapsed, obs, CO2_r, RHcham, RH_f, ) %>% 
  filter(RH_f != 74)
# removed first measurment, probably didnt wait till humidity stabelised before start

co2Cycle20250425 %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = CO2_r, col = RH_f)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "CO2_r (umol/min)") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))

co2Cycle20250425 %>% filter(RH_f == 74)


# co2Cycle20250425 %>% head()

## II. Load python data ----
## this data is measured with the multispec recorded by python code AM- CO2 dye 5-kiek

##redo/loop for every measurment##
#loads python data
python.data20250425 <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-04-25-0843_ICOS_CO2cycle_basetest_licor+tape+directApply+multispec.csv",
                                sep = ";") 

# Rename PulseNumber to obs for alignment
python.data20250425 <- python.data20250425 %>%
  rename(obs = PulseNumber)

#add metadata
python.data20250425 <- python.data20250425 %>% mutate(
  SamplingDate = as.Date("2025-04-25"),
  Protocol = "SS_600-0-500_Cycle_30-70RH",  
  DyeType = "Purple",
  ApplicationMethod = "directApply+tape",
  Filename = basename("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-04-25-0843_ICOS_CO2cycle_basetest_licor+tape+directApply+multispec.csv")
  )

# python.data20250425 %>% head()
##end of one measurment##


python.data20250425 %>% 
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_point() +
  geom_line()




## III. Merge Licor and python data ----
## need to do the pairing by time
## Create PairingID -> Need to create a PairingID column that would stick to the LR1 and licor data based on the .Hour

## __ i. data pairing ----

# licor data time pairing
licor.time20250425 <- co2Cycle20250425 %>%
  select(SamplingDate, Protocol, obs, elapsed) %>%
  arrange(obs)

licor.time20250425 %>% head()

# python data time pairing
python.time20250425 <- python.data20250425 %>%
  arrange(obs) %>%
  select(SamplingDate, Protocol, obs, Time..min.,)

python.time20250425 %>%  head()


# Join licor_data and python.data by the common 'obs' column
TimePaired.data20250425 <- licor.time20250425 %>%
  inner_join(python.time20250425, by = "obs") %>%
  arrange(obs)  

TimePaired.data20250425 %>% head()




## Visualize Pairing of measurements
rbind(
  licor.time20250425 %>% 
    mutate(Instrument = "Licor") %>% dplyr::rename(Hour = elapsed),
  python.time20250425 %>% 
    mutate(Instrument = "python") %>% dplyr::rename(Hour = Time..min.)
) %>% as.data.frame() %>% 
  ggplot(aes(x = Hour, y = 1)) + 
  facet_wrap(Protocol ~., scales = "free_x", ncol = 1) +
  geom_line() +
  geom_point(size = 3, aes(, col = Instrument)) 


TimePaired.data20250425 %>% summarise(across(c(elapsed, Time..min.), ~sum(is.na(.))))
## changed licor.hour to elapsed


## __ i. Merge ----
merged.df20250425 <- left_join(python.data20250425, co2Cycle20250425)

merged.df20250425 %>% head() %>% select(DyeType, Protocol, SamplingDate, Licor.Hour,RH_f, CO2_r, elapsed, Time..min., obs, Reflectance_590, RHcham)



merged.df20250425 %>% 
  ggplot(aes(x = elapsed, y = Time..min., col = Protocol)) +
  geom_abline(slope = 1, intercept =  0) + geom_point(size = 3) +  theme_bw()

merged.df20250425 %>% with(table(Reflectance_590, CO2_r, RH_f))


## IV. Data analysis ----

## __ iv) estimate hysteresis
merged.df20250425 %>%
  ggplot(aes(x = CO2_r, y = Reflectance_590,
             col = RH_f)) +
  facet_grid( ~ RH_f) + #, scales = "free_y"
  geom_point(size = 3) + #geom_line(aes(group = Cycle), lwd = 1) +
  geom_path() +
  theme_bw() + theme(panel.grid = element_blank())



## colour gradiant
merged.df20250425 %>% 
  filter(RH_f %in% c(70, 30)) %>% 
  mutate(RH_f = factor(RH_f, levels = c(70, 30))) %>%  
  ggplot(aes(x = obs, y = Reflectance_590)) +
  scale_color_gradient(low = "darkorchid4", high = "thistle1") +
  
  facet_grid(~RH_f, scales = "free_x") +
  geom_line() +
  geom_point(aes(col = CO2_r), size = 3) + 
  theme_bw() + theme(panel.grid = element_blank()) 

merged.df20250425 %>% 
  group_by(RH_f) %>% 
  mutate(newObs = obs - min(obs)) %>% 
  ggplot(aes(x = newObs, y = Reflectance_590, lty = RH_f)) +
  scale_color_gradient(low = "darkorchid4", high = "thistle1") +
  
  geom_line() +
  geom_point(aes(col = CO2_r), size = 4) + 
  theme_bw() + theme(panel.grid = element_blank())







# X CO2 cycles under humidities 70% and 30% bind 2025-04-24/2025-04-25 ----

#merge with other data following the same protocol

## I. bind data sets

## __i. bind_rows licor data ----

licor.dataBound <- bind_rows(co2Cycle20250424, co2Cycle20250425) %>% 
  arrange(obs)

licor.dataBound %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = CO2_r, col = RH_f)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "CO2_r (umol/min)") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))


## __ii. bind_rows python data ----

python.dataBound <- bind_rows(python.data20250424, python.data20250425) %>% 
  arrange(obs)

python.dataBound %>% 
  ggplot(aes(x = Time..min., y = Reflectance_590, group = Filename)) +
  geom_point() +
  geom_line()



## __ iii. Merge ----
merged.dfBoundPL <- left_join(python.dataBound, licor.dataBound)

merged.dfBoundPL %>% head() %>% select(DyeType, Protocol, SamplingDate, Licor.Hour,RH_f, CO2_r, elapsed, Time..min., obs, Reflectance_590, RHcham)



merged.dfBoundPL %>% 
  ggplot(aes(x = elapsed, y = Time..min., col = Protocol)) +
  geom_abline(slope = 1, intercept =  0) + geom_point(size = 3) +  theme_bw()

merged.dfBoundPL %>% with(table(Reflectance_590, CO2_r, RH_f))


## IV. Data analysis ----


## colour gradiant
merged.dfBoundPL %>% 
  ggplot(aes(x = obs, y = Reflectance_590, group = Filename)) +
  scale_color_gradient(low = "darkorchid4", high = "thistle1") +
  
  facet_grid(~RH_f, scales = "free_x") +
  geom_line() +
  geom_point(aes(col = CO2_r), size = 4) + 
  theme_bw() + theme(panel.grid = element_blank()) 

merged.dfBoundPL %>% 
  group_by(RH_f) %>% 
  mutate(newObs = obs - min(obs)) %>% 
  ggplot(aes(x = newObs, y = Reflectance_590, lty = RH_f)) +
  scale_color_gradient(low = "darkorchid4", high = "thistle1") +
  
  geom_line(aes(group = Filename)) +
  geom_point(aes(col = CO2_r), size = 4) + 
  theme_bw() + theme(panel.grid = element_blank())



## for this we need the relative reflectance, that would work better












# 2025-04-29: CO2 cycles under humidity 70% ----

## I'm testing changes in sensor hysteresis to co2 up/down cycles under a specific huminity of 70
## The dye was applied directly on the tip of the fiber optic cable

## I. Load Licor CO2_r data ----
co2Cycle20250429_70 <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/LicorDataCSV/2025-04-29-1340_ICOS_70%+oscilations_licor+tape+directApply+multispec.csv",
                             fileEncoding="latin1", sep = ";", dec = ".", skip = 13) %>% .[-1,] %>% 
  mutate(SamplingDate = as.Date("2025-04-29"),
         Protocol = "SS_600-0-500_Cycle_70RH",
         elapsed = as.numeric(sub(",", ".", as.character(elapsed), fixed = TRUE))/60+2, #added +2 to match python data
         # substituding , for . when going from character to numeric.
         CO2_r = as.numeric(sub(",", ".", as.character(CO2_r), fixed = TRUE)),
         CO2_r = ifelse(CO2_r < 0, 0, CO2_r %>% round),
         RHcham = as.numeric(sub(",", ".", as.character(RHcham), fixed = TRUE)),
         RH_f = round(RHcham, digits = 0) %>% as.factor(),
         Licor.Hour = hhmmss %>% myHour.f()) %>% 
  select(SamplingDate, Protocol, Licor.Hour, hhmmss, elapsed, obs, CO2_r, RHcham, RH_f, ) 

co2Cycle20250429_70 %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = CO2_r, col = RH_f)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_color_manual(values = c("#00BFC4")) + 
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "CO2_r (umol/min)") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))




# co2Cycle20250429_70 %>% head()

## II. Load python data ----
## this data is measured with the multispec recorded by python code AM- CO2 dye 5-kiek
## Here files are in different folders for each protocol.-->???


##redo/loop for every measurment##
#loads python data
python.data20250429_70 <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-04-29-1340_ICOS_70+oscilations_licor+tape+directApply+multispec.csv",
                                sep = ";") 

# Rename PulseNumber to obs for alignment
python.data20250429_70 <- python.data20250429_70 %>%
  rename(obs = PulseNumber)

#add metadata
python.data20250429_70 <- python.data20250429_70 %>% mutate(
  SamplingDate = as.Date("2025-04-29"),
  Protocol = "SS_600-0-500_Cycle_70RH",  
  DyeType = "Purple",
  ApplicationMethod = "directApply+tape",
  Filename = basename("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-04-25-0843_ICOS_CO2cycle_basetest_licor+tape+directApply+multispec.csv")
)

# python.data20250429_70 %>% head()
##end of one measurment##


python.data20250429_70 %>% 
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_point() +
  geom_line()




## III. Merge Licor and python data ----
## need to do the pairing by time
## Create PairingID -> Need to create a PairingID column that would stick to the LR1 and licor data based on the .Hour

## __ i. data pairing ----

# licor data time pairing
licor.time20250429_70 <- co2Cycle20250429_70 %>%
  select(SamplingDate, Protocol, obs, elapsed) %>%
  arrange(obs)

licor.time20250429_70 %>% head()

# python data time pairing
python.time20250429_70 <- python.data20250429_70 %>%
  arrange(obs) %>%
  select(SamplingDate, Protocol, obs, Time..min.,)

python.time20250429_70 %>%  head()


# Join licor_data and python.data by the common 'obs' column
TimePaired.data20250429_70 <- licor.time20250429_70 %>%
  inner_join(python.time20250429_70, by = "obs") %>%
  arrange(obs)  

TimePaired.data20250429_70 %>% head()




## Visualize Pairing of measurements
rbind(
  licor.time20250429_70 %>% 
    mutate(Instrument = "Licor") %>% dplyr::rename(Hour = elapsed),
  python.time20250429_70 %>% 
    mutate(Instrument = "python") %>% dplyr::rename(Hour = Time..min.)
) %>% as.data.frame() %>% 
  ggplot(aes(x = Hour, y = 1)) + 
  facet_wrap(Protocol ~., scales = "free_x", ncol = 1) +
  geom_line() +
  geom_point(size = 3, aes(, col = Instrument)) 


TimePaired.data20250429_70 %>% summarise(across(c(elapsed, Time..min.), ~sum(is.na(.))))
## changed licor.hour to elapsed


## __ i. Merge ----
merged.df20250429_70 <- left_join(python.data20250429_70, co2Cycle20250429_70)

merged.df20250429_70 %>% head() %>% select(DyeType, Protocol, SamplingDate, Licor.Hour,RH_f, CO2_r, elapsed, Time..min., obs, Reflectance_590, RHcham)



merged.df20250429_70 %>% 
  ggplot(aes(x = elapsed, y = Time..min., col = Protocol)) +
  geom_abline(slope = 1, intercept =  0) + geom_point(size = 3) +  theme_bw()

merged.df20250429_70 %>% with(table(Reflectance_590, CO2_r, RH_f))


## IV. Data analysis ----

## __ iv) estimate hysteresis
merged.df20250429_70 %>%
  ggplot(aes(x = CO2_r, y = Reflectance_590,
             col = RH_f)) +
  facet_grid( ~ RH_f) + #, scales = "free_y"
  geom_point(size = 3) + #geom_line(aes(group = Cycle), lwd = 1) +
  geom_path() +
  theme_bw() + theme(panel.grid = element_blank())



## colour gradiant
merged.df20250429_70 %>% 
  ggplot(aes(x = obs, y = Reflectance_590)) +
  scale_color_gradient(low = "darkorchid4", high = "thistle1") +
  
  facet_grid(~RH_f, scales = "free_x") +
  geom_line() +
  geom_point(aes(col = CO2_r), size = 3) + 
  theme_bw() + theme(panel.grid = element_blank()) 







# 2025-04-29: CO2 cycles under humidity 30% ----
## I'm testing changes in sensor hysteresis to co2 up/down cycles under a specific humidity of 30
## The dye was applied directly on the tip of the fiber optic cable

## I. Load Licor CO2_r data ----
co2Cycle20250429_30 <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/LicorDataCSV/2025-04-29-1646_ICOS_30%+oscilations_licor+tape+directApply+multispec.csv",
                             fileEncoding="latin1", sep = ";", dec = ".", skip = 13) %>% .[-1,] %>% 
  mutate(SamplingDate = as.Date("2025-04-29"),
         Protocol = "SS_600-0-500_Cycle_30RH",
         elapsed = as.numeric(sub(",", ".", as.character(elapsed), fixed = TRUE))/60+2, #added +2 to match python data
         # substituding , for . when going from character to numeric.
         CO2_r = as.numeric(sub(",", ".", as.character(CO2_r), fixed = TRUE)),
         CO2_r = ifelse(CO2_r < 0, 0, CO2_r %>% round),
         RHcham = as.numeric(sub(",", ".", as.character(RHcham), fixed = TRUE)),
         RH_f = round(RHcham, digits = 0) %>% as.factor(),
         Licor.Hour = hhmmss %>% myHour.f()) %>% 
  select(SamplingDate, Protocol, Licor.Hour, hhmmss, elapsed, obs, CO2_r, RHcham, RH_f, ) 

co2Cycle20250429_30 %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = CO2_r, col = RH_f)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "CO2_r (umol/min)") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))


# co2Cycle20250429_30 %>% head()

## II. Load python data ----
## this data is measured with the multispec recorded by python code AM- CO2 dye 5-kiek
## Here files are in different folders for each protocol.-->???


##redo/loop for every measurment##
#loads python data
python.data20250429_30 <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-04-29-1646_ICOS_30+oscilations_licor+tape+directApply+multispec.csv",
                                sep = ";") 

# Rename PulseNumber to obs for alignment
python.data20250429_30 <- python.data20250429_30 %>%
  rename(obs = PulseNumber)

#add metadata
python.data20250429_30 <- python.data20250429_30 %>% mutate(
  SamplingDate = as.Date("2025-04-29"),
  Protocol = "SS_600-0-500_Cycle_30RH",  
  DyeType = "Purple",
  ApplicationMethod = "directApply+tape",
  Filename = basename("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-04-29-1646_ICOS_30+oscilations_licor+tape+directApply+multispec.csv")
)

# python.data20250429_30 %>% head()
##end of one measurment##


python.data20250429_30 %>% 
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_point() +
  geom_line()

python.data20250429_30 %>% 
  
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_point() +
  geom_line() +
  geom_smooth(formula = y~x, method = "lm", se = F) 





## III. Merge Licor and python data ----
## need to do the pairing by time
## Create PairingID -> Need to create a PairingID column that would stick to the LR1 and licor data based on the .Hour

## __ i. data pairing ----

# licor data time pairing
licor.time20250429_30 <- co2Cycle20250429_30 %>%
  select(SamplingDate, Protocol, obs, elapsed) %>%
  arrange(obs)

licor.time20250429_30 %>% head()

# python data time pairing
python.time20250429_30 <- python.data20250429_30 %>%
  arrange(obs) %>%
  select(SamplingDate, Protocol, obs, Time..min.,)

python.time20250429_30 %>%  head()


# Join licor_data and python.data by the common 'obs' column
TimePaired.data20250429_30 <- licor.time20250429_30 %>%
  inner_join(python.time20250429_30, by = "obs") %>%
  arrange(obs)  

TimePaired.data20250429_30 %>% head()




## Visualize Pairing of measurements
rbind(
  licor.time20250429_30 %>% 
    mutate(Instrument = "Licor") %>% dplyr::rename(Hour = elapsed),
  python.time20250429_30 %>% 
    mutate(Instrument = "python") %>% dplyr::rename(Hour = Time..min.)
) %>% as.data.frame() %>% 
  ggplot(aes(x = Hour, y = 1)) + 
  facet_wrap(Protocol ~., scales = "free_x", ncol = 1) +
  geom_line() +
  geom_point(size = 3, aes(, col = Instrument)) 


TimePaired.data20250429_30 %>% summarise(across(c(elapsed, Time..min.), ~sum(is.na(.))))
## changed licor.hour to elapsed


## __ i. Merge ----
merged.df20250429_30 <- left_join(python.data20250429_30, co2Cycle20250429_30)

merged.df20250429_30 %>% head() %>% select(DyeType, Protocol, SamplingDate, Licor.Hour,RH_f, CO2_r, elapsed, Time..min., obs, Reflectance_590, RHcham)



merged.df20250429_30 %>% 
  ggplot(aes(x = elapsed, y = Time..min., col = Protocol)) +
  geom_abline(slope = 1, intercept =  0) + geom_point(size = 3) +  theme_bw()

merged.df20250429_30 %>% with(table(Reflectance_590, CO2_r, RH_f))


## IV. Data analysis ----

## __ iv) estimate hysteresis
merged.df20250429_30 %>%
  ggplot(aes(x = CO2_r, y = Reflectance_590,
             col = RH_f)) +
  facet_grid( ~ RH_f) + #, scales = "free_y"
  geom_point(size = 3) + #geom_line(aes(group = Cycle), lwd = 1) +
  geom_path() +
  theme_bw() + theme(panel.grid = element_blank())



## colour gradiant
merged.df20250429_30 %>% 
  ggplot(aes(x = obs, y = Reflectance_590)) +
  scale_color_gradient(low = "darkorchid4", high = "thistle1") +
  
  facet_grid(~RH_f, scales = "free_x") +
  geom_line() +
  geom_point(aes(col = CO2_r), size = 3) + 
  theme_bw() + theme(panel.grid = element_blank()) 









# -->2025-04-30: CO2 cycles | humidities 70% and 30% no breaks ----

## I'm testing changes in sensor hysteresis to co2 up/down cycles
## The dye was applied directly on the tip of the fiber optic cable
## This measuremtn has as end goal to create a base line test

## I. Load Licor CO2_r data ----
co2Cycle20250430 <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/LicorDataCSV/2025-04-30-1155_ICOS_30-70Oscilations_licor+tape+directApply+multispec.csv",
                             fileEncoding="latin1", sep = ";", dec = ".", skip = 13) %>% .[-1,] %>% 
  mutate(SamplingDate = as.Date("2025-04-30"),
         Protocol = "SS_600-0-500_Cycle_30-70RH no breaks",
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




co2Cycle20250430 %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = CO2_r, col = RH_f)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "CO2_r (umol/min)") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))


# co2Cycle20250430 %>% head()

## II. Load python data ----
## this data is measured with the multispec recorded by python code AM- CO2 dye 5-kiek
## Here files are in different folders for each protocol.-->???


##redo/loop for every measurment##
#loads python data
python.data20250430 <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-04-30-1155_ICOS_30-70Oscilations_licor+tape+directApply+multispec.csv",
                                sep = ",") 

# Rename PulseNumber to obs for alignment
python.data20250430 <- python.data20250430 %>%
  rename(obs = PulseNumber)

#add metadata
python.data20250430 <- python.data20250430 %>% mutate(
  SamplingDate = as.Date("2025-04-30"),
  Protocol = "SS_600-0-500_Cycle_30-70RH no breaks",  
  DyeType = "Purple",
  ApplicationMethod = "directApply+tape",
  Filename = basename("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-04-24-1437_ICOS_CO2cycle_basetest_licor+tape+directApply+multispec.csv")
)

# python.data20250430 %>% head()
##end of one measurment##


python.data20250430 %>% 
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_point() +
  geom_line()


## III. Merge Licor and python data ----
## need to do the pairing by time
## Create PairingID -> Need to create a PairingID column that would stick to the LR1 and licor data based on the .Hour

## __ i. data pairing ----

# licor data time pairing
licor.time20250430 <- co2Cycle20250430 %>%
  select(SamplingDate, Protocol, obs, elapsed) %>%
  arrange(obs)

# licor.time20250430 %>% head()

# python data time pairing
python.time20250430 <- python.data20250430 %>%
  arrange(obs) %>%
  select(SamplingDate, Protocol, obs, Time..min.,)

# python.min20250430 %>%  head()


# Join licor_data and python.data by the common 'obs' column
TimePaired.data20250430 <- licor.time20250430 %>%
  inner_join(python.time20250430, by = "obs") %>%
  arrange(obs)  

# TimePaired.data20250430 %>% head()




## Visualize Pairing of measurements
rbind(
  licor.time20250430 %>% 
    mutate(Instrument = "Licor") %>% dplyr::rename(Hour = elapsed),
  python.time20250430 %>% 
    mutate(Instrument = "python") %>% dplyr::rename(Hour = Time..min.)
) %>% as.data.frame() %>% 
  ggplot(aes(x = Hour, y = 1)) + 
  facet_wrap(Protocol ~., scales = "free_x", ncol = 1) +
  geom_line() +
  geom_point(size = 3, aes(, col = Instrument)) 


TimePaired.data20250430 %>% summarise(across(c(elapsed, Time..min.), ~sum(is.na(.))))
## changed licor.hour to elapsed



## __ i. Merge ----
merged.df20250430 <- left_join(python.data20250430, co2Cycle20250430)

merged.df20250424 %>% head() %>% select(DyeType, Protocol, SamplingDate, Licor.Hour, elapsed, Time..min., obs, Reflectance_590, CO2_r, RHcham, RH_f, Tair, Tair_f, Qin, Flow)

merged.df20250430 %>% select(Time..min., elapsed)


merged.df20250430 %>% 
  ggplot(aes(x = elapsed, y = Time..min., col = Protocol)) +
  geom_abline(slope = 1, intercept =  0) + geom_point(size = 3) +  theme_bw()

merged.df20250430 %>% with(table(Reflectance_590, CO2_r, RH_f))


## IV. Data analysis ----

## __ iv) estimate hysteresis
merged.df20250430 %>%
  ggplot(aes(x = CO2_r, y = Reflectance_590,
             col = RH_f)) +
  facet_grid( ~ RH_f) + #, scales = "free_y"
  geom_point(size = 3) + #geom_line(aes(group = Cycle), lwd = 1) +
  geom_path() +
  theme_bw() + theme(panel.grid = element_blank())



## colour gradiant
merged.df20250430 %>% 
  mutate(RH_f = factor(RH_f, levels = c("70", "30"))) %>% 
  ggplot(aes(x = obs, y = Reflectance_590)) +
  scale_color_gradient(low = "darkorchid4", high = "thistle1") +
  
  facet_grid(~RH_f, scales = "free_x") +
  geom_line() +
  geom_point(aes(col = CO2_r), size = 3) + 
  theme_bw() + theme(panel.grid = element_blank()) 

merged.df20250430 %>% 
  group_by(RH_f) %>% 
  mutate(newObs = obs - min(obs)) %>% 
  ggplot(aes(x = newObs, y = Reflectance_590, lty = RH_f)) +
  scale_color_gradient(low = "darkorchid4", high = "thistle1") +
  
  geom_line() +
  geom_point(aes(col = CO2_r), size = 4) + 
  theme_bw() + theme(panel.grid = element_blank())






## colour gradiant
merged.df20250430 %>%   
  filter(RH_f %in% c(70, 30)) %>% 
  mutate(RH_f = factor(RH_f, levels = c(70, 30))) %>%  
  ggplot(aes(x = obs, y = Reflectance_590)) +
  scale_color_gradient(low = "darkorchid4", high = "thistle1") +
  
  #  facet_grid(~RH_f, scales = "free_x") +
  geom_line() +
  geom_point(aes(col = CO2_r), size = 3) + 
  theme_bw() + theme(panel.grid = element_blank())






merged.df20250430 %>% 
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




merged.df20250430 %>% 
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














# X 2025-05-06: humidity oscilations | CO2 400 ----

## I'm testing changes brought by repeded huminity oscilations
## The dye was applied directly on the tip of the fiber optic cable

## there was an issue with the program that resulted in co2 not being constant
## data unusable

## I. Load Licor CO2_r data ----
co2Cycle.20250506.RHosc.400co2 <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/LicorDataCSV/2025-05-06-1008_ICOS_RHoscilations70-0-60_licor+tape+directApply+multispec.csv",
                             fileEncoding="latin1", sep = ";", dec = ".", skip = 13) %>% .[-1,] %>% 
  mutate(SamplingDate = as.Date("2025-05-06"),
         Protocol = "humidity oscilations at CO2 400",
         elapsed = as.numeric(sub(",", ".", as.character(elapsed), fixed = TRUE))/60+2, #added +2 to match python data
         # substituding , for . when going from character to numeric.
         CO2_r = as.numeric(sub(",", ".", as.character(CO2_r), fixed = TRUE)),
         CO2_r = ifelse(CO2_r < 0, 0, CO2_r %>% round),
         RHcham = as.numeric(sub(",", ".", as.character(RHcham), fixed = TRUE)),
         RH_f = round(RHcham, digits = 0) %>% as.factor(),
         Licor.Hour = hhmmss %>% myHour.f()) %>% 
  select(SamplingDate, Protocol, Licor.Hour, hhmmss, elapsed, obs, CO2_r, RHcham, RH_f, ) 

co2Cycle.20250506.RHosc.400co2 %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = RHcham, col = CO2_r)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
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
python.data.20250506.RHosc.400co2 <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-06-1008_ICOS_RHoscilations70-0-60_licor+tape+directApply+multispec.csv",
                                sep = ",") 

# Rename PulseNumber to obs for alignment
python.data.20250506.RHosc.400co2 <- python.data.20250506.RHosc.400co2 %>%
  rename(obs = PulseNumber)

#add metadata
python.data.20250506.RHosc.400co2 <- python.data.20250506.RHosc.400co2 %>% mutate(
  SamplingDate = as.Date("2025-05-06"),
  Protocol = "humidity oscilations at CO2 400",  
  DyeType = "Purple",
  ApplicationMethod = "directApply+tape",
  Filename = basename("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-06-1008_ICOS_RHoscilations70-0-60_licor+tape+directApply+multispec.csv")
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

merged.df.20250506.RHosc.400co2 %>% head() %>% select(DyeType, Protocol, SamplingDate, Licor.Hour,RH_f, CO2_r, elapsed, Time..min., obs, Reflectance_590, RHcham)


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
  ggplot(aes(x = obs, y = Reflectance_590)) +
  scale_color_gradient(low = "darkorchid4", high = "thistle1") +
  
  # facet_grid(~RH_f, scales = "free_x") +
  geom_line() +
  geom_point(aes(col = CO2_r), size = 4) + 
  theme_bw() + theme(panel.grid = element_blank()) 










# -->2025-05-06: temperature changes | CO2 400 ----

## I'm testing changes brought by temperature change
## The dye was applied directly on the tip of the fiber optic cable

## I. Load Licor CO2_r data ----
co2Cycle.20250506.TempCha.400co2 <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/LicorDataCSV/2025-05-06-1646_ICOS_tempChange+Oscillations_licor+tape+directApply+multispec.csv",
                                           fileEncoding="latin1", sep = ";", dec = ".", skip = 13) %>% .[-1,] %>% 
  mutate(SamplingDate = as.Date("2025-05-06"),
         Protocol = "temp change at CO2 400",
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

co2Cycle.20250506.TempCha.400co2 %>% 
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
  SamplingDate = as.Date("2025-05-06"),
  Protocol = "temp change at CO2 400",  
  DyeType = "Purple",
  ApplicationMethod = "directApply+tape",
  Filename = basename("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-06-1646_ICOS_tempChange+Oscillations_licor+tape+directApply+multispec.csv")
)

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




















# -->2025-05-08: humidity oscilations | CO2 400 ----

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
         RHcham = as.numeric(sub(",", ".", as.character(RHcham), fixed = TRUE)),
         RH_f = round(RHcham, digits = 0) %>% as.factor(),
         Licor.Hour = hhmmss %>% myHour.f()) %>% 
  select(SamplingDate, Protocol, Licor.Hour, hhmmss, elapsed, obs, CO2_r, RHcham, RH_f, ) 

co2Cycle.20250506.RHosc.400co2 %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = RHcham, col = CO2_r)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
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

merged.df.20250506.RHosc.400co2 %>% head() %>% select(DyeType, Protocol, SamplingDate, Licor.Hour,RH_f, CO2_r, elapsed, Time..min., obs, Reflectance_590, RHcham)

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
merged.df.20250506.RHosc.400co2 %>% head(15)
  ggplot(aes(x = obs, y = Reflectance_590)) +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  
  # facet_grid(~RH_f, scales = "free_x") +
  geom_line() +
  geom_point(aes(col = RHcham), size = 3) + 
  theme_bw() + theme(panel.grid = element_blank()) 








merged.df.20250506.RHosc.400co2 %>% head(30
                                         )



# 2025-05-12: CO2 cycles | humidities 70% and 30% no breaks ----

## I'm testing changes in sensor hysteresis to co2 up/down cycles
## The dye was applied an on ecoflex octopus with white paper as background
## This measuremtn has as end goal to create to compare to basetest

## I. Load Licor CO2_r data ----
co2Cycle20250512_1030 <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/LicorDataCSV/2025-05-12-1030_ICOS_CO2cycle_basetest_licor+ecoflex+paper+octopus+multispec.csv",
                             fileEncoding="latin1", sep = ";", dec = ".", skip = 13) %>% .[-1,] %>% 
  mutate(SamplingDate = as.Date("2025-05-12"),
         Protocol = "SS_600-0-500_Cycle_30-70RH no breaks",
         elapsed = as.numeric(sub(",", ".", as.character(elapsed), fixed = TRUE))/60+2, #added +2 to match python data
         # substituding , for . when going from character to numeric.
         CO2_r = as.numeric(sub(",", ".", as.character(CO2_r), fixed = TRUE)),
         CO2_r = ifelse(CO2_r < 0, 0, CO2_r %>% round),
         RHcham = as.numeric(sub(",", ".", as.character(RHcham), fixed = TRUE)),
         RH_f = round(RHcham, digits = 0) %>% as.factor(),
         Licor.Hour = hhmmss %>% myHour.f()) %>% 
  select(SamplingDate, Protocol, Licor.Hour, hhmmss, elapsed, obs, CO2_r, RHcham, RH_f, ) 

co2Cycle20250512_1030 %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = CO2_r, col = RH_f)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "CO2_r (umol/min)") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))


# co2Cycle20250512_1030 %>% head()

## II. Load python data ----
## this data is measured with the multispec recorded by python code AM- CO2 dye 5-kiek
## Here files are in different folders for each protocol.-->???


##redo/loop for every measurment##
#loads python data
python.data20250512_1030 <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-12-1030_ICOS_CO2cycles_licor+ecoflex+octopus+multispec.csv",
                                sep = ",") 

# Rename PulseNumber to obs for alignment
python.data20250512_1030 <- python.data20250512_1030 %>%
  rename(obs = PulseNumber)

#add metadata
python.data20250512_1030 <- python.data20250512_1030 %>% mutate(
  SamplingDate = as.Date("2025-05-12"),
  Protocol = "SS_600-0-500_Cycle_30-70RH no breaks",  
  DyeType = "Purple",
  ApplicationMethod = "ecoflex+paper+octopus",
  Filename = basename("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-12-1030_ICOS_CO2cycles_licor+ecoflex+octopus+multispec.csv")
)

# python.data20250512_1030 %>% head()
##end of one measurment##


python.data20250512_1030 %>% 
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_point() +
  geom_line()


## III. Merge Licor and python data ----
## need to do the pairing by time
## Create PairingID -> Need to create a PairingID column that would stick to the LR1 and licor data based on the .Hour

## __ i. data pairing ----

# licor data time pairing
licor.time20250512_1030 <- co2Cycle20250512_1030 %>%
  select(SamplingDate, Protocol, obs, elapsed) %>%
  arrange(obs)

# licor.time20250512_1030 %>% head()

# python data time pairing
python.time20250512_1030 <- python.data20250512_1030 %>%
  arrange(obs) %>%
  select(SamplingDate, Protocol, obs, Time..min.,)

# python.min20250512_1030 %>%  head()


# Join licor_data and python.data by the common 'obs' column
TimePaired.data20250512_1030 <- licor.time20250512_1030 %>%
  inner_join(python.time20250512_1030, by = "obs") %>%
  arrange(obs)  

# TimePaired.data20250512_1030 %>% head()




## Visualize Pairing of measurements
rbind(
  licor.time20250512_1030 %>% 
    mutate(Instrument = "Licor") %>% dplyr::rename(Hour = elapsed),
  python.time20250512_1030 %>% 
    mutate(Instrument = "python") %>% dplyr::rename(Hour = Time..min.)
) %>% as.data.frame() %>% 
  ggplot(aes(x = Hour, y = 1)) + 
  facet_wrap(Protocol ~., scales = "free_x", ncol = 1) +
  geom_line() +
  geom_point(size = 3, aes(, col = Instrument)) 


TimePaired.data20250512_1030 %>% summarise(across(c(elapsed, Time..min.), ~sum(is.na(.))))
## changed licor.hour to elapsed



## __ i. Merge ----
merged.df20250512_1030 <- left_join(python.data20250512_1030, co2Cycle20250512_1030)

merged.df20250512_1030 %>% head() %>% select(DyeType, Protocol, SamplingDate, Licor.Hour,RH_f, CO2_r, elapsed, Time..min., obs, Reflectance_590, RHcham)

merged.df20250512_1030 %>% select(Time..min., elapsed)


merged.df20250512_1030 %>% 
  ggplot(aes(x = elapsed, y = Time..min., col = Protocol)) +
  geom_abline(slope = 1, intercept =  0) + geom_point(size = 3) +  theme_bw()

merged.df20250512_1030 %>% with(table(Reflectance_590, CO2_r, RH_f))


## IV. Data analysis ----

## __ iv) estimate hysteresis
merged.df20250512_1030 %>%
  ggplot(aes(x = CO2_r, y = Reflectance_590,
             col = obs)) +
  facet_grid( ~ RH_f) + #, scales = "free_y"
  geom_point(size = 3) + #geom_line(aes(group = Cycle), lwd = 1) +
  geom_path() +
  theme_bw() + theme(panel.grid = element_blank())



## colour gradiant
merged.df20250512_1030 %>% 
  mutate(RH_f = factor(RH_f, levels = c("70", "30"))) %>% 
  ggplot(aes(x = obs, y = Reflectance_590)) +
  scale_color_gradient(low = "darkorchid4", high = "thistle1") +
  
  facet_grid(~RH_f, scales = "free_x") +
  geom_line() +
  geom_point(aes(col = CO2_r), size = 3) + 
  theme_bw() + theme(panel.grid = element_blank()) 

merged.df20250512_1030 %>% 
  group_by(RH_f) %>% 
  mutate(newObs = obs - min(obs)) %>% 
  ggplot(aes(x = newObs, y = Reflectance_590, lty = RH_f)) +
  scale_color_gradient(low = "darkorchid4", high = "thistle1") +
  
  geom_line() +
  geom_point(aes(col = CO2_r), size = 4) + 
  theme_bw() + theme(panel.grid = element_blank())

























# 2025-05-12: temperature changes | CO2 400 ----

## I'm testing changes brought by temperature change
## The dye was applied on an ecoflex octopus with paper incased in it for reflectance

## I. Load Licor CO2_r data ----
co2Cycle.20250506.Tc.400co2.TempCha.400co2 <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/LicorDataCSV/2025-05-12-1645_ICOS_tempChange+oscilations_licor+ecoflex+octopus+paper+multispec.csv",
                                             fileEncoding="latin1", sep = ";", dec = ".", skip = 13) %>% .[-1,] %>% 
  mutate(SamplingDate = as.Date("2025-05-12"),
         Protocol = "temp change at CO2 400",
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

co2Cycle.20250506.Tc.400co2.TempCha.400co2 %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = RHcham, col = CO2_r)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "RHcham") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))

co2Cycle.20250506.Tc.400co2.TempCha.400co2 %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = Tair)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "Tair (degrees)") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))


co2Cycle.20250506.Tc.400co2.TempCha.400co2 %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = CO2_r, col = Tair_f)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "CO2_r (umol/min)") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))



# co2Cycle.20250506.Tc.400co2.TempCha.400co2 %>% select(obs, Tair)


## II. Load python data ----## II. Load python CO2_rdata ----
## this data is measured with the multispec recorded by python code AM- CO2 dye 5-kiek
## Here files are in different folders for each protocol.-->???


##redo/loop for every measurment##
#loads python data
python.data.20250506.Tc.400co2.TempCha.400co2 <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-12-1645_ICOS_tempChange+oscillations_licor+ecoflex+octopus+paper+multispec.csv",
                                                sep = ",") 

# Rename PulseNumber to obs for alignment
python.data.20250506.Tc.400co2.TempCha.400co2 <- python.data.20250506.Tc.400co2.TempCha.400co2 %>%
  rename(obs = PulseNumber)

#add metadata
python.data.20250506.Tc.400co2.TempCha.400co2 <- python.data.20250506.Tc.400co2.TempCha.400co2 %>% mutate(
  SamplingDate = as.Date("2025-05-12"),
  Protocol = "temp change at CO2 400",  
  DyeType = "Purple",
  ApplicationMethod = "ecoflex+paper+octopus",
  Filename = basename("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-12-1645_ICOS_tempChange+oscillations_licor+ecoflex+octopus+paper+multispec.csv")
)

# python.data.20250506.Tc.400co2.TempCha.400co2 %>% head()
##end of one measurment##


python.data.20250506.Tc.400co2.TempCha.400co2 %>% 
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_point() +
  geom_line()


## III. Merge Licor and python data ----
## need to do the pairing by time
## Create PairingID -> Need to create a PairingID column that would stick to the LR1 and licor data based on the .Hour

## __ i. data pairing ----

# licor data time pairing
licor.time.20250506.Tc.400co2.TempCha.400co2 <- co2Cycle.20250506.Tc.400co2.TempCha.400co2 %>%
  select(SamplingDate, Protocol, obs, elapsed) %>%
  arrange(obs)

# licor.time.20250506.Tc.400co2.TempCha.400co2 %>% head()

# python data time pairing
python.time.20250506.Tc.400co2.TempCha.400co2 <- python.data.20250506.Tc.400co2.TempCha.400co2 %>%
  arrange(obs) %>%
  select(SamplingDate, Protocol, obs, Time..min.,)

# python.min.20250506.Tc.400co2.TempCha.400co2 %>%  head()


# Join licor_data and python.data by the common 'obs' column
TimePaired.data.20250506.Tc.400co2.TempCha.400co2 <- licor.time.20250506.Tc.400co2.TempCha.400co2 %>%
  inner_join(python.time.20250506.Tc.400co2.TempCha.400co2, by = "obs") %>%
  arrange(obs)  

# TimePaired.data.20250506.Tc.400co2.TempCha.400co2 %>% head()




## Visualize Pairing of measurements
rbind(
  licor.time.20250506.Tc.400co2.TempCha.400co2 %>% 
    mutate(Instrument = "Licor") %>% dplyr::rename(Hour = elapsed),
  python.time.20250506.Tc.400co2.TempCha.400co2 %>% 
    mutate(Instrument = "python") %>% dplyr::rename(Hour = Time..min.)
) %>% as.data.frame() %>% 
  ggplot(aes(x = Hour, y = 1)) + 
  facet_wrap(Protocol ~., scales = "free_x", ncol = 1) +
  geom_line() +
  geom_point(size = 3, aes(, col = Instrument)) 


TimePaired.data.20250506.Tc.400co2.TempCha.400co2 %>% summarise(across(c(elapsed, Time..min.), ~sum(is.na(.))))
## changed licor.hour to elapsed



## __ i. Merge ----
merged.df.20250506.Tc.400co2.TempCha.400co2 <- left_join(python.data.20250506.Tc.400co2.TempCha.400co2, co2Cycle.20250506.Tc.400co2.TempCha.400co2)

merged.df.20250506.Tc.400co2.TempCha.400co2 %>% head() %>% select(DyeType, Protocol, SamplingDate, Licor.Hour,RH_f, CO2_r, elapsed, Time..min., obs, Reflectance_590, RHcham)


merged.df.20250506.Tc.400co2.TempCha.400co2 %>% 
  ggplot(aes(x = elapsed, y = Time..min., col = Protocol)) +
  geom_abline(slope = 1, intercept =  0) + geom_point(size = 3) +  theme_bw()

merged.df.20250506.Tc.400co2.TempCha.400co2 %>% with(table(Reflectance_590, Tair, RH_f))


## IV. Data analysis ----

## __ iv) estimate hysteresis
merged.df.20250506.Tc.400co2.TempCha.400co2 %>%
  ggplot(aes(x = CO2_r, y = Reflectance_590)) +
  # facet_grid( ~ RH_f) + #, scales = "free_y"
  geom_point(size = 3) + #geom_line(aes(group = Cycle), lwd = 1) +
  geom_path() +
  theme_bw() + theme(panel.grid = element_blank())



## colour gradiant
merged.df.20250506.Tc.400co2.TempCha.400co2 %>% 
  filter(Tair_f %in% c(15, 30)) %>% 
  ggplot(aes(x = obs, y = Reflectance_590)) +
  scale_color_gradient(low = "darkorchid4", high = "thistle1") +
  
  facet_grid(~Tair_f, scales = "free_x") +
  geom_line() +
  geom_point(aes(col = CO2_r), size = 3) + 
  theme_bw() + theme(panel.grid = element_blank()) 









# -->2025-05-20: (blue dye) CO2 cycles | humidities 70% and 30% no breaks ----

## I'm testing changes in sensor hysteresis to co2 up/down cycles
## The dye was applied an on PDMS octopus with white ABS as background
## measurment was preformed with new blue dye

## I. Load Licor CO2_r data ----
# no licor data due to corrupted files

## II. Load python data ----
## this data is measured with the multispec recorded by python code AM- CO2 dye 5-kiek

#loads python data
python.data20250520_Blue <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-20-1721_ICOS_30-70oscillations_blueDye_licor+PDMS+ABS+octopus+multispec.csv",
                                     sep = ",") 

# Rename PulseNumber to obs for alignment
python.data20250520_Blue <- python.data20250520_Blue %>%
  rename(obs = PulseNumber)

#add metadata
python.data20250520_Blue <- python.data20250520_Blue %>% mutate(
  SamplingDate = as.Date("2025-05-20"),
  Protocol = "SS_600-0-500_Cycle_30-70RH no breaks",  
  DyeType = "Purple",
  ApplicationMethod = "PDMS+paper+octopus",
  Filename = basename("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-20-1721_ICOS_30-70oscillations_blueDye_licor+PDMS+ABS+octopus+multispec.csv")
)

# python.data20250520_Blue %>% head()
##end of one measurment##


python.data20250520_Blue %>% 
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_point() +
  geom_line()






# 2025-05-21: (purple dye) flow on and off | 200 co2 | RH 30% | temp 25 ----

## I'm testing wheather shutting of the flow in the licor results in a visible effect in measured dye reflectance
## The dye was applied an on PDMS octopus with paper as background

## I. Load Licor CO2_r data ----
#no licor data, due to technical issues (corrupted file)


## II. Load python data ----
## this data is measured with the multispec recorded by python code AM- CO2 dye 5-kiek


#loads python data
python.data20250521_1057_flowOnOff <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-21-1057_ICOS_flowOff_purpleDye_licor+PDMS+paper+octopus+multispec.csv",
                                     sep = ",") 

# Rename PulseNumber to obs for alignment
python.data20250521_1057_flowOnOff <- python.data20250521_1057_flowOnOff %>%
  rename(obs = PulseNumber)

#add metadata
python.data20250521_1057_flowOnOff <- python.data20250521_1057_flowOnOff %>% mutate(
  SamplingDate = as.Date("2025-05-21"),
  Protocol = "flow on and off",  
  DyeType = "Purple",
  ApplicationMethod = "PDMS+paper+octopus",
  Filename = basename("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-21-1057_ICOS_flowOff_purpleDye_licor+PDMS+paper+octopus+multispec.csv")
)

python.data20250521_1057_flowOnOff %>% head()
##end of one measurment##


python.data20250521_1057_flowOnOff %>% 
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_point() +
  geom_line()






# 2025-05-21: (purple dye) flow on | 200 co2 | RH 30% | temp 25 ----

## I'm testing wheather the change due to flow on and off from the previous experiment is a result of the dye jet to be stabilised or the flow on and off it self
## The dye was applied an on PDMS octopus with paper as background

## I. Load Licor CO2_r data ----
#no licor data, due to technical issues (corrupted file)


## II. Load python data ----
## this data is measured with the multispec recorded by python code AM- CO2 dye 5-kiek


#loads python data
python.data20250521_1130_FlowOn <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-21-1130_ICOS_doNothingOn_purpleDye_licor+PDMS+paper+octopus+multispec.csv",
                                             sep = ",") 

# Rename PulseNumber to obs for alignment
python.data20250521_1130_FlowOn <- python.data20250521_1130_FlowOn %>%
  rename(obs = PulseNumber)

#add metadata
python.data20250521_1130_FlowOn <- python.data20250521_1130_FlowOn %>% mutate(
  SamplingDate = as.Date("2025-05-21"),
  Protocol = "do nothing with flow on",  
  DyeType = "Purple",
  ApplicationMethod = "PDMS+paper+octopus",
  Filename = basename("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-21-1130_ICOS_doNothingOn_purpleDye_licor+PDMS+paper+octopus+multispec.csv")
)

python.data20250521_1130_FlowOn %>% head()
##end of one measurment##


python.data20250521_1130_FlowOn %>% 
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_point() +
  geom_line()







# 2025-05-21: (purple dye) flow Off | 200 co2 | RH 30% | temp 25 ----

## I'm testing wheather the change due to flow on and off from the previous experiment is a result of the dye jet to be stabilised or the flow on and off it self
## The dye was applied an on PDMS octopus with paper as background

## I. Load Licor CO2_r data ----
#no licor data, due to technical issues (corrupted file)


## II. Load python data ----
## this data is measured with the multispec recorded by python code AM- CO2 dye 5-kiek


#loads python data
python.data20250521_1207_doNothingOff <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-21-1207_ICOS_doNothingOff_purpleDye_licor+PDMS+paper+octopus+multispec.csv",
                                                 sep = ",") 

# Rename PulseNumber to obs for alignment
python.data20250521_1207_doNothingOff <- python.data20250521_1207_doNothingOff %>%
  rename(obs = PulseNumber)

#add metadata
python.data20250521_1207_doNothingOff <- python.data20250521_1207_doNothingOff %>% mutate(
  SamplingDate = as.Date("2025-05-21"),
  Protocol = "do nothing with flow off",  
  DyeType = "Purple",
  ApplicationMethod = "PDMS+paper+octopus",
  Filename = basename("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-21-1207_ICOS_doNothingOff_purpleDye_licor+PDMS+paper+octopus+multispec.csv")
)

python.data20250521_1207_doNothingOff %>% head()
##end of one measurment##


python.data20250521_1207_doNothingOff %>% 
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_point() +
  geom_line()




# 2025-05-21: (purple dye) flow rate | 200 co2 | RH 30% | temp 25 ----

## I'm testing wheather changing flow rate from 400 to 0 in the licor results in a visible effect in measured dye reflectance
## The dye was applied an on PDMS octopus with paper as background

## I. Load Licor CO2_r data ----
#no licor data, due to technical issues (corrupted file)


## II. Load python data ----
## this data is measured with the multispec recorded by python code AM- CO2 dye 5-kiek


#loads python data
python.data20250521_1214_flowRate <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-21-1214_ICOS_flowRate_purpleDye_licor+PDMS+paper+octopus+multispec.csv",
                                                  sep = ",") 

# Rename PulseNumber to obs for alignment
python.data20250521_1214_flowRate <- python.data20250521_1214_flowRate %>%
  rename(obs = PulseNumber)

#add metadata
python.data20250521_1214_flowRate <- python.data20250521_1214_flowRate %>% mutate(
  SamplingDate = as.Date("2025-05-21"),
  Protocol = "flow rate 400-0-400",  
  DyeType = "Purple",
  ApplicationMethod = "PDMS+paper+octopus",
  Filename = basename("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-21-1214_ICOS_flowRate_purpleDye_licor+PDMS+paper+octopus+multispec.csv")
)

python.data20250521_1214_flowRate %>% head()
##end of one measurment##


python.data20250521_1214_flowRate %>% 
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_point() +
  geom_line()






# 2025-05-21: (purple dye) flow percentage | 200 co2 | RH 30% | temp 25 ----

## I'm testing wheather changing flow persentage from 80 to 0 in the licor results in a visible effect in measured dye reflectance
## The dye was applied an on PDMS octopus with paper as background

## I. Load Licor CO2_r data ----
#no licor data, due to technical issues (corrupted file)


## II. Load python data ----
## this data is measured with the multispec recorded by python code AM- CO2 dye 5-kiek


#loads python data
python.data20250521_1257_licht <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-21-1236_ICOS_flowPercent_purpleDye_licor+PDMS+paper+octopus+multispec.csv",
                                              sep = ",") 

# Rename PulseNumber to obs for alignment
python.data20250521_1257_licht <- python.data20250521_1257_licht %>%
  rename(obs = PulseNumber)

#add metadata
python.data20250521_1257_licht <- python.data20250521_1257_licht %>% mutate(
  SamplingDate = as.Date("2025-05-21"),
  Protocol = "flow percentage 80-0",  
  DyeType = "Purple",
  ApplicationMethod = "PDMS+paper+octopus",
  Filename = basename("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-21-1236_ICOS_flowPercent_purpleDye_licor+PDMS+paper+octopus+multispec.csv")
)

python.data20250521_1257_licht %>% head()
##end of one measurment##


python.data20250521_1257_licht %>% 
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_point() +
  geom_line()






# 2025-05-21: (purple dye) light 0-2000 | 200 co2 | RH 30% | temp 25 ----

## I'm testing wheather changing light from 0 to 2000 in the licor results in a visible effect in measured dye reflectance
## The dye was applied an on PDMS octopus with paper as background

## I. Load Licor CO2_r data ----
#no licor data, due to technical issues (corrupted file)


## II. Load python data ----
## this data is measured with the multispec recorded by python code AM- CO2 dye 5-kiek


#loads python data
python.data20250521_1257_licht <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-21-1257_ICOS_licht_purpleDye_licor+PDMS+paper+octopus+multispec.csv",
                                                 sep = ",") 

# Rename PulseNumber to obs for alignment
python.data20250521_1257_licht <- python.data20250521_1257_licht %>%
  rename(obs = PulseNumber)

#add metadata
python.data20250521_1257_licht <- python.data20250521_1257_licht %>% mutate(
  SamplingDate = as.Date("2025-05-21"),
  Protocol = "flow percentage 0-2000",  
  DyeType = "Purple",
  ApplicationMethod = "PDMS+paper+octopus",
  Filename = basename("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-21-1257_ICOS_licht_purpleDye_licor+PDMS+paper+octopus+multispec.csv")
)

python.data20250521_1257_licht %>% head()
##end of one measurment##


python.data20250521_1257_licht %>% 
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_point() +
  geom_line()








# 2025-05-21: (purple dye) Fan from 10000-3000 | 200 co2 | RH 30% | temp 25 ----

## I'm testing wheather changing the fan from 10000 to 3000 in the licor results in a visible effect in measured dye reflectance
## The dye was applied an on PDMS octopus with paper as background

## I. Load Licor CO2_r data ----
#no licor data, due to technical issues (corrupted file)


## II. Load python data ----
## this data is measured with the multispec recorded by python code AM- CO2 dye 5-kiek


#loads python data
python.data20250521_1327_fanOnOff <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-21-1327_ICOS_fanOnOff_purpleDye_licor+PDMS+paper+octopus+multispec.csv",
                                           sep = ",") 

# Rename PulseNumber to obs for alignment
python.data20250521_1327_fanOnOff <- python.data20250521_1327_fanOnOff %>%
  rename(obs = PulseNumber)

#add metadata
python.data20250521_1327_fanOnOff <- python.data20250521_1327_fanOnOff %>% mutate(
  SamplingDate = as.Date("2025-05-21"),
  Protocol = "fan speed from 10000 to 3000",  
  DyeType = "Purple",
  ApplicationMethod = "PDMS+paper+octopus",
  Filename = basename("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-21-1327_ICOS_fanOnOff_purpleDye_licor+PDMS+paper+octopus+multispec.csv")
)

python.data20250521_1327_fanOnOff %>% head()
##end of one measurment##


python.data20250521_1327_fanOnOff %>% 
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_point() +
  geom_line()







# 2025-05-21: (purple dye) Fan percentage | 200 co2 | RH 30% | temp 25 ----

## I'm testing wheather changing the fan percentage from 80% to 20% in the licor results in a visible effect in measured dye reflectance
## The dye was applied an on PDMS octopus with paper as background

## I. Load Licor CO2_r data ----
#no licor data, due to technical issues (corrupted file)


## II. Load python data ----
## this data is measured with the multispec recorded by python code AM- CO2 dye 5-kiek


#loads python data
python.data20250521_1356_fanPercent <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-21-1356_ICOS_fanPercent_purpleDye_licor+PDMS+paper+octopus+multispec.csv",
                                              sep = ",") 

# Rename PulseNumber to obs for alignment
python.data20250521_1356_fanPercent <- python.data20250521_1356_fanPercent %>%
  rename(obs = PulseNumber)

#add metadata
python.data20250521_1356_fanPercent <- python.data20250521_1356_fanPercent %>% mutate(
  SamplingDate = as.Date("2025-05-21"),
  Protocol = "fan percentage 80 to 20%",  
  DyeType = "Purple",
  ApplicationMethod = "PDMS+paper+octopus",
  Filename = basename("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-21-1356_ICOS_fanPercent_purpleDye_licor+PDMS+paper+octopus+multispec.csv")
)

python.data20250521_1356_fanPercent %>% head()
##end of one measurment##


python.data20250521_1356_fanPercent %>% 
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_point() +
  geom_line()











# 2025-05-21: (purple dye) leak test | 200 co2 | RH 30% | temp 25 ----

## I'm testing wheather blowing on the licor chamber results in a visible effect in measured dye reflectance indicating leaks in the chamber
## The dye was applied an on PDMS octopus with paper as background

## I. Load Licor CO2_r data ----
#no licor data, due to technical issues (corrupted file)


## II. Load python data ----
## this data is measured with the multispec recorded by python code AM- CO2 dye 5-kiek


#loads python data
python.data20250521_1414_leakCheck <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-21-1414_ICOS_leakCheck_purpleDye_licor+PDMS+paper+octopus+multispec.csv",
                                                sep = ",") 

# Rename PulseNumber to obs for alignment
python.data20250521_1414_leakCheck <- python.data20250521_1414_leakCheck %>%
  rename(obs = PulseNumber)

#add metadata
python.data20250521_1414_leakCheck <- python.data20250521_1414_leakCheck %>% mutate(
  SamplingDate = as.Date("2025-05-21"),
  Protocol = "fan percentage 80 to 20%",  
  DyeType = "Purple",
  ApplicationMethod = "PDMS+paper+octopus",
  Filename = basename("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-21-1414_ICOS_leakCheck_purpleDye_licor+PDMS+paper+octopus+multispec.csv")
)

python.data20250521_1414_leakCheck %>% head()
##end of one measurment##


python.data20250521_1414_leakCheck %>% 
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_point() +
  geom_line()










# 2025-05-21: (purple dye) flow rate | 200 co2 | RH 30% | temp 25 ----

## I'm testing wheather changing flow rate from 400 to 0 in the licor results in a visible effect in measured dye reflectance
## the dye has been in same conditions for longer now so i want to check if it's more stable now, previous graph was difficult to read 
## The dye was applied an on PDMS octopus with paper as background

## I. Load Licor CO2_r data ----
#no licor data, due to technical issues (corrupted file)


## II. Load python data ----
## this data is measured with the multispec recorded by python code AM- CO2 dye 5-kiek


#loads python data
python.data20250521_1428_flowRate <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-21-1428_ICOS_flowRate_purpleDye_licor+PDMS+paper+octopus+multispec.csv",
                                              sep = ",") 

# Rename PulseNumber to obs for alignment
python.data20250521_1428_flowRate <- python.data20250521_1428_flowRate %>%
  rename(obs = PulseNumber)

#add metadata
python.data20250521_1428_flowRate <- python.data20250521_1214_flowRate %>% mutate(
  SamplingDate = as.Date("2025-05-21"),
  Protocol = "flow rate 400-0-400",  
  DyeType = "Purple",
  ApplicationMethod = "PDMS+paper+octopus",
  Filename = basename("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-21-1428_ICOS_flowRate_purpleDye_licor+PDMS+paper+octopus+multispec.csv")
)

python.data20250521_1428_flowRate %>% head()
##end of one measurment##


python.data20250521_1428_flowRate %>% 
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_point() +
  geom_line()











# 2025-05-21: (purple dye) flow rate 0 | 200 co2 | RH 30% | temp 25 ----

## see if constant flow rate 0 results in change in measured reflectance
## The dye was applied an on PDMS octopus with paper as background

## I. Load Licor CO2_r data ----
#no licor data, due to technical issues (corrupted file)


## II. Load python data ----
## this data is measured with the multispec recorded by python code AM- CO2 dye 5-kiek


#loads python data
python.data20250521_1621_flowRate0 <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-21-1621_ICOS_flowRate0_purpleDye_licor+PDMS+paper+octopus+multispec.csv",
                                              sep = ",") 

# Rename PulseNumber to obs for alignment
python.20250521_1621_flowRate0 <- python.20250521_1621_flowRate0 %>%
  rename(obs = PulseNumber)

#add metadata
python.20250521_1621_flowRate0 <- python.data20250521_1621_flowRate0 %>% mutate(
  SamplingDate = as.Date("2025-05-21"),
  Protocol = "flow rate 0",  
  DyeType = "Purple",
  ApplicationMethod = "PDMS+paper+octopus",
  Filename = basename("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-21-1621_ICOS_flowRate0_purpleDye_licor+PDMS+paper+octopus+multispec.csv")
)

python.20250521_1621_flowRate0 %>% head()
##end of one measurment##


python.20250521_1621_flowRate0 %>% 
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_point() +
  geom_line()









# 2025-05-21: (purple dye) flow and fan | 200 co2 | RH 30% | temp 25 ----

## see if when flow and fan simultaniously changed if this is displayed on the measured reflectance
## flow 0     400
## fan  3000  10000
## The dye was applied an on PDMS octopus with paper as background

## I. Load Licor CO2_r data ----
#no licor data, due to technical issues (corrupted file)


## II. Load python data ----
## this data is measured with the multispec recorded by python code AM- CO2 dye 5-kiek


#loads python data
python.data20250521_1641_flowAndFan <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-21-1641_ICOS_flowAndFan_purpleDye_licor+PDMS+paper+octopus+multispec.csv",
                                               sep = ",") 

# Rename PulseNumber to obs for alignment
python.20250521_1641_flowAndFan <- python.20250521_1641_flowAndFan %>%
  rename(obs = PulseNumber)

#add metadata
python.20250521_1641_flowAndFan <- python.data20250521_1641_flowAndFan %>% mutate(
  SamplingDate = as.Date("2025-05-21"),
  Protocol = "flow and fan simultanious change",  
  DyeType = "Purple",
  ApplicationMethod = "PDMS+paper+octopus",
  Filename = basename("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-21-1641_ICOS_flowAndFan_purpleDye_licor+PDMS+paper+octopus+multispec.csv")
)

python.20250521_1641_flowAndFan %>% head()
##end of one measurment##


python.20250521_1641_flowAndFan %>% 
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_point() +
  geom_line()



# 2025-05-22: (purple dye) flow and fan, leaf test | 200 co2 | RH 30% | temp 25 ----

## see if when flow and fan simultaniously changed if this is displayed on the measured reflectance
## flow 0     400
## fan  3000  10000
## The dye was applied an on PDMS octopus with paper as background

## I. Load Licor CO2_r data ----
#no licor data, due to technical issues (corrupted file)


## II. Load python data ----
## this data is measured with the multispec recorded by python code AM- CO2 dye 5-kiek


#loads python data
python.data20250522_1130_flowAndFan.leafTest <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-22-1130_ICOS_flowAndFan+leafTest_purpleDye_licor+PDMS+paper+octopus+multispec.csv",
                                                sep = ",") 

# Rename PulseNumber to obs for alignment
python.20250522_1130_flowAndFan.leafTest <- python.20250522_1130_flowAndFan.leafTest %>%
  rename(obs = PulseNumber)

#add metadata
python.20250522_1130_flowAndFan.leafTest <- python.data20250522_1130_flowAndFan.leafTest %>% mutate(
  SamplingDate = as.Date("2025-05-22"),
  Protocol = "flow and fan simultanious change durring leaf",  
  DyeType = "Purple",
  ApplicationMethod = "PDMS+paper+octopus",
  Filename = basename("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-22-1130_ICOS_flowAndFan+leafTest_purpleDye_licor+PDMS+paper+octopus+multispec.csv")
)

python.20250522_1130_flowAndFan.leafTest %>% head()
##end of one measurment##


python.20250522_1130_flowAndFan.leafTest %>% 
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_point() +
  geom_line()





# 2025-05-22: (purple dye) flow rate 400-0-400, leaf test | 200 co2 | RH 30% | temp 25 ----

## see if when flow and fan simultaniously changed if this is displayed on the measured reflectance
## flow 0     400
## fan  10000  10000
## The dye was applied an on PDMS octopus with paper as background

## I. Load Licor CO2_r data ----
#no licor data, due to technical issues (corrupted file)


## II. Load python data ----
## this data is measured with the multispec recorded by python code AM- CO2 dye 5-kiek


#loads python data

python.data20250522_1442_flowRate.leafTest <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-22-1442_ICOS_flowRate+leafTest_purpleDye_licor+PDMS+paper+octopus+multispec.csv",
                                                         sep = ",") 

# Rename PulseNumber to obs for alignment
python.data20250522_1442_flowRate.leafTest <- python.data20250522_1442_flowRate.leafTest %>%
  rename(obs = PulseNumber)

#add metadata
python.data20250522_1442_flowRate.leafTest <- python.data20250522_1442_flowRate.leafTest %>% mutate(
  SamplingDate = as.Date("2025-05-22"),
  Protocol = "flow rate oscillations, fan constant, durring leaf",  
  DyeType = "Purple",
  ApplicationMethod = "PDMS+paper+octopus",
  Filename = basename("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-22-1442_ICOS_flowRate+leafTest_purpleDye_licor+PDMS+paper+octopus+multispec.csv")
)

python.data20250522_1442_flowRate.leafTest %>% head()
##end of one measurment##


python.data20250522_1442_flowRate.leafTest %>% 
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_point() +
  geom_line()










# 2025-05-29: leaf test, plateau + light ----

## first flow on to ensure dye stable, then flow and let the dye reatch a plateau then oscillation light, so i would expect indents when light on from photosynthesis thus co2 absorption
## The dye was applied on an PDMS octopus with ABS incased in it for reflectance

## I. Load Licor CO2_r data ----
co2Cycle.20250529.plateau.leafTest <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/LicorDataCSV/2025-05-29-1154_ICOS_leafTest+plateau+lightOscillations_purpleDye_licor+PDMS+paper+octopus+mutispec.csv",
                                                            fileEncoding="latin1", sep = ";", dec = ".", skip = 13) %>% .[-1,] %>% 
  mutate(SamplingDate = as.Date("2025-05-29"),
         Protocol = "leafTest+plateau+lightOscillations",
         elapsed = as.numeric(sub(",", ".", as.character(elapsed), fixed = TRUE))/60+2, #added +2 to match python data
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

co2Cycle.20250529.plateau.leafTest %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = RHcham, col = CO2_r)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "RHcham") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))

co2Cycle.20250529.plateau.leafTest %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = Tair)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "Tair (degrees)") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))


co2Cycle.20250529.plateau.leafTest %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = CO2_r, col = Tair_f)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "CO2_r (umol/min)") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))



## II. Load python data ----## II. Load python CO2_rdata ----
## this data is measured with the multispec recorded by python code AM- CO2 dye 5-kiek

#loads python data
python.data.20250529.plateau.leafTest <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-29-1154_ICOS_leafTest+plateau+lightOscillations_purpleDye_licor+PDMS+paper+octopus+multispec.csv",
                                                          sep = ",") 

# Rename PulseNumber to obs for alignment
python.data.20250529.plateau.leafTest <- python.data.20250529.plateau.leafTest %>%
  rename(obs = PulseNumber)

#add metadata
python.data.20250529.plateau.leafTest <- python.data.20250529.plateau.leafTest %>% mutate(
  shareTime = Time..min.,
  SamplingDate = as.Date("2025-05-29"),
  Protocol = "leafTest+plateau+lightOscillations",  
  DyeType = "Purple",
  ApplicationMethod = "PDMS+ABS+octopus",
  Filename = basename("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-29-1154_ICOS_leafTest+plateau+lightOscillations_purpleDye_licor+PDMS+paper+octopus+multispec.csv")
)



python.data.20250529.plateau.leafTest %>% 
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_point() +
  geom_line()



## III. Merge Licor and python data ----
## need to do the pairing by time
## Create PairingID -> Need to create a PairingID column that would stick to the LR1 and licor data based on the .Hour

## __ i. data pairing ----

# licor data time pairing
licor.time.20250529.plateau.leafTest <- co2Cycle.20250529.plateau.leafTest %>%
  select(SamplingDate, Protocol, obs, elapsed) %>%
  arrange(obs)

# licor.time.20250529.plateau.leafTest %>% head()

# python data time pairing
python.time.20250529.plateau.leafTest <- python.data.20250529.plateau.leafTest %>%
  arrange(obs) %>%
  select(SamplingDate, Protocol, obs, Time..min.,)

# python.min.20250529.plateau.leafTest %>%  head()


# Join licor_data and python.data by the common 'obs' column
TimePaired.data.20250529.plateau.leafTest <- licor.time.20250529.plateau.leafTest %>%
  inner_join(python.time.20250529.plateau.leafTest, by = "obs") %>%
  arrange(obs)  

# TimePaired.data.20250529.plateau.leafTest %>% head()




## Visualize Pairing of measurements
rbind(
  licor.time.20250529.plateau.leafTest %>% 
    mutate(Instrument = "Licor") %>% dplyr::rename(Hour = elapsed),
  python.time.20250529.plateau.leafTest %>% 
    mutate(Instrument = "python") %>% dplyr::rename(Hour = Time..min.)
) %>% as.data.frame() %>% 
  ggplot(aes(x = Hour, y = 1)) + 
  facet_wrap(Protocol ~., scales = "free_x", ncol = 1) +
  geom_line() +
  geom_point(size = 3, aes(, col = Instrument)) 


TimePaired.data.20250529.plateau.leafTest %>% summarise(across(c(elapsed, Time..min.), ~sum(is.na(.))))
## changed licor.hour to elapsed


## __ i. Merge ----
merged.df.20250529.plateau.leafTest <- left_join(
  python.data.20250529.plateau.leafTest %>% select(-obs), 
  co2Cycle.20250529.plateau.leafTest
  )
merged.df.20250529.plateau.leafTest %>% 
  ggplot(aes(x = shareTime, y = Reflectance_590, )) +
  geom_line() +
  geom_point(data = . %>% filter(!is.na(Tair)),
             aes(col = Tair), size = 3.5)



merged.df.20250529.plateau.leafTest %>% head() %>% select(DyeType, Protocol, SamplingDate, Licor.Hour,RH_f, CO2_r, elapsed, Time..min., obs, Reflectance_590, RHcham)


merged.df.20250529.plateau.leafTest %>% 
  ggplot(aes(x = elapsed, y = Time..min., col = Protocol)) +
  geom_abline(slope = 1, intercept =  0) + geom_point(size = 3) +  theme_bw()

merged.df.20250529.plateau.leafTest %>% with(table(Reflectance_590, Tair, RH_f))


## IV. Data analysis ----

## __ iv) estimate hysteresis
merged.df.20250529.plateau.leafTest %>%
  ggplot(aes(x = CO2_r, y = Reflectance_590)) +
  # facet_grid( ~ RH_f) + #, scales = "free_y"
  geom_point(size = 3) + #geom_line(aes(group = Cycle), lwd = 1) +
  geom_path() +
  theme_bw() + theme(panel.grid = element_blank())



##########not jet working############
## colour gradiant
merged.df.20250529.plateau.leafTest %>% 
  filter(Tair_f %in% c(15, 30)) %>% 
  ggplot(aes(x = obs, y = Reflectance_590)) +
  scale_color_gradient(low = "darkorchid4", high = "thistle1") +
  
  facet_grid(~Tair_f, scales = "free_x") +
  geom_line() +
  geom_point(aes(col = CO2_r), size = 3) + 
  theme_bw() + theme(panel.grid = element_blank()) 



# -->2025-05-29: leaf test, plateau + light, short, direct light oscillations----

## first flow on to ensure dye stable, then flow and let the dye reatch a plateau then oscillation light, so i would expect indents when light on from photosynthesis thus co2 absorption
## The dye was applied on an PDMS octopus with ABS incased in it for reflectance
## direct after stabilisation start oscillations, already before plateau fully formed. This is to check if the teh lowering of teh total during the first plateau measuremtn test didn't scruw the persieved photosynthesis.

## I. Load Licor CO2_r data ----
co2Cycle.20250529.plateau.leafTest.direct <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/LicorDataCSV/2025-05-29-1333_lCOS_leafTest+plateau+lightOscillations_purpleDye_licor+PDMS+paper+octopus+multispec.csv",
                                               fileEncoding="latin1", sep = ";", dec = ".", skip = 13) %>% .[-1,] %>% 
  mutate(SamplingDate = as.Date("2025-05-29"),
         Protocol = "leafTest+plateau+direct start light oscillations",
         elapsed = as.numeric(sub(",", ".", as.character(elapsed), fixed = TRUE))/60+2, #added +2 to match python data
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


co2Cycle.20250529.plateau.leafTest.direct %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = RHcham, col = CO2_r)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "RHcham") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))

co2Cycle.20250529.plateau.leafTest.direct %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = Tair)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "Tair (degrees)") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))


co2Cycle.20250529.plateau.leafTest.direct %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = CO2_r, col = Tair_f)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "CO2_r (umol/min)") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))



# co2Cycle.20250529.plateau.leafTest.direct %>% select(obs, Tair)


## II. Load python data ----## II. Load python CO2_rdata ----
## this data is measured with the multispec recorded by python code AM- CO2 dye 5-kiek

#loads python data
python.data.20250529.plateau.leafTest.direct <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-29-1333_ICOS_leafTest+plateau+lightOscillations_purpleDye_licor+PDMS+paper+octopus+multispec.csv",
                                                  sep = ",") 

# Rename PulseNumber to obs for alignment
python.data.20250529.plateau.leafTest.direct <- python.data.20250529.plateau.leafTest.direct %>%
  rename(obs = PulseNumber)

#add metadata
python.data.20250529.plateau.leafTest.direct <- python.data.20250529.plateau.leafTest.direct %>% mutate(
  shareTime = Time..min.,
  SamplingDate = as.Date("2025-05-29"),
  Protocol = "leafTest+plateau+direct start light oscillations",  
  DyeType = "Purple",
  ApplicationMethod = "PDMS+ABS+octopus",
  Filename = basename("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-05-29-1333_ICOS_leafTest+plateau+lightOscillations_purpleDye_licor+PDMS+paper+octopus+multispec.csv")
)

# python.data.20250529.plateau.leafTest.direct %>% head()
##end of one measurment##


python.data.20250529.plateau.leafTest.direct %>% 
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_point() +
  geom_line()


## III. Merge Licor and python data ----
## need to do the pairing by time
## Create PairingID -> Need to create a PairingID column that would stick to the LR1 and licor data based on the .Hour

## __ i. data pairing ----

# licor data time pairing
licor.time.20250529.plateau.leafTest.direct <- co2Cycle.20250529.plateau.leafTest.direct %>%
  select(SamplingDate, Protocol, obs, elapsed) %>%
  arrange(obs)

# licor.time.20250529.plateau.leafTest.direct %>% head()

# python data time pairing
python.time.20250529.plateau.leafTest.direct <- python.data.20250529.plateau.leafTest.direct %>%
  arrange(obs) %>%
  select(SamplingDate, Protocol, obs, Time..min.,)

# python.min.20250529.plateau.leafTest.direct %>%  head()


# Join licor_data and python.data by the common 'obs' column
TimePaired.data.20250529.plateau.leafTest.direct <- licor.time.20250529.plateau.leafTest.direct %>%
  inner_join(python.time.20250529.plateau.leafTest.direct, by = "obs") %>%
  arrange(obs)  

# TimePaired.data.20250529.plateau.leafTest.direct %>% head()




## Visualize Pairing of measurements
rbind(
  licor.time.20250529.plateau.leafTest.direct %>% 
    mutate(Instrument = "Licor") %>% dplyr::rename(Hour = elapsed),
  python.time.20250529.plateau.leafTest.direct %>% 
    mutate(Instrument = "python") %>% dplyr::rename(Hour = Time..min.)
) %>% as.data.frame() %>% 
  ggplot(aes(x = Hour, y = 1)) + 
  facet_wrap(Protocol ~., scales = "free_x", ncol = 1) +
  geom_line() +
  geom_point(size = 3, aes(, col = Instrument)) 


TimePaired.data.20250529.plateau.leafTest.direct %>% summarise(across(c(elapsed, Time..min.), ~sum(is.na(.))))
## changed licor.hour to elapsed


## __ i. Merge ----
merged.df.20250529.plateau.leafTest.direct <- left_join(
  python.data.20250529.plateau.leafTest.direct %>% select(-obs), 
  co2Cycle.20250529.plateau.leafTest.direct
)
merged.df.20250529.plateau.leafTest.direct %>% 
  ggplot(aes(x = shareTime, y = Reflectance_590, )) +
  geom_line() +
  geom_point(data = . %>% filter(!is.na(Tair)),
             aes(col = Tair), size = 3.5) +
  
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






merged.df.20250529.plateau.leafTest.direct %>% head() %>% select(DyeType, Protocol, SamplingDate, Licor.Hour,RH_f, CO2_r, elapsed, Time..min., obs, Reflectance_590, RHcham)










# -->2025-06-02: leaf test, light oscillations 2min----

## flow on but light oscillating every 2 minutes, this way co2 assimilation (with is fast) stops for 2 minutes well scroma closing/opening which effects humidity (this is slow), so i expect humidity to remain conctand
## The dye was applied on an PDMS octopus with ABS incased in it for reflectance

## I. Load Licor CO2_r data ----
co2Cycle.20250602.leafTest.OutputRHline <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/LicorDataCSV/2025-06-02-1447_ICOS_leafTest+lightOscilations2min+OutputRHline_purpleDye_licor+PDMS+paper+octopus+multispec.csv",
                                               fileEncoding="latin1", sep = ";", dec = ".", skip = 13) %>% .[-1,] %>% 
  mutate(SamplingDate = as.Date("2025-06-02"),
         Protocol = "light oscillations 2 min, stroma, linair RH",
         elapsed = as.numeric(sub(",", ".", as.character(elapsed), fixed = TRUE))/60+2, #added +2 to match python data
         # substituding , for . when going from character to numeric.
         CO2_r = as.numeric(sub(",", ".", as.character(CO2_r), fixed = TRUE)),
         CO2_r = ifelse(CO2_r < 0, 0, CO2_r %>% round),
         Qin = as.numeric(sub(",", ".", as.character(Qin), fixed = TRUE)),
         Flow = as.numeric(sub(",", ".", as.character(Flow), fixed = TRUE)),
         Fan_speed = as.numeric(sub(",", ".", as.character(Fan_speed), fixed = TRUE)),
         RHcham = as.numeric(sub(",", ".", as.character(RHcham), fixed = TRUE)),
         RH_f = round(RHcham, digits = 0) %>% as.factor(),
         Tair = as.numeric(sub(",", ".", as.character(Tair), fixed = TRUE)),
         Tair_f = round(Tair, digits = 0) %>% as.factor(),
         shareTime = round(elapsed, digits = 0),
         Licor.Hour = hhmmss %>% myHour.f()) %>% 
  select(SamplingDate, Protocol, Licor.Hour, hhmmss, elapsed, obs, CO2_r, RHcham, RH_f, Tair, Tair_f, Qin, Flow, Fan_speed, shareTime) 

co2Cycle.20250602.leafTest.OutputRHline %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = RHcham, col = CO2_r)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "RHcham") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))

co2Cycle.20250602.leafTest.OutputRHline %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = Tair)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "Tair (degrees)") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))


co2Cycle.20250602.leafTest.OutputRHline %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = CO2_r, col = Tair_f)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "CO2_r (umol/min)") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))



# co2Cycle.20250602.leafTest.OutputRHline %>% select(obs, Tair)


## II. Load python data ----## II. Load python CO2_rdata ----
## this data is measured with the multispec recorded by python code AM- CO2 dye 5-kiek

#loads python data
python.data.20250602.leafTest.OutputRHline <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-06-02-1447_ICOS_leafTest+lightOscillations2min+outputRHline_purpleDye_licor+PDMS+paper+octopus+multispec.csv",
                                                  sep = ",") 

# Rename PulseNumber to obs for alignment
python.data.20250602.leafTest.OutputRHline <- python.data.20250602.leafTest.OutputRHline %>%
  rename(obs = PulseNumber)

#add metadata
python.data.20250602.leafTest.OutputRHline <- python.data.20250602.leafTest.OutputRHline %>% mutate(
  shareTime = Time..min.,
  SamplingDate = as.Date("2025-06-02"),
  Protocol = "light oscillations 2 min, stroma, linair RH",  
  DyeType = "Purple",
  ApplicationMethod = "PDMS+ABS+octopus",
  Filename = basename("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-06-02-1447_ICOS_leafTest+lightOscillations2min+outputRHline_purpleDye_licor+PDMS+paper+octopus+multispec.csv")
)

# python.data.20250602.leafTest.OutputRHline %>% head()
##end of one measurment##


python.data.20250602.leafTest.OutputRHline %>% 
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_point() +
  geom_line()


# averaging to Python data----
python_avg.20250602.leafTest.OutputRHline <- python.data.20250602.leafTest.OutputRHline %>%
  mutate(group = floor(row_number() / 10)) %>%  
  group_by(group) %>%
  summarise(
    Time_avg.20250602.leafTest.OutputRHline = mean(Time..min., na.rm = TRUE),  
    Reflectance_avg.20250602.leafTest.OutputRHline = mean(Reflectance_590, na.rm = TRUE) 
  ) %>%
  ungroup() %>%  
  mutate(obs = row_number()) %>%  
  select(-group)

python_avg.20250602.leafTest.OutputRHline %>%
  ggplot(aes(x = Time_avg.20250602.leafTest.OutputRHline, y = Reflectance_avg.20250602.leafTest.OutputRHline)) +
  geom_point() +
  geom_line()


## III. Merge Licor and python data ----
## need to do the pairing by time
## Create PairingID -> Need to create a PairingID column that would stick to the LR1 and licor data based on the .Hour

## __ i. data pairing ----

# licor data time pairing
licor.time.20250602.leafTest.OutputRHline <- co2Cycle.20250602.leafTest.OutputRHline %>%
  select(SamplingDate, Protocol, obs, elapsed) %>%
  arrange(obs)

# licor.time.20250602.leafTest.OutputRHline %>% head()

# python data time pairing
python.time_avg.20250602.leafTest.OutputRHline <- python_avg.20250602.leafTest.OutputRHline %>%
  arrange(obs) %>%
  select(obs, Time_avg.20250602.leafTest.OutputRHline)

# python.min.20250602.leafTest.OutputRHline %>%  head()


# Join licor_data and python.data by the common 'obs' column
TimePaired.data.20250602.leafTest.OutputRHline <- licor.time.20250602.leafTest.OutputRHline %>%
  inner_join(python.time_avg.20250602.leafTest.OutputRHline, by = "obs") %>%
  arrange(obs)  

# TimePaired.data.20250602.leafTest.OutputRHline %>% head()




## Visualize Pairing of measurements
rbind(
  licor.time.20250602.leafTest.OutputRHline %>% 
    mutate(Instrument = "Licor") %>% dplyr::rename(Hour = elapsed),
  python.time.20250602.leafTest.OutputRHline %>% 
    mutate(Instrument = "python") %>% dplyr::rename(Hour = Time..min.)
) %>% as.data.frame() %>% 
  ggplot(aes(x = Hour, y = 1)) + 
  facet_wrap(Protocol ~., scales = "free_x", ncol = 1) +
  geom_line() +
  geom_point(size = 3, aes(, col = Instrument)) 


TimePaired.data.20250602.leafTest.OutputRHline %>% summarise(across(c(elapsed, Time_avg.20250602.leafTest.OutputRHline), ~sum(is.na(.))))
## changed licor.hour to elapsed


#smooth merge
# Assign the closest 'obs' from Licor data
# Merge with Licor data

merged_smooth.20250602.leafTest.OutputRHline <- merge(
  co2Cycle.20250602.leafTest.OutputRHline,   
  python_avg.20250602.leafTest.OutputRHline,                                
  by.x = "elapsed",                          
  by.y = "Time_avg.20250602.leafTest.OutputRHline",                         
  all.x = TRUE                               
)



## __ i. Merge ----
merged.df.20250602.leafTest.OutputRHline <- left_join(
  python_avg.20250602.leafTest.OutputRHline, 
  co2Cycle.20250602.leafTest.OutputRHline
)
merged.df.20250602.leafTest.OutputRHline %>% 
  ggplot(aes(x = shareTime, y = Reflectance_avg.20250602.leafTest.OutputRHline, )) +
  geom_line() +
  geom_point(data = . %>% filter(!is.na(Tair)),
             aes(col = Tair), size = 3.5) +
    geom_point(aes(y = Flow/400 +1690)) +
  geom_step(data = . %>% filter(!is.na(Flow)), direction = "vh",
          aes(y = Flow/400 +1690), col = "green") +
  
  geom_point(aes(y = Qin/400 +1680)) +
  geom_step(data = . %>% filter(!is.na(Qin)), direction = "vh",
            aes(y = Qin/400 +1680), col = "goldenrod") 

merged.df.20250602.leafTest.OutputRHline %>% 
  ggplot(aes(x = shareTime, y = RHcham, )) +
  geom_line(data = . %>% filter(!is.na(Flow)))+
  geom_point(data = . %>% filter(!is.na(Flow))) 
  


merged.df.20250602.leafTest.OutputRHline %>% head() %>% select(Protocol, SamplingDate, Licor.Hour,RH_f, CO2_r, elapsed, Time_avg.20250602.leafTest.OutputRHline, obs, Reflectance_avg.20250602.leafTest.OutputRHline, RHcham)


merged.df.20250602.leafTest.OutputRHline %>% 
  ggplot(aes(x = elapsed, y = Time..min., col = Protocol)) +
  geom_abline(slope = 1, intercept =  0) + geom_point(size = 3) +  theme_bw()

merged.df.20250602.leafTest.OutputRHline %>% with(table(Reflectance_590, Tair, RH_f))






## IV. Data analysis ----
##########not jet working############

## __ iv) estimate hysteresis
merged.df.20250602.leafTest.OutputRHline %>%
  ggplot(aes(x = CO2_r, y = Reflectance_590)) +
  # facet_grid( ~ RH_f) + #, scales = "free_y"
  geom_point(size = 3) + #geom_line(aes(group = Cycle), lwd = 1) +
  geom_path() +
  theme_bw() + theme(panel.grid = element_blank())


## colour gradiant
merged.df.20250602.leafTest.OutputRHline %>% 
  filter(Tair_f %in% c(15, 30)) %>% 
  ggplot(aes(x = obs, y = Reflectance_590)) +
  scale_color_gradient(low = "darkorchid4", high = "thistle1") +
  
  facet_grid(~Tair_f, scales = "free_x") +
  geom_line() +
  geom_point(aes(col = CO2_r), size = 3) + 
  theme_bw() + theme(panel.grid = element_blank()) 







# 2025-06-02: silica leaf test----

## silica korrels added to licor chamber to try and compensate for humidity increase
## The dye was applied on an PDMS octopus with ABS incased in it for reflectance

## I. Load Licor CO2_r data ----
co2Cycle.20250602.leafTest.silica <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/LicorDataCSV/2025-06-02-1542_ICOS_leafTest+lightOscillations5min+silica_purpleDye_licor+PDMS+Paper++octopus+multispec.csv",
                                                    fileEncoding="latin1", sep = ";", dec = ".", skip = 13) %>% .[-1,] %>% 
  mutate(SamplingDate = as.Date("2025-06-02"),
         Protocol = "silica leafTest",
         elapsed = as.numeric(sub(",", ".", as.character(elapsed), fixed = TRUE))/60+2, #added +2 to match python data
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

co2Cycle.20250602.leafTest.silica %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = RHcham, col = CO2_r)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "RHcham") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))

co2Cycle.20250602.leafTest.silica %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = Tair)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "Tair (degrees)") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))


co2Cycle.20250602.leafTest.silica %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = CO2_r, col = Tair_f)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "CO2_r (umol/min)") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))



# co2Cycle.20250602.leafTest.silica %>% select(obs, Tair)


## II. Load python data ----## II. Load python CO2_rdata ----
## this data is measured with the multispec recorded by python code AM- CO2 dye 5-kiek

#loads python data
python.data.20250602.leafTest.silica <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-06-02-1542_ICOS_leafTest+lightOscillations5min+silica_purpleDye_licor+PDMS+paper+octopus+multispec.csv",
                                                       sep = ",") 

# Rename PulseNumber to obs for alignment
python.data.20250602.leafTest.silica <- python.data.20250602.leafTest.silica %>%
  rename(obs = PulseNumber)

#add metadata
python.data.20250602.leafTest.silica <- python.data.20250602.leafTest.silica %>% mutate(
  shareTime = Time..min.,
  SamplingDate = as.Date("2025-06-02"),
  Protocol = "silica leafTest",  
  DyeType = "Purple",
  ApplicationMethod = "PDMS+ABS+octopus",
  Filename = basename("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-06-02-1542_ICOS_leafTest+lightOscillations5min+silica_purpleDye_licor+PDMS+paper+octopus+multispec.csv")
)

# python.data.20250602.leafTest.silica %>% head()
##end of one measurment##


python.data.20250602.leafTest.silica %>% 
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_point() +
  geom_line()


## III. Merge Licor and python data ----
## need to do the pairing by time
## Create PairingID -> Need to create a PairingID column that would stick to the LR1 and licor data based on the .Hour

## __ i. data pairing ----

# licor data time pairing
licor.time.20250602.leafTest.silica <- co2Cycle.20250602.leafTest.silica %>%
  select(SamplingDate, Protocol, obs, elapsed) %>%
  arrange(obs)

# licor.time.20250602.leafTest.silica %>% head()

# python data time pairing
python.time.20250602.leafTest.silica <- python.data.20250602.leafTest.silica %>%
  arrange(obs) %>%
  select(SamplingDate, Protocol, obs, Time..min.,)

# python.min.20250602.leafTest.silica %>%  head()


# Join licor_data and python.data by the common 'obs' column
TimePaired.data.20250602.leafTest.silica <- licor.time.20250602.leafTest.silica %>%
  inner_join(python.time.20250602.leafTest.silica, by = "obs") %>%
  arrange(obs)  

# TimePaired.data.20250602.leafTest.silica %>% head()



## Visualize Pairing of measurements
rbind(
  licor.time.20250602.leafTest.silica %>% 
    mutate(Instrument = "Licor") %>% dplyr::rename(Hour = elapsed),
  python.time.20250602.leafTest.silica %>% 
    mutate(Instrument = "python") %>% dplyr::rename(Hour = Time..min.)
) %>% as.data.frame() %>% 
  ggplot(aes(x = Hour, y = 1)) + 
  facet_wrap(Protocol ~., scales = "free_x", ncol = 1) +
  geom_line() +
  geom_point(size = 3, aes(, col = Instrument)) 


TimePaired.data.20250602.leafTest.silica %>% summarise(across(c(elapsed, Time..min.), ~sum(is.na(.))))
## changed licor.hour to elapsed



## __ i. Merge ----
merged.df.20250602.leafTest.silica <- left_join(
  python.data.20250602.leafTest.silica %>% select(-obs), 
  co2Cycle.20250602.leafTest.silica
)
merged.df.20250602.leafTest.silica %>% 
  ggplot(aes(x = shareTime, y = Reflectance_590, )) +
  geom_line() +
  geom_point(data = . %>% filter(!is.na(Tair)),
             aes(col = Tair), size = 3.5)



merged.df.20250602.leafTest.silica %>% head() %>% select(DyeType, Protocol, SamplingDate, Licor.Hour,RH_f, CO2_r, elapsed, Time..min., obs, Reflectance_590, RHcham)


merged.df.20250602.leafTest.silica %>% 
  ggplot(aes(x = elapsed, y = Time..min., col = Protocol)) +
  geom_abline(slope = 1, intercept =  0) + geom_point(size = 3) +  theme_bw()

merged.df.20250602.leafTest.silica %>% with(table(Reflectance_590, Tair, RH_f))



# X 2025-06-02:lag test | CO2 350-450-370 ----

## I'm testing whether the observed lag is a result of the extraims or always present
## The dye was applied on an PDMS octopus with ABS incased in it for reflectance

## I. Load Licor CO2_r data ----
co2Cycle.20250602.lagTest <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/LicorDataCSV/2025-06-02-1656_ICOS_lagTest+350-450-330CO2Oscillations_purpleDye_licor+PDMS+paper+octopus+multispec.csv",
                                                       fileEncoding="latin1", sep = ";", dec = ".", skip = 13) %>% .[-1,] %>% 
  mutate(SamplingDate = as.Date("2025-06-02"),
         Protocol = "350-450-330CO2Oscillations",
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

co2Cycle.20250602.lagTest %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = RHcham, col = CO2_r)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "RHcham") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))

co2Cycle.20250602.lagTest %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = Tair)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "Tair (degrees)") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))


co2Cycle.20250602.lagTest %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = CO2_r, col = Tair_f)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "CO2_r (umol/min)") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))



# co2Cycle.20250602.lagTest %>% select(obs, Tair)


## II. Load python data ----## II. Load python CO2_rdata ----
## this data is measured with the multispec recorded by python code AM- CO2 dye 5-kiek
## Here files are in different folders for each protocol.-->???


##redo/loop for every measurment##
#loads python data
python.data.20250602.lagTest <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-06-02-1656_ICOS_lagTest+350-450-370CO2Oscillations_purpleDye_licor+PDMS+paper+octopus+multispec.csv",
                                                          sep = ",") 

# Rename PulseNumber to obs for alignment
python.data.20250602.lagTest <- python.data.20250602.lagTest %>%
  rename(obs = PulseNumber)

#add metadata
python.data.20250602.lagTest <- python.data.20250602.lagTest %>% mutate(
  SamplingDate = as.Date("2025-06-02"),
  Protocol = "350-450-330CO2Oscillations",  
  DyeType = "Purple",
  ApplicationMethod = "PDMS+ABS+octopus",
  Filename = basename("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-06-02-1656_ICOS_lagTest+350-450-370CO2Oscillations_purpleDye_licor+PDMS+paper+octopus+multispec.csv")
)

# python.data.20250602.lagTest %>% head()
##end of one measurment##


python.data.20250602.lagTest %>% 
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_point() +
  geom_line()


## III. Merge Licor and python data ----
## need to do the pairing by time
## Create PairingID -> Need to create a PairingID column that would stick to the LR1 and licor data based on the .Hour

## __ i. data pairing ----

# licor data time pairing
licor.time.20250602.lagTest <- co2Cycle.20250602.lagTest %>%
  select(SamplingDate, Protocol, obs, elapsed) %>%
  arrange(obs)

# licor.time.20250602.lagTest %>% head()

# python data time pairing
python.time.20250602.lagTest <- python.data.20250602.lagTest %>%
  arrange(obs) %>%
  select(SamplingDate, Protocol, obs, Time..min.,)

# python.min.20250602.lagTest %>%  head()


# Join licor_data and python.data by the common 'obs' column
TimePaired.data.20250602.lagTest <- licor.time.20250602.lagTest %>%
  inner_join(python.time.20250602.lagTest, by = "obs") %>%
  arrange(obs)  

# TimePaired.data.20250602.lagTest %>% head()




## Visualize Pairing of measurements
rbind(
  licor.time.20250602.lagTest %>% 
    mutate(Instrument = "Licor") %>% dplyr::rename(Hour = elapsed),
  python.time.20250602.lagTest %>% 
    mutate(Instrument = "python") %>% dplyr::rename(Hour = Time..min.)
) %>% as.data.frame() %>% 
  ggplot(aes(x = Hour, y = 1)) + 
  facet_wrap(Protocol ~., scales = "free_x", ncol = 1) +
  geom_line() +
  geom_point(size = 3, aes(, col = Instrument)) 


TimePaired.data.20250602.lagTest %>% summarise(across(c(elapsed, Time..min.), ~sum(is.na(.))))
## changed licor.hour to elapsed



## __ i. Merge ----
merged.df.20250602.lagTest <- left_join(python.data.20250602.lagTest, co2Cycle.20250602.lagTest)

merged.df.20250602.lagTest %>% head() %>% select(DyeType, Protocol, SamplingDate, Licor.Hour,RH_f, CO2_r, elapsed, Time..min., obs, Reflectance_590, RHcham)


merged.df.20250602.lagTest %>% 
  ggplot(aes(x = elapsed, y = Time..min., col = Protocol)) +
  geom_abline(slope = 1, intercept =  0) + geom_point(size = 3) +  theme_bw()

merged.df.20250602.lagTest %>% with(table(Reflectance_590, Tair, RH_f))


## IV. Data analysis ----

## __ iv) estimate hysteresis
merged.df.20250602.lagTest %>%
  ggplot(aes(x = CO2_r, y = Reflectance_590)) +
  # facet_grid( ~ RH_f) + #, scales = "free_y"
  geom_point(size = 3) + #geom_line(aes(group = Cycle), lwd = 1) +
  geom_path() +
  theme_bw() + theme(panel.grid = element_blank())



## colour gradiant
merged.df.20250602.lagTest %>% 
  filter(RH_f %in% c(70, 30)) %>% 
  mutate(RH_f = factor(RH_f, levels = c(70, 30))) %>%  
  ggplot(aes(x = obs, y = Reflectance_590)) +
  scale_color_gradient(low = "darkorchid4", high = "thistle1") +
  
  facet_grid(~RH_f, scales = "free_x") +
  geom_line() +
  geom_point(aes(col = CO2_r), size = 3) + 
  theme_bw() + theme(panel.grid = element_blank()) 

merged.df.20250602.lagTest %>% 
  group_by(RH_f) %>% 
  mutate(newObs = obs - min(obs)) %>% 
  ggplot(aes(x = newObs, y = Reflectance_590, lty = RH_f)) +
  scale_color_gradient(low = "darkorchid4", high = "thistle1") +
  
  geom_line() +
  geom_point(aes(col = CO2_r), size = 4) + 
  theme_bw() + theme(panel.grid = element_blank())




























# 2025-06-06:temp 25-26 oscillations ----

## I'm testing whether a single degree of temperature difference has a notable effect
## The dye was applied on an PDMS octopus with ABS incased in it for reflectance

## I. Load Licor CO2_r data ----
co2Cycle.20250606.temp25_26 <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/LicorDataCSV/2025-06-06-1305_ICOS_tempOscillations25-26_purpleDye_licor+PDMS+paper+octopus+multispec.csv",
                                      fileEncoding="latin1", sep = ";", dec = ".", skip = 13) %>% .[-1,] %>% 
  mutate(SamplingDate = as.Date("2025-06-06"),
         Protocol = "temp25-26CO2Oscillations",
         elapsed = as.numeric(sub(",", ".", as.character(elapsed), fixed = TRUE))/60+2, #added +2 to match python data
         # substituding , for . when going from character to numeric.
         CO2_r = as.numeric(sub(",", ".", as.character(CO2_r), fixed = TRUE)),
         CO2_r = ifelse(CO2_r < 0, 0, CO2_r %>% round),
         RHcham = as.numeric(sub(",", ".", as.character(RHcham), fixed = TRUE)),
         RH_f = round(RHcham, digits = 0) %>% as.factor(),
         Tair = as.numeric(sub(",", ".", as.character(Tair), fixed = TRUE)),
         Tair_f = round(Tair, digits = 0) %>% as.factor(),
         shareTime = round(elapsed, digits = 0),
         Licor.Hour = hhmmss %>% myHour.f()) %>% 
  select(SamplingDate, Protocol, Licor.Hour, hhmmss, elapsed, obs, CO2_r, RHcham, RH_f, Tair, Tair_f) 

co2Cycle.20250606.temp25_26 %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = RHcham, col = CO2_r)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "RHcham") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))

co2Cycle.20250606.temp25_26 %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = Tair)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "Tair (degrees)") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))


co2Cycle.20250606.temp25_26 %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = CO2_r, col = Tair_f)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "CO2_r (umol/min)") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))



# co2Cycle.20250606.temp25_26 %>% select(obs, Tair)


## II. Load python data ----## II. Load python CO2_rdata ----
## this data is measured with the multispec recorded by python code AM- CO2 dye 5-kiek
## Here files are in different folders for each protocol.-->???


##redo/loop for every measurment##
#loads python data
python.data.20250606.temp25_26 <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-06-06-1305_ICOS_tempOscillations25-26_purpleDye_licor+PDMS+paper+octopus+multispec.csv",
                                         sep = ",") 

# Rename PulseNumber to obs for alignment
python.data.20250606.temp25_26 <- python.data.20250606.temp25_26 %>%
  rename(obs = PulseNumber)

#add metadata
python.data.20250606.temp25_26 <- python.data.20250606.temp25_26 %>% mutate(
  shareTime = Time..min.,
  SamplingDate = as.Date("2025-06-02"),
  Protocol = "temp25-26CO2Oscillations",  
  DyeType = "Purple",
  ApplicationMethod = "PDMS+ABS+octopus",
  Filename = basename("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-06-06-1305_ICOS_tempOscillations25-26_purpleDye_licor+PDMS+paper+octopus+multispec.csv")
)

# python.data.20250606.temp25_26 %>% head()
##end of one measurment##


python.data.20250606.temp25_26 %>% 
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_point() +
  geom_line()







## III. Merge Licor and python data ----
## need to do the pairing by time
## Create PairingID -> Need to create a PairingID column that would stick to the LR1 and licor data based on the .Hour

## __ i. data pairing ----

# licor data time pairing
licor.time.20250606.temp25_26 <- co2Cycle.20250606.temp25_26 %>%
  select(SamplingDate, Protocol, obs, elapsed) %>%
  arrange(obs)

# licor.time.20250606.temp25_26 %>% head()

# python data time pairing
python.time.20250606.temp25_26 <- python.data.20250606.temp25_26 %>%
  arrange(obs) %>%
  select(SamplingDate, Protocol, obs, Time..min.,)

# python.min.20250606.temp25_26 %>%  head()


# Join licor_data and python.data by the common 'obs' column
TimePaired.data.20250606.temp25_26 <- licor.time.20250606.temp25_26 %>%
  inner_join(python.time.20250606.temp25_26, by = "obs") %>%
  arrange(obs)  

# TimePaired.data.20250606.temp25_26 %>% head()




## Visualize Pairing of measurements
rbind(
  licor.time.20250606.temp25_26 %>% 
    mutate(Instrument = "Licor") %>% dplyr::rename(Hour = elapsed),
  python.time.20250606.temp25_26 %>% 
    mutate(Instrument = "python") %>% dplyr::rename(Hour = Time..min.)
) %>% as.data.frame() %>% 
  ggplot(aes(x = Hour, y = 1)) + 
  facet_wrap(Protocol ~., scales = "free_x", ncol = 1) +
  geom_line() +
  geom_point(size = 3, aes(, col = Instrument)) 


TimePaired.data.20250606.temp25_26 %>% summarise(across(c(elapsed, Time..min.), ~sum(is.na(.))))
## changed licor.hour to elapsed



## __ i. Merge ----
merged.df.20250606.temp25_26 <- left_join(python.data.20250606.temp25_26, co2Cycle.20250606.temp25_26)

merged.df.20250606.temp25_26 %>% head() %>% select(DyeType, Protocol, SamplingDate, Licor.Hour,RH_f, CO2_r, elapsed, Time..min., obs, Reflectance_590, RHcham)


merged.df.20250606.temp25_26 %>% 
  ggplot(aes(x = elapsed, y = Time..min., col = Protocol)) +
  geom_abline(slope = 1, intercept =  0) + geom_point(size = 3) +  theme_bw()

merged.df.20250606.temp25_26 %>% with(table(Reflectance_590, Tair, RH_f))


## IV. Data analysis ----

## __ iv) estimate hysteresis
merged.df.20250606.temp25_26 %>%
  ggplot(aes(x = CO2_r, y = Reflectance_590)) +
  # facet_grid( ~ RH_f) + #, scales = "free_y"
  geom_point(size = 3) + #geom_line(aes(group = Cycle), lwd = 1) +
  geom_path() +
  theme_bw() + theme(panel.grid = element_blank())





merged.df.20250606.temp25_26 <- left_join(
  python.data.20250606.temp25_26 %>% select(-obs), 
  co2Cycle.20250606.temp25_26
)
merged.df.20250606.temp25_26 %>% 
  ggplot(aes(x = shareTime, y = Reflectance_590, )) +
  geom_line() +
  geom_point(data = . %>% filter(!is.na(Tair)),
             aes(col = Tair), size = 3.5)




## colour gradiant
merged.df.20250606.temp25_26 %>% 
  filter(CO2_r %in% c(15, 30)) %>% 
  ggplot(aes(x = obs, y = Reflectance_590)) +
  scale_color_gradient(low = "darkorchid4", high = "thistle1") +
  
  facet_grid(~CO2_r, scales = "free_x") +
  geom_line() +
  geom_point(aes(col = CO2_r), size = 3) + 
  theme_bw() + theme(panel.grid = element_blank()) 























# X 2025-06-12: CO2 cycles | humidities 70% and 30% no breaks ----

## I'm testing thin layer dye application
## The dye was applied an on PDMS octopus with ABS as background
## This measuremtn has as end goal to create to compare to basetest

## I. Load Licor CO2_r data ----
co2Cycle20250612_1131.ThinLayer <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/LicorDataCSV/2025-06-12-1131_lCOS_30-70CO2Oscillations+basetest_purpleDye_licor+ThinLayer+ABS+multispec.csv",
                                  fileEncoding="latin1", sep = ";", dec = ".", skip = 13) %>% .[-1,] %>% 
  mutate(SamplingDate = as.Date("2025-06-12"),
         Protocol = "SS_600-0-500_Cycle_30-70RH no breaks",
         elapsed = as.numeric(sub(",", ".", as.character(elapsed), fixed = TRUE))/60+2, #added +2 to match python data
         # substituding , for . when going from character to numeric.
         CO2_r = as.numeric(sub(",", ".", as.character(CO2_r), fixed = TRUE)),
         CO2_r = ifelse(CO2_r < 0, 0, CO2_r %>% round),
         RHcham = as.numeric(sub(",", ".", as.character(RHcham), fixed = TRUE)),
         RH_f = round(RHcham, digits = 0) %>% as.factor(),
         Licor.Hour = hhmmss %>% myHour.f()) %>% 
  select(SamplingDate, Protocol, Licor.Hour, hhmmss, elapsed, obs, CO2_r, RHcham, RH_f, ) 

co2Cycle20250612_1131.ThinLayer %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = CO2_r, col = RH_f)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "CO2_r (umol/min)") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))


# co2Cycle20250612_1131.ThinLayer %>% head()

## II. Load python data ----
## this data is measured with the multispec recorded by python code AM- CO2 dye 5-kiek
## Here files are in different folders for each protocol.-->???


##redo/loop for every measurment##
#loads python data
python.data20250612_1131.ThinLayer <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-06-12-1131_ICOS_30-70CO2Oscillations+baseTest_purpleDye_licor+thin layer+ABS+multispec.csv",
                                     sep = ",") 

# Rename PulseNumber to obs for alignment
python.data20250612_1131.ThinLayer <- python.data20250612_1131.ThinLayer %>%
  rename(obs = PulseNumber)

#add metadata
python.data20250612_1131.ThinLayer <- python.data20250612_1131.ThinLayer %>% mutate(
  SamplingDate = as.Date("2025-06-12"),
  Protocol = "SS_600-0-500_Cycle_30-70RH no breaks",  
  DyeType = "Purple",
  ApplicationMethod = "PDMS+ABS+octopus",
  Filename = basename("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-06-12-1131_ICOS_30-70CO2Oscillations+baseTest_purpleDye_licor+thin layer+ABS+multispec.csv")
)

# python.data20250612_1131.ThinLayer %>% head()
##end of one measurment##


python.data20250612_1131.ThinLayer %>% 
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_point() +
  geom_line()


## III. Merge Licor and python data ----
## need to do the pairing by time
## Create PairingID -> Need to create a PairingID column that would stick to the LR1 and licor data based on the .Hour

## __ i. data pairing ----

# licor data time pairing
licor.time20250612_1131.ThinLayer <- co2Cycle20250612_1131.ThinLayer %>%
  select(SamplingDate, Protocol, obs, elapsed) %>%
  arrange(obs)

# licor.time20250612_1131.ThinLayer %>% head()

# python data time pairing
python.time20250612_1131.ThinLayer <- python.data20250612_1131.ThinLayer %>%
  arrange(obs) %>%
  select(SamplingDate, Protocol, obs, Time..min.,)

# python.min20250612_1131.ThinLayer %>%  head()


# Join licor_data and python.data by the common 'obs' column
TimePaired.data20250612_1131.ThinLayer <- licor.time20250612_1131.ThinLayer %>%
  inner_join(python.time20250612_1131.ThinLayer, by = "obs") %>%
  arrange(obs)  

# TimePaired.data20250612_1131.ThinLayer %>% head()




## Visualize Pairing of measurements
rbind(
  licor.time20250612_1131.ThinLayer %>% 
    mutate(Instrument = "Licor") %>% dplyr::rename(Hour = elapsed),
  python.time20250612_1131.ThinLayer %>% 
    mutate(Instrument = "python") %>% dplyr::rename(Hour = Time..min.)
) %>% as.data.frame() %>% 
  ggplot(aes(x = Hour, y = 1)) + 
  facet_wrap(Protocol ~., scales = "free_x", ncol = 1) +
  geom_line() +
  geom_point(size = 3, aes(, col = Instrument)) 


TimePaired.data20250612_1131.ThinLayer %>% summarise(across(c(elapsed, Time..min.), ~sum(is.na(.))))
## changed licor.hour to elapsed



## __ i. Merge ----
merged.df20250612_1131.ThinLayer <- left_join(python.data20250612_1131.ThinLayer, co2Cycle20250612_1131.ThinLayer)

merged.df20250612_1131.ThinLayer %>% head() %>% select(DyeType, Protocol, SamplingDate, Licor.Hour,RH_f, CO2_r, elapsed, Time..min., obs, Reflectance_590, RHcham)

merged.df20250612_1131.ThinLayer %>% select(Time..min., elapsed)


merged.df20250612_1131.ThinLayer %>% 
  ggplot(aes(x = elapsed, y = Time..min., col = Protocol)) +
  geom_abline(slope = 1, intercept =  0) + geom_point(size = 3) +  theme_bw()

merged.df20250612_1131.ThinLayer %>% with(table(Reflectance_590, CO2_r, RH_f))


## IV. Data analysis ----

## __ iv) estimate hysteresis
merged.df20250612_1131.ThinLayer %>%
  ggplot(aes(x = CO2_r, y = Reflectance_590,
             col = obs)) +
  facet_grid( ~ RH_f) + #, scales = "free_y"
  geom_point(size = 3) + #geom_line(aes(group = Cycle), lwd = 1) +
  geom_path() +
  theme_bw() + theme(panel.grid = element_blank())



## colour gradiant
merged.df20250612_1131.ThinLayer %>% 
  mutate(RH_f = factor(RH_f, levels = c("70", "30"))) %>% 
  ggplot(aes(x = obs, y = Reflectance_590)) +
  scale_color_gradient(low = "darkorchid4", high = "thistle1") +
  
  facet_grid(~RH_f, scales = "free_x") +
  geom_line() +
  geom_point(aes(col = CO2_r), size = 3) + 
  theme_bw() + theme(panel.grid = element_blank()) 

merged.df20250612_1131.ThinLayer %>% 
  group_by(RH_f) %>% 
  mutate(newObs = obs - min(obs)) %>% 
  ggplot(aes(x = newObs, y = Reflectance_590, lty = RH_f)) +
  scale_color_gradient(low = "darkorchid4", high = "thistle1") +
  
  geom_line() +
  geom_point(aes(col = CO2_r), size = 4) + 
  theme_bw() + theme(panel.grid = element_blank())






# 2025-06-16: CO2 cycles | humidities 70% and 30% no breaks ----

## I'm testing topped of octopus
## The dye was applied an on PDMS octopus with ABS as background incaded by PDMS
## This measuremtn has as end goal to create to compare to basetest

## I. Load Licor CO2_r data ----
co2Cycle20250616_1651.ToppedOffOctopus <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/LicorDataCSV/2025-06-16-1651_lCOS_30-70CO2Oscillations+baseTest_purpleDye_licor+PDMS+ABS+toppedOffOctopus+multispec.csv",
                                            fileEncoding="latin1", sep = ";", dec = ".", skip = 13) %>% .[-1,] %>% 
  mutate(SamplingDate = as.Date("2025-06-16"),
         Protocol = "SS_600-0-500_Cycle_30-70RH no breaks",
         elapsed = as.numeric(sub(",", ".", as.character(elapsed), fixed = TRUE))/60+2, #added +2 to match python data
         # substituding , for . when going from character to numeric.
         CO2_r = as.numeric(sub(",", ".", as.character(CO2_r), fixed = TRUE)),
         CO2_r = ifelse(CO2_r < 0, 0, CO2_r %>% round),
         RHcham = as.numeric(sub(",", ".", as.character(RHcham), fixed = TRUE)),
         RH_f = round(RHcham, digits = 0) %>% as.factor(),
         Licor.Hour = hhmmss %>% myHour.f()) %>% 
  select(SamplingDate, Protocol, Licor.Hour, hhmmss, elapsed, obs, CO2_r, RHcham, RH_f, ) 

co2Cycle20250616_1651.ToppedOffOctopus %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = CO2_r, col = RH_f)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "CO2_r (umol/min)") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))


# co2Cycle20250616_1651.ToppedOffOctopus %>% head()

## II. Load python data ----
## this data is measured with the multispec recorded by python code AM- CO2 dye 5-kiek
## Here files are in different folders for each protocol.-->???


##redo/loop for every measurment##
#loads python data
python.data20250616_1651.ToppedOffOctopus <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-06-16-1651_ICOS_30-70CO2Oscillations+baseTest_purpleDye_licor+PDMS+ABS+topedOffOctopus+multispec.csv",
                                               sep = ",") 

# Rename PulseNumber to obs for alignment
python.data20250616_1651.ToppedOffOctopus <- python.data20250616_1651.ToppedOffOctopus %>%
  rename(obs = PulseNumber)

#add metadata
python.data20250616_1651.ToppedOffOctopus <- python.data20250616_1651.ToppedOffOctopus %>% mutate(
  SamplingDate = as.Date("2025-06-16"),
  Protocol = "SS_600-0-500_Cycle_30-70RH no breaks",  
  DyeType = "Purple",
  ApplicationMethod = "PDMS+ABS+octopus+topped off",
  Filename = basename("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-06-16-1651_ICOS_30-70CO2Oscillations+baseTest_purpleDye_licor+PDMS+ABS+topedOffOctopus+multispec.csv")
)

# python.data20250616_1651.ToppedOffOctopus %>% head()
##end of one measurment##


python.data20250616_1651.ToppedOffOctopus %>% 
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_point() +
  geom_line()


## III. Merge Licor and python data ----
## need to do the pairing by time
## Create PairingID -> Need to create a PairingID column that would stick to the LR1 and licor data based on the .Hour

## __ i. data pairing ----

# licor data time pairing
licor.time20250616_1651.ToppedOffOctopus <- co2Cycle20250616_1651.ToppedOffOctopus %>%
  select(SamplingDate, Protocol, obs, elapsed) %>%
  arrange(obs)

# licor.time20250616_1651.ToppedOffOctopus %>% head()

# python data time pairing
python.time20250616_1651.ToppedOffOctopus <- python.data20250616_1651.ToppedOffOctopus %>%
  arrange(obs) %>%
  select(SamplingDate, Protocol, obs, Time..min.,)

# python.min20250616_1651.ToppedOffOctopus %>%  head()


# Join licor_data and python.data by the common 'obs' column
TimePaired.data20250616_1651.ToppedOffOctopus <- licor.time20250616_1651.ToppedOffOctopus %>%
  inner_join(python.time20250616_1651.ToppedOffOctopus, by = "obs") %>%
  arrange(obs)  

# TimePaired.data20250616_1651.ToppedOffOctopus %>% head()




## Visualize Pairing of measurements
rbind(
  licor.time20250616_1651.ToppedOffOctopus %>% 
    mutate(Instrument = "Licor") %>% dplyr::rename(Hour = elapsed),
  python.time20250616_1651.ToppedOffOctopus %>% 
    mutate(Instrument = "python") %>% dplyr::rename(Hour = Time..min.)
) %>% as.data.frame() %>% 
  ggplot(aes(x = Hour, y = 1)) + 
  facet_wrap(Protocol ~., scales = "free_x", ncol = 1) +
  geom_line() +
  geom_point(size = 3, aes(, col = Instrument)) 


TimePaired.data20250616_1651.ToppedOffOctopus %>% summarise(across(c(elapsed, Time..min.), ~sum(is.na(.))))
## changed licor.hour to elapsed



## __ i. Merge ----
merged.df20250616_1651.ToppedOffOctopus <- left_join(python.data20250616_1651.ToppedOffOctopus, co2Cycle20250616_1651.ToppedOffOctopus)

merged.df20250616_1651.ToppedOffOctopus %>% head() %>% select(DyeType, Protocol, SamplingDate, Licor.Hour,RH_f, CO2_r, elapsed, Time..min., obs, Reflectance_590, RHcham)

merged.df20250616_1651.ToppedOffOctopus %>% select(Time..min., elapsed)


merged.df20250616_1651.ToppedOffOctopus %>% 
  ggplot(aes(x = elapsed, y = Time..min., col = Protocol)) +
  geom_abline(slope = 1, intercept =  0) + geom_point(size = 3) +  theme_bw()

merged.df20250616_1651.ToppedOffOctopus %>% with(table(Reflectance_590, CO2_r, RH_f))


## IV. Data analysis ----

## __ iv) estimate hysteresis
merged.df20250616_1651.ToppedOffOctopus %>%
  ggplot(aes(x = CO2_r, y = Reflectance_590,
             col = obs)) +
  facet_grid( ~ RH_f) + #, scales = "free_y"
  geom_point(size = 3) + #geom_line(aes(group = Cycle), lwd = 1) +
  geom_path() +
  theme_bw() + theme(panel.grid = element_blank())



## colour gradiant
merged.df20250616_1651.ToppedOffOctopus %>% 
  mutate(RH_f = factor(RH_f, levels = c("70", "30"))) %>% 
  ggplot(aes(x = obs, y = Reflectance_590)) +
  scale_color_gradient(low = "darkorchid4", high = "thistle1") +
  
  facet_grid(~RH_f, scales = "free_x") +
  geom_line() +
  geom_point(aes(col = CO2_r), size = 3) + 
  theme_bw() + theme(panel.grid = element_blank()) 

merged.df20250616_1651.ToppedOffOctopus %>% 
  group_by(RH_f) %>% 
  mutate(newObs = obs - min(obs)) %>% 
  ggplot(aes(x = newObs, y = Reflectance_590, lty = RH_f)) +
  scale_color_gradient(low = "darkorchid4", high = "thistle1") +
  
  geom_line() +
  geom_point(aes(col = CO2_r), size = 4) + 
  theme_bw() + theme(panel.grid = element_blank())







# XX 2025-06-20: CO2 cycles | humidities 70% and 30% no breaks ----

## I'm testing thin layer dye application
## The dye was applied an on PDMS octopus with ABS as background
## This measuremtn has as end goal to create to compare to basetest

## I. Load Licor CO2_r data ----
co2Cycle20250616_1316.ThinLayer <- read.csv(,
                                            fileEncoding="latin1", sep = ";", dec = ".", skip = 13) %>% .[-1,] %>% 
  mutate(SamplingDate = as.Date("2025-06-16"),
         Protocol = "SS_600-0-500_Cycle_30-70RH no breaks",
         elapsed = as.numeric(sub(",", ".", as.character(elapsed), fixed = TRUE))/60+2, #added +2 to match python data
         # substituding , for . when going from character to numeric.
         CO2_r = as.numeric(sub(",", ".", as.character(CO2_r), fixed = TRUE)),
         CO2_r = ifelse(CO2_r < 0, 0, CO2_r %>% round),
         RHcham = as.numeric(sub(",", ".", as.character(RHcham), fixed = TRUE)),
         RH_f = round(RHcham, digits = 0) %>% as.factor(),
         Licor.Hour = hhmmss %>% myHour.f()) %>% 
  select(SamplingDate, Protocol, Licor.Hour, hhmmss, elapsed, obs, CO2_r, RHcham, RH_f, ) 

co2Cycle20250616_1316.ThinLayer %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = CO2_r, col = RH_f)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "CO2_r (umol/min)") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))


# co2Cycle20250616_1316.ThinLayer %>% head()

## II. Load python data ----
## this data is measured with the multispec recorded by python code AM- CO2 dye 5-kiek
## Here files are in different folders for each protocol.-->???


##redo/loop for every measurment##
#loads python data
python.data20250616_1316.ThinLayer <- read.csv(..........,
                                               sep = ",") 

# Rename PulseNumber to obs for alignment
python.data20250616_1316.ThinLayer <- python.data20250616_1316.ThinLayer %>%
  rename(obs = PulseNumber)

#add metadata
python.data20250616_1316.ThinLayer <- python.data20250616_1316.ThinLayer %>% mutate(
  SamplingDate = as.Date("2025-06-16"),
  Protocol = "SS_600-0-500_Cycle_30-70RH no breaks",  
  DyeType = "Purple",
  ApplicationMethod = "Paper",
  Filename = basename(..............)
)

# python.data20250616_1316.ThinLayer %>% head()
##end of one measurment##


python.data20250616_1316.ThinLayer %>%
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_point() +
  geom_line()


## III. Merge Licor and python data ----
## need to do the pairing by time
## Create PairingID -> Need to create a PairingID column that would stick to the LR1 and licor data based on the .Hour

## __ i. data pairing ----

# licor data time pairing
licor.time20250616_1316.ThinLayer <- co2Cycle20250616_1316.ThinLayer %>%
  select(SamplingDate, Protocol, obs, elapsed) %>%
  arrange(obs)

# licor.time20250616_1316.ThinLayer %>% head()

# python data time pairing
python.time20250616_1316.ThinLayer <- python.data20250616_1316.ThinLayer %>%
  arrange(obs) %>%
  select(SamplingDate, Protocol, obs, Time..min.,)

# python.min20250616_1316.ThinLayer %>%  head()


# Join licor_data and python.data by the common 'obs' column
TimePaired.data20250616_1316.ThinLayer <- licor.time20250616_1316.ThinLayer %>%
  inner_join(python.time20250616_1316.ThinLayer, by = "obs") %>%
  arrange(obs)  

# TimePaired.data20250616_1316.ThinLayer %>% head()




## Visualize Pairing of measurements
rbind(
  licor.time20250616_1316.ThinLayer %>% 
    mutate(Instrument = "Licor") %>% dplyr::rename(Hour = elapsed),
  python.time20250616_1316.ThinLayer %>% 
    mutate(Instrument = "python") %>% dplyr::rename(Hour = Time..min.)
) %>% as.data.frame() %>% 
  ggplot(aes(x = Hour, y = 1)) + 
  facet_wrap(Protocol ~., scales = "free_x", ncol = 1) +
  geom_line() +
  geom_point(size = 3, aes(, col = Instrument)) 


TimePaired.data20250616_1316.ThinLayer %>% summarise(across(c(elapsed, Time..min.), ~sum(is.na(.))))
## changed licor.hour to elapsed



## __ i. Merge ----
merged.df20250616_1316.ThinLayer <- left_join(python.data20250616_1316.ThinLayer, co2Cycle20250616_1316.ThinLayer)

merged.df20250616_1316.ThinLayer %>% head() %>% select(DyeType, Protocol, SamplingDate, Licor.Hour,RH_f, CO2_r, elapsed, Time..min., obs, Reflectance_590, RHcham)

merged.df20250616_1316.ThinLayer %>% select(Time..min., elapsed)


merged.df20250616_1316.ThinLayer %>% 
  ggplot(aes(x = elapsed, y = Time..min., col = Protocol)) +
  geom_abline(slope = 1, intercept =  0) + geom_point(size = 3) +  theme_bw()

merged.df20250616_1316.ThinLayer %>% with(table(Reflectance_590, CO2_r, RH_f))


## IV. Data analysis ----

## __ iv) estimate hysteresis
merged.df20250616_1316.ThinLayer %>%
  ggplot(aes(x = CO2_r, y = Reflectance_590,
             col = obs)) +
  facet_grid( ~ RH_f) + #, scales = "free_y"
  geom_point(size = 3) + #geom_line(aes(group = Cycle), lwd = 1) +
  geom_path() +
  theme_bw() + theme(panel.grid = element_blank())



## colour gradiant
merged.df20250616_1316.ThinLayer %>% 
  mutate(RH_f = factor(RH_f, levels = c("70", "30"))) %>% 
  ggplot(aes(x = obs, y = Reflectance_590)) +
  scale_color_gradient(low = "darkorchid4", high = "thistle1") +
  
  facet_grid(~RH_f, scales = "free_x") +
  geom_line() +
  geom_point(aes(col = CO2_r), size = 3) + 
  theme_bw() + theme(panel.grid = element_blank()) 

merged.df20250616_1316.ThinLayer %>% 
  group_by(RH_f) %>% 
  mutate(newObs = obs - min(obs)) %>% 
  ggplot(aes(x = newObs, y = Reflectance_590, lty = RH_f)) +
  scale_color_gradient(low = "darkorchid4", high = "thistle1") +
  
  geom_line() +
  geom_point(aes(col = CO2_r), size = 4) + 
  theme_bw() + theme(panel.grid = element_blank())






# 2025-06-20: CO2 cycles | humidities 70% and 30% no breaks ----

## I'm testing small oscilation steps
## The dye was applied an on PDMS octopus with paper as background
## This measuremtn has as end goal to create to compare to basetest

## I. Load Licor CO2_r data ----
co2Cycle20250616_1316.ThinLayer <- read.csv(..........,
                                            fileEncoding="latin1", sep = ";", dec = ".", skip = 13) %>% .[-1,] %>% 
  mutate(SamplingDate = as.Date("2025-06-20"),
         Protocol = "SmallStepsCO2Oscillations 350-450",
         elapsed = as.numeric(sub(",", ".", as.character(elapsed), fixed = TRUE))/60+2, #added +2 to match python data
         # substituding , for . when going from character to numeric.
         CO2_r = as.numeric(sub(",", ".", as.character(CO2_r), fixed = TRUE)),
         CO2_r = ifelse(CO2_r < 0, 0, CO2_r %>% round),
         RHcham = as.numeric(sub(",", ".", as.character(RHcham), fixed = TRUE)),
         RH_f = round(RHcham, digits = 0) %>% as.factor(),
         Licor.Hour = hhmmss %>% myHour.f()) %>% 
  select(SamplingDate, Protocol, Licor.Hour, hhmmss, elapsed, obs, CO2_r, RHcham, RH_f, ) 

co2Cycle20250616_1316.ThinLayer %>% 
  mutate(Time =  (Licor.Hour -  Licor.Hour[1])) %>% 
  
  ggplot(aes(x = elapsed, y = CO2_r, col = RH_f)) +
  # geom_step()+
  geom_point()+
  geom_step(direction = "mid") +
  scale_x_continuous(name = "Time (min)") + 
  scale_y_continuous(name = "CO2_r (umol/min)") +
  theme_bw() + theme(panel.grid = element_blank(),
                     legend.position.inside = c(.1,.9))


# co2Cycle20250616_1316.ThinLayer %>% head()

## II. Load python data ----
## this data is measured with the multispec recorded by python code AM- CO2 dye 5-kiek
## Here files are in different folders for each protocol.-->???


##redo/loop for every measurment##
#loads python data
python.data20250616_1316.ThinLayer <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-06-20-1612_ICOS_SmallStepsCO2Oscillations_purpleDye_licor+PDMS+paper+multispec.csv",
                                               sep = ",") 

# Rename PulseNumber to obs for alignment
python.data20250616_1316.ThinLayer <- python.data20250616_1316.ThinLayer %>%
  rename(obs = PulseNumber)

#add metadata
python.data20250616_1316.ThinLayer <- python.data20250616_1316.ThinLayer %>% mutate(
  SamplingDate = as.Date("2025-06-20"),
  Protocol = "SmallStepsCO2Oscillations 350-450",  
  DyeType = "Purple",
  ApplicationMethod = "PDMS+Paper+octopus",
  Filename = basename("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-06-20-1612_ICOS_SmallStepsCO2Oscillations_purpleDye_licor+PDMS+paper+multispec.csv")
)

# python.data20250616_1316.ThinLayer %>% head()
##end of one measurment##


python.data20250616_1316.ThinLayer %>%
  ggplot(aes(x = Time..min., y = Reflectance_590)) +
  geom_point() +
  geom_line()


## III. Merge Licor and python data ----
## need to do the pairing by time
## Create PairingID -> Need to create a PairingID column that would stick to the LR1 and licor data based on the .Hour

## __ i. data pairing ----

# licor data time pairing
licor.time20250616_1316.ThinLayer <- co2Cycle20250616_1316.ThinLayer %>%
  select(SamplingDate, Protocol, obs, elapsed) %>%
  arrange(obs)

# licor.time20250616_1316.ThinLayer %>% head()

# python data time pairing
python.time20250616_1316.ThinLayer <- python.data20250616_1316.ThinLayer %>%
  arrange(obs) %>%
  select(SamplingDate, Protocol, obs, Time..min.,)

# python.min20250616_1316.ThinLayer %>%  head()


# Join licor_data and python.data by the common 'obs' column
TimePaired.data20250616_1316.ThinLayer <- licor.time20250616_1316.ThinLayer %>%
  inner_join(python.time20250616_1316.ThinLayer, by = "obs") %>%
  arrange(obs)  

# TimePaired.data20250616_1316.ThinLayer %>% head()




## Visualize Pairing of measurements
rbind(
  licor.time20250616_1316.ThinLayer %>% 
    mutate(Instrument = "Licor") %>% dplyr::rename(Hour = elapsed),
  python.time20250616_1316.ThinLayer %>% 
    mutate(Instrument = "python") %>% dplyr::rename(Hour = Time..min.)
) %>% as.data.frame() %>% 
  ggplot(aes(x = Hour, y = 1)) + 
  facet_wrap(Protocol ~., scales = "free_x", ncol = 1) +
  geom_line() +
  geom_point(size = 3, aes(, col = Instrument)) 


TimePaired.data20250616_1316.ThinLayer %>% summarise(across(c(elapsed, Time..min.), ~sum(is.na(.))))
## changed licor.hour to elapsed



## __ i. Merge ----
merged.df20250616_1316.ThinLayer <- left_join(python.data20250616_1316.ThinLayer, co2Cycle20250616_1316.ThinLayer)

merged.df20250616_1316.ThinLayer %>% head() %>% select(DyeType, Protocol, SamplingDate, Licor.Hour,RH_f, CO2_r, elapsed, Time..min., obs, Reflectance_590, RHcham)

merged.df20250616_1316.ThinLayer %>% select(Time..min., elapsed)


merged.df20250616_1316.ThinLayer %>% 
  ggplot(aes(x = elapsed, y = Time..min., col = Protocol)) +
  geom_abline(slope = 1, intercept =  0) + geom_point(size = 3) +  theme_bw()

merged.df20250616_1316.ThinLayer %>% with(table(Reflectance_590, CO2_r, RH_f))


## IV. Data analysis ----

## __ iv) estimate hysteresis
merged.df20250616_1316.ThinLayer %>%
  ggplot(aes(x = CO2_r, y = Reflectance_590,
             col = obs)) +
  facet_grid( ~ RH_f) + #, scales = "free_y"
  geom_point(size = 3) + #geom_line(aes(group = Cycle), lwd = 1) +
  geom_path() +
  theme_bw() + theme(panel.grid = element_blank())



## colour gradiant
merged.df20250616_1316.ThinLayer %>% 
  mutate(RH_f = factor(RH_f, levels = c("70", "30"))) %>% 
  ggplot(aes(x = obs, y = Reflectance_590)) +
  scale_color_gradient(low = "darkorchid4", high = "thistle1") +
  
  facet_grid(~RH_f, scales = "free_x") +
  geom_line() +
  geom_point(aes(col = CO2_r), size = 3) + 
  theme_bw() + theme(panel.grid = element_blank()) 

merged.df20250616_1316.ThinLayer %>% 
  group_by(RH_f) %>% 
  mutate(newObs = obs - min(obs)) %>% 
  ggplot(aes(x = newObs, y = Reflectance_590, lty = RH_f)) +
  scale_color_gradient(low = "darkorchid4", high = "thistle1") +
  
  geom_line() +
  geom_point(aes(col = CO2_r), size = 4) + 
  theme_bw() + theme(panel.grid = element_blank())










###histeresis calculations, revisit later----
# CO2Refl.rel800 %>% 
#   filter(Cycle != 7) %>% 
#   group_by(SamplingDate, Cycle, Tair_C, obs, Licor.Hour, CO2_r, CO2level ) %>% 
#   summarize(Purpleness = Rel.Rfl[Wavelength == 585] - Rel.Rfl[Wavelength == 620]) %>% 
#   group_by(Cycle) %>% 
#   mutate(Up = ifelse(Licor.Hour <= Licor.Hour[CO2level == 800], "Yes", "No"),
#          Down = ifelse(Licor.Hour >= Licor.Hour[CO2level == 800] | Licor.Hour == Licor.Hour[CO2level == 0], "Yes", "No")) %>% 
#   summarize(
#     AreaUp = pracma::trapz(CO2_r[Up == "Yes"], Purpleness[Up == "Yes"]) %>% abs,
#     AreaDown = pracma::trapz(CO2_r[Down == "Yes"], Purpleness[Down == "Yes"]) %>% abs,
#     Hysteresis = (AreaDown - AreaUp)
#   )# %>% 
#   ggplot(aes(x = Cycle %>% as.character %>% as.numeric, y = Hysteresis)) +
#   geom_point() + geom_line()
#   
#       ## COnfirm area of 2
#   

