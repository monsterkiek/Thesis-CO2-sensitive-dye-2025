##                        ##
## ICOS - CO2 dye sensor  ##
##  Test Data Analysis    ##
##                        ##

## This file contains all data collected using a real leaf well oscillating light


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



# 2025-05-29: leaf test, plateau + light, short, direct light oscillations----

## first flow on to ensure dye stable, then flow and let the dye reatch a plateau then oscillation light, so i would expect indents when light on from photosynthesis thus co2 absorption
## The dye was applied on an PDMS octopus with ABS incased in it for reflectance
## direct after stabilisation start oscillations, already before plateau fully formed. This is to check if the teh lowering of teh total during the first plateau measuremtn test didn't scruw the persieved photosynthesis.


getwd()
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
  
  geom_step(data = . %>% filter(!is.na(Qin)), direction = "vh",
            aes(y = Qin + 13900), col = "goldenrod", size = 1)
  #geom_point(data = . %>% filter(!is.na(Qin)), 
  #           aes(y = Qin + 13900), col = "goldenrod", size = 1)










merged.df.20250529.plateau.leafTest.direct %>% head() %>% select(DyeType, Protocol, SamplingDate, Licor.Hour, elapsed, Time..min., obs, Reflectance_590, CO2_r, RHcham, RH_f, Tair, Tair_f, Qin, Flow)

merged.df.20250529.plateau.leafTest.direct %>% 
  ggplot(aes(x = elapsed, y = Time..min., col = Protocol)) +
  geom_abline(slope = 1, intercept =  0) + geom_point(size = 3) +  theme_bw()

merged.df.20250529.plateau.leafTest.direct %>% with(table(Reflectance_590, Tair, RH_f))


## Mauris code for nicer plot ----
DarkIntervals <- merged.df.20250529.plateau.leafTest.direct %>% 
  filter(!is.na(Qin)) %>% 
  mutate(Lights = Qin < 2,
         LightPulse = cumsum(!Lights & lag(Lights, default=TRUE))) %>% 
  group_by(LightPulse) %>% 
  summarize(Dark.strt = min(shareTime), 
            Dark.end = max(shareTime), )


merged.df.20250529.plateau.leafTest.direct %>% 
  
  ggplot()+
  
  geom_rect(data = DarkIntervals, inherit.aes =  F,
            aes(group = LightPulse,
                xmin = Dark.strt, xmax = Dark.end, 
                ymin = -Inf, ymax = Inf), fill = "grey") +
  
  geom_line(aes(x = shareTime , y = zoo::rollmean(Reflectance_590, 10, na.pad = T), ),
            col = "black") +
  # geom_point(data = . %>% filter(!is.na(Qin)),
  #            aes(x = shareTime, y = Qin)) + 
  

  scale_y_continuous(name = "Reflectance (590nm)", limits = c(2235, 2325)) +
  scale_x_continuous(name = "Time (min)", limits = c(8, 30),
                     breaks = seq(10, 30, 4), labels = seq(0, 20, 4)) +
  theme_bw() + theme(panel.grid = element_blank()) 
  






merged.df.20250529.plateau.leafTest.direct %>% 
  
  ggplot()+
  
  geom_rect(data = DarkIntervals, inherit.aes =  F,
            aes(group = LightPulse,
                xmin = Dark.strt, xmax = Dark.end, 
                ymin = -Inf, ymax = Inf), fill = "grey") +
  
  geom_line(aes(x = shareTime , y = zoo::rollmean(Reflectance_590, 10, na.pad = T), ),
            col = "black") +
  # geom_point(data = . %>% filter(!is.na(Qin)),
  #            aes(x = shareTime, y = Qin)) + 
  
  
  scale_y_continuous(name = "Reflectance (590nm)", limits = c(2235, 2343)) +
  scale_x_continuous(name = "Time (min)", limits = c(8, 30),
                     breaks = seq(10, 30, 4), labels = seq(0, 20, 4)) +
  theme_bw() + theme(panel.grid = element_blank()) 













  
geom_step(data = . %>% filter(!is.na(CO2_r)), direction = "vh",
          aes(x = shareTime, y = CO2_r*1 + 13000), col = "slategray") +
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
             aes(y = RHcham*5 + 12400), col = "steelblue", size = 0.5) +
  





  
  
  
  






# 2025-06-02: leaf test, light oscillations 2min----

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
         CO2_s = as.numeric(sub(",", ".", as.character(CO2_s), fixed = TRUE)),
         Qin = as.numeric(sub(",", ".", as.character(Qin), fixed = TRUE)),
         Flow = as.numeric(sub(",", ".", as.character(Flow), fixed = TRUE)),
         Fan_speed = as.numeric(sub(",", ".", as.character(Fan_speed), fixed = TRUE)),
         RHcham = as.numeric(sub(",", ".", as.character(RHcham), fixed = TRUE)),
         RH_f = round(RHcham, digits = 0) %>% as.factor(),
         Tair = as.numeric(sub(",", ".", as.character(Tair), fixed = TRUE)),
         Tair_f = round(Tair, digits = 0) %>% as.factor(),
         shareTime = round(elapsed, digits = 0),
         Licor.Hour = hhmmss %>% myHour.f()) %>% 
  select(SamplingDate, Protocol, Licor.Hour, hhmmss, elapsed, obs, CO2_s, CO2_r, RHcham, RH_f, Tair, Tair_f, Qin, Flow, Fan_speed, shareTime) 


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
         CO2_s = as.numeric(sub(",", ".", as.character(CO2_s), fixed = TRUE)),
         Fan_speed = as.numeric(sub(",", ".", as.character(Fan_speed), fixed = TRUE)),
         RHcham = as.numeric(sub(",", ".", as.character(RHcham), fixed = TRUE)),
         RH_f = round(RHcham, digits = 0) %>% as.factor(),
         Tair = as.numeric(sub(",", ".", as.character(Tair), fixed = TRUE)),
         Tair_f = round(Tair, digits = 0) %>% as.factor(),
         shareTime = round(elapsed, digits = 0),
         Licor.Hour = hhmmss %>% myHour.f()) %>% 
  select(SamplingDate, Protocol, Licor.Hour, hhmmss, elapsed, obs, CO2_r, CO2_s, RHcham, RH_f, Tair, Tair_f, Qin, Flow, Fan_speed, shareTime) 


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









### Mauris code for plotting ----

merged.df <- left_join(
  python.data.20250602.leafTest.OutputRHline %>% 
    select(shareTime, Reflectance_590),
  co2Cycle.20250602.leafTest.OutputRHline %>% 
    select(CO2_r, CO2_s, RHcham, Tair, Qin, shareTime) %>% 
    filter(shareTime >= 6)
)

DarkIntervals <- merged.df %>% 
  filter(!is.na(Qin)) %>% 
  mutate(Lights = Qin < 2,
         LightPulse = cumsum(!Lights & lag(Lights, default=TRUE))) %>% 
  group_by(LightPulse) %>% 
  summarize(Dark.strt = min(shareTime), 
            Dark.end = max(shareTime), )
  
  Rfl.plt <- merged.df %>%
    filter(shareTime >= 6) %>%  # ensure filtering is applied here too
    mutate(Roll590 = zoo::rollmean(Reflectance_590, 10, na.pad = TRUE)) %>%
    ggplot() +
    geom_rect(data = DarkIntervals, inherit.aes = FALSE,
              aes(group = LightPulse,
                  xmin = Dark.strt, xmax = Dark.end,
                  ymin = -Inf, ymax = Inf), fill = "grey") +
    geom_line(aes(x = shareTime, y = Roll590), col = "black") +
    scale_y_continuous(name = "Reflectance (590nm)", limits = c(1640, 1655)) +
    scale_x_continuous(name = NULL) +
    theme_bw() + theme(panel.grid = element_blank())
  

CO2_s.plt <- merged.df %>% 
  filter(!is.na(Qin)) %>% 
  ggplot() +
  geom_rect(data = DarkIntervals, inherit.aes =  F,
            aes(group = LightPulse,
                xmin = Dark.strt, xmax = Dark.end, 
                ymin = -Inf, ymax = Inf), fill = "grey") +
  
  geom_point(aes(x = shareTime , y = CO2_s),
             col = "slategray") +
  geom_line(aes(x = shareTime , y = CO2_s),
            col = "slategray") +
  
  scale_y_continuous(name = "sample [CO2] (ppm)") +
  scale_x_continuous(name = NULL) +
  theme_bw() + theme(panel.grid = element_blank())

RH.plt <- merged.df %>% 
  filter(!is.na(Qin)) %>% 
  ggplot() +
  geom_rect(data = DarkIntervals, inherit.aes =  F,
            aes(group = LightPulse,
                xmin = Dark.strt, xmax = Dark.end, 
                ymin = -Inf, ymax = Inf), fill = "grey") +
  
  geom_point(aes(x = shareTime , y = RHcham),
             col = "steelblue") +
  geom_line(aes(x = shareTime , y = RHcham),
            col = "steelblue") +
  
  scale_y_continuous(name = "Rel. Humidity (%)") +
  scale_x_continuous(name = NULL) +
  theme_bw() + theme(panel.grid = element_blank())


Temp.plt <- merged.df %>% 
  filter(!is.na(Qin)) %>% 
  ggplot() +
  geom_rect(data = DarkIntervals, inherit.aes =  F,
            aes(group = LightPulse,
                xmin = Dark.strt, xmax = Dark.end, 
                ymin = -Inf, ymax = Inf), fill = "grey") +
  
  geom_point(aes(x = shareTime , y = Tair),
             col = "firebrick") +
  geom_line(aes(x = shareTime , y = Tair),
            col = "firebrick") +
  
  scale_y_continuous(name = "Chamber temperature (°C)", limits = c(25.5, 27.5)) +
  scale_x_continuous(name = "Time (min)") +
  theme_bw() + theme(panel.grid = element_blank())



cowplot::plot_grid(Rfl.plt, CO2_s.plt, RH.plt, Temp.plt, ncol = 1, align = "hv")








