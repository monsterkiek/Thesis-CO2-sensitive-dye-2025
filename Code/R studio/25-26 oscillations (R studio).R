# 2025-06-06:temp 25-26 oscillations ----

## I'm testing whether a single degree of temperature difference has a notable effect
## The dye was applied on an PDMS octopus with ABS incased in it for reflectance

## I. Load Licor CO2_r data ----
co2Cycle.20250606.temp25_26 <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/LicorDataCSV/2025-06-06-1305_ICOS_tempOscillations25-26_purpleDye_licor+PDMS+paper+octopus+multispec.csv",
                                               fileEncoding="latin1", sep = ";", dec = ".", skip = 13) %>% .[-1,] %>% 
  mutate(SamplingDate = as.Date("2025-06-06"),
         Protocol = "temp25-26CO2Oscillation",
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



## II. Load python data ----## II. Load python CO2_rdata ----
## this data is measured with the multispec recorded by python code AM- CO2 dye 5-kiek

#loads python data
python.data.20250606.temp25_26 <- read.csv("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-06-06-1305_ICOS_tempOscillations25-26_purpleDye_licor+PDMS+paper+octopus+multispec.csv",
                                                  sep = ",") 

# Rename PulseNumber to obs for alignment
python.data.20250606.temp25_26 <- python.data.20250606.temp25_26 %>%
  rename(obs = PulseNumber)

#add metadata
python.data.20250606.temp25_26 <- python.data.20250606.temp25_26 %>% mutate(
  shareTime = Time..min.,
  SamplingDate = as.Date("2025-06-06"),
  Protocol = "temp25-26CO2Oscillation",  
  DyeType = "Purple",
  ApplicationMethod = "PDMS+ABS+octopus",
  Filename = basename("C:/Users/nikit/Stichting Jan IngenHousz Institute/JII RESEARCH - Nikita's BSc Project/DataAnalysis&Data/DataAnalysis/PythonDataAnalyse/2025-06-06-1305_ICOS_tempOscillations25-26_purpleDye_licor+PDMS+paper+octopus+multispec.csv")
)



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
merged.df.20250606.temp25_26 <- left_join(
  python.data.20250606.temp25_26 %>% select(-obs), 
  co2Cycle.20250606.temp25_26
)
merged.df.20250606.temp25_26 %>% 
  mutate(roll.Refl_590 = zoo::rollmean(Reflectance_590, 50, na.pad = T)) %>% 
  ggplot(aes(x = shareTime, y = roll.Refl_590)) +
  geom_line(aes()) +
  geom_point(data = . %>% filter(!is.na(Tair)),
             aes(col = Tair), size = 3.5) +
    scale_y_continuous(name = "Reflectance (590 nm)") +
    scale_x_continuous(name = "Time (min)", limits = c(5, 25)) 



merged.df.20250606.temp25_26 %>%
  mutate(roll.Refl_590 = zoo::rollmean(Reflectance_590, 50, na.pad = TRUE)) %>%
  ggplot(aes(x = shareTime, y = roll.Refl_590)) +
  geom_line() +
  geom_point(data = . %>% filter(!is.na(Tair)),
             aes(col = Tair), size = 3.5) +
  scale_y_continuous(name = "Reflectance (590 nm)") +
  scale_x_continuous(name = "Time (min)", limits = c(5, 25)) +
  scale_color_gradient(
    name = "Tair (°C)",
    low = "#8B0000",  # Dark Red (close to brick)
    high = "#FFA07A"  # Light Salmon for warm tone
  )


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



##########not jet working############
## colour gradiant
merged.df.20250606.temp25_26 %>% 
  filter(Tair_f %in% c(15, 30)) %>% 
  ggplot(aes(x = obs, y = Reflectance_590)) +
  scale_color_gradient(low = "darkorchid4", high = "thistle1") +
  
  facet_grid(~Tair_f, scales = "free_x") +
  geom_line() +
  geom_point(aes(col = CO2_r), size = 3) + 
  theme_bw() + theme(panel.grid = element_blank()) 







