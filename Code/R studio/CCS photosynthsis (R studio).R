##                        ##
## ICOS - CO2 dye sensor  ##
##  Test Data Analysis    ##
##                        ##

## This file contains the data of photosynthsis measerments in a closed chamber system, 

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
  geom_line() +
    scale_y_continuous(name = "Reflectance (590 nm)", limits = c(1025, 1220)) +
    scale_x_continuous(name = "Time (min)") 





