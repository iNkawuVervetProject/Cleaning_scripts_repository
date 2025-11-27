# -----------------------------------------------------------
# Script by: Josefien Tankink
# Contact: j.a.tankink@gmail.comm
# Goal: clean focal file and add observation number and 
# duration of behaviours
# -----------------------------------------------------------

# ---------- Library -------
{
  library(lubridate)
library(dplyr)
library(tidyr)
}

# ---------- Data --------
{
  setwd("yourworkingdirectory")

focal <- read.csv("focal.CSV",1) %>%  # June 2022 to October 2022
  dplyr::select(date, time, group, timezone, observer1, idindividual1, behaviour, behaviourtype,
                behaviourfocal, idindividual2, interaction, complete, agonisticinteraction, idsupporters,
                supportersbehaviour, victimsupresponse, idredirection, redirectionbehaviour, victimredresponse,
                idinterruptor, behaviourinterruptor, responsefocal, behaviourfocal2, idindividual3, interaction2,
                behaviourfocal3, idindividual4, interaction3, vposture, vheight, vposition, remarks, datainfo, deviceid) %>%
  mutate(date = as.Date(date)) %>% 
  filter(date > "2020-01-01")
}


# ---- Add observation nb and duration ----
{
  ffocals <- focal %>%
  arrange(date, idindividual1, time) %>% 
  mutate(
    DateTime = ymd_hms(paste(date, time)),  # Combine with an arbitrary date
    time_diff = as.numeric(difftime(DateTime, lag(DateTime, default = first(DateTime)), units = "secs")),
    change_group = idindividual1 != lag(idindividual1, default = first(idindividual1)) | time_diff > 1200,
    Obs.nr = cumsum(change_group) + 1,
    Duration = case_when(
      row_number() == 1 ~ 0,
      behaviour == "START" ~ 0,
      T ~ time_diff
    ),
    behaviourfocal = sub(" ", "", behaviourfocal)
  ) %>%
  filter(idindividual1 != "") %>% 
  distinct()
  }

# ---- Check data distribution ----
{
  hist(ffocals$date, breaks = "months")
unique(ffocals$group)
hist(ffocals %>% 
       filter(group == "Ankhase") %>% 
       pull(date), breaks = "months")
hist(ffocals %>% 
       filter(group == "Baie Dankie") %>% 
       pull(date), breaks = "months")
hist(ffocals %>% 
       filter(group == "Crossing") %>% 
       pull(date), breaks = "months")
hist(ffocals %>% 
       filter(group == "IFamily") %>% 
       pull(date), breaks = "months")
hist(ffocals %>% 
       filter(group == "Kubu") %>% 
       pull(date), breaks = "months")
hist(ffocals %>% 
       filter(group == "Lemon Tree") %>% 
       pull(date), breaks = "months")
hist(ffocals %>% 
       filter(group == "Noha") %>% 
       pull(date), breaks = "months")
hist(ffocals %>% 
       filter(group == "RnB") %>% 
       pull(date), breaks = "months")
}

# ---- Save output ----
write.csv(ffocals, "Output/Cleaned_focal.csv", row.names = F)


