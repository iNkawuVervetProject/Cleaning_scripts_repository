# -----------------------------------------------------------
# Script by: Josefien Tankink
# Contact: j.a.tankink@gmail.comm
# Goal: match bge summary to bge interactions with missing
# encounter group entries
# -----------------------------------------------------------

# -------- Library ---------
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr)
  library(readr); library(lubridate); library(ggplot2);
  library(lme4); library(car); library(emmeans); library(effects);
  library(glmmTMB); library(MASS); library(DHARMa)
})

# ---------- Data ----------
{
  setwd("yourworkingdirectory") 

int <- read.csv("bge_interactions.csv")
sum <- read.csv("bge_summary.csv")

table(int$encountergp)
}

# ---- Match nearest summary to interactions if missing encounter gp -----
{# ---- Standardize/parse datetimes ----
{
int_clean <- int %>%
  mutate(
    rowid = row_number(),
    # treat empty string as NA for encountergp
    encountergp = na_if(encountergp, ""),
    # fix times like "15.54" -> "15:54"
    time_std = str_replace(time, "^(\\d{1,2})\\.(\\d{2})$", "\\1:\\2"),
    dt = parse_date_time(paste(date, time_std),
                         orders = c("Y-m-d H:M:S", "Y-m-d H:M"),
                         tz = "UTC")
  )

sum_clean <- `sum` %>%
  mutate(
    time_std = str_replace(time, "^(\\d{1,2})\\.(\\d{2})$", "\\1:\\2"),
    dt = parse_date_time(paste(date, time_std),
                         orders = c("Y-m-d H:M:S", "Y-m-d H:M"),
                         tz = "UTC")
  ) %>%
  dplyr::select(date, group, encountergp_sum = encountergp, dt_sum = dt)
}

# ---- Only rows in int with missing encountergp ----
{
  int_missing <- int_clean %>%
    filter(is.na(encountergp))
  }

# ---- Join by date + group, then keep pairs within 120 minutes ----
# I chose 120 minutes, but if you want to be more conservative you can bring this number down
{
  candidates <- int_missing %>%
  left_join(sum_clean, by = c("date", "group")) %>%
  mutate(diff_mins = abs(as.numeric(difftime(dt_sum, dt, units = "mins")))) %>%
  filter(!is.na(diff_mins), diff_mins <= 120)
  }

# ---- If multiple matches, take the closest in time ----
{
  best_matches <- candidates %>%
    group_by(rowid) %>%
    slice_min(diff_mins, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    dplyr::select(rowid, encountergp_match = encountergp_sum)
  }

# ---- Fill encountergp in the original data where it was missing ----
{
  int_updated <- int_clean %>%
    left_join(best_matches, by = "rowid") %>%
    mutate(
      encountergp = if_else(
        is.na(encountergp),
        coalesce(encountergp_match, encountergp),
        encountergp
      )
    ) %>%
    dplyr::select(-rowid, -time_std, -dt, -encountergp_match)
 
# Result: int_updated has encountergp filled where matches were found

table(int_updated$encountergp)
table(int$encountergp)
}
  
  # Only NA's left are for interactions where there is no summary
  }

# ----- Save your data ------
write.csv(int_updated, "bge_interactions_updated.csv", row.names = F)
