##### load packages #####
library(readxl)
library(tidyverse)
library(wesanderson)

##### import data #####
Log_Sheet <- read_excel("2022 Echo Lab Patient Log Sheet.xlsx")

##### change structure of variables for easy use #####
Log_Sheet$`PT WEIGHT (kg)` <- suppressWarnings(as.numeric(Log_Sheet$`PT WEIGHT (kg)`))
Log_Sheet$DATE <- as.Date(Log_Sheet$DATE, format = "%y-%m-%d")
fcols <- c(7,9,10,15:18) # cols that should be factors
Log_Sheet[fcols] <- lapply(Log_Sheet[fcols], factor)
Log_Sheet <- Log_Sheet %>%
  mutate(BMI = round(`PT WEIGHT (kg)`/(`PT HEIGHT (m)`)^2, digits = 2)) %>%
  separate(col = DATE, into = c(NA, "MONTH", NA), sep = "-", remove = F) %>%
  select(-`PT NAME`) %>%
  mutate(MONTH = as.numeric(MONTH))

# start and end dates
DATE <- seq.Date(first(Log_Sheet$DATE), last(Log_Sheet$DATE), by = "day")
jan1 <- first(DATE)
today <- last(DATE)
yesterday <- nth(DATE, n = (length(DATE) - 1))
past30 <-  nth(DATE, n = (length(DATE) - 30))

##### correct misspelling and mislabels #####
# OP/IP
Log_Sheet$`OP/IP` <- suppressWarnings(fct_recode(Log_Sheet$`OP/IP`, 
                                                 "OP" = "0", 
                                                 "OP" = "O", 
                                                 "IP" = "IO"))

# EXAM
Log_Sheet$EXAM <- suppressWarnings(fct_recode(Log_Sheet$EXAM, 
                                              "ATTEMPT TEE" = "TEE ATTEMPT", 
                                              "ATTEMPT TEE" = "Attempted TEE", 
                                              "PERICARDIOCENTESIS" = "Periocardiocentesis"))

# SUPPORT STAFF
Log_Sheet$`SUPPORT STAFF` <- suppressWarnings(fct_recode(Log_Sheet$`SUPPORT STAFF`,
                                                         "NURSE" = "RN",
                                                         "RAPID RESPONSE" = "RR NURSE",
                                                         "DI NURSE & TECH" = "DI NURSE"))

# AGITATED SALINE & DEFINITY
Log_Sheet$`AGITATED SALINE & DEFINITY` <- suppressWarnings(fct_recode(Log_Sheet$`AGITATED SALINE & DEFINITY`,
                                                                      "AGITATED SALINE & DEFINITY" = "AGITATED SALINE & SALINE",
                                                                      "DEFINITY" = "CCU NURSE"))

# READER OF THE DAY 
`%notin%` <- Negate(`%in%`)

readers <- c("CA/LI", "CA", "CA/MARTINEZ", "MARTINEZ", "GIRN", "YOUSAF", "CA/ARORA", "ARORA",      
             "LARSON", "CA/WERDEN","WERDEN", "CA/LARSON", "WARK", "LI", "ROSSOW", "CA/KENNEDY", 
             "CA/YOUSAF", "CA/GIRN", "CA/WARK", "BERGER", "WAGGONER", "CA/WAGGONER", "KENNEDY", "GHIMIRE",    
             "CA/ROSSOW", "WOLFE", "SWAN")

# use above function and list of readers to replace all values that do not match approved readers with "CA"
Log_Sheet$`READING DR/READER OF THE DAY` <- replace(
  Log_Sheet$`READING DR/READER OF THE DAY`,
  which(Log_Sheet$`READING DR/READER OF THE DAY` %notin% readers 
        & !is.na(Log_Sheet$`READING DR/READER OF THE DAY`)), 
  values = "CA")

# TECH
# split into two columns
Log_Sheet <- Log_Sheet %>%
  separate(col = TECH, into = c("TECH1", "TECH2"), sep = "/", fill = "right")

##### Filter important information and create appropriate tables #####

# create a function to simplify table creation
typecount <- function(x) {
  temptable <- as.data.frame(table(Log_Sheet$DATE, x))
  pivot_wider(temptable,
              names_from = 'x',
              values_from = 'Freq')
}

exam_count <- typecount(Log_Sheet$EXAM) # starting table also Exam Count
exam_count <- exam_count %>% 
  rename("TEE.Att" = "ATTEMPT TEE",
         "PCC" = "PERICARDIOCENTESIS",
         "Canceled" = "CXLD",
         "Limited" = "LTD",
         "No Show" = "N/S",
         "Rescheduled" = "R/S")
IpOp_count <- typecount(Log_Sheet$`OP/IP`) # number of IP and OP per day
tech1 <- typecount(Log_Sheet$TECH1) # tech 1 col
tech2 <- typecount(Log_Sheet$TECH2) # tech 2 col (both for later use)

# completed exams per day
comp <- c("TTE", "LTD", "TEE", "PERICARDIOCENTESIS", "ATTEMPT TEE", "DSE")
completed_day_count <- Log_Sheet %>%
  filter(EXAM %in% comp) %>%
  group_by(DATE) %>%
  summarize(Completed = n()) %>%
  complete(DATE=seq.Date(jan1, today, by = "day")) %>%
  replace_na(list(Completed = 0))

#### COVID and BAV Exams ####

# search FINDINGS for covid cases
covid <- str_detect(Log_Sheet$`FINDINGS (REMEMBER TO LABEL COVID PATIENTS)`, 
                    regex('COVID', ignore_case = T))
# summarize into a table
covid_count <- Log_Sheet %>%
  filter(covid == "TRUE") %>%
  group_by(DATE) %>%
  summarize(Covid = n()) %>%
  complete(DATE=seq.Date(jan1, today, by = "day")) %>%
  replace_na(list(Covid = 0))

# search INDICATION for BAVs
bav <- str_detect(Log_Sheet$INDICATION, "BAV")
# summarize into a table
bav_count <- Log_Sheet %>%
  filter(bav == "TRUE") %>%
  group_by(DATE) %>%
  summarize(BAV = n()) %>%
  complete(DATE=seq.Date(jan1, today, by = "day")) %>%
  replace_na(list(BAV = 0))

#### CCU, ED, STAT exams ####

# create function similar to above to count the number of exams in given column and summarize by date 
coolcount <- function(x) {
  Log_Sheet %>%
    filter(!is.na(x)) %>%
    group_by(DATE) %>%
    summarize(N = n()) %>%
    complete(DATE = seq.Date(jan1, today, by = "day")) %>%
    replace_na(list(N = 0))
}

ccu_count <- coolcount(Log_Sheet$CCU) # CCU exams
ed_count <- coolcount(Log_Sheet$ED) # ED exams 
stat_count <- coolcount(Log_Sheet$STAT) # STAT exams 

#### Definity per Day ####
# create list for definity exams
def <- c("DEFINITY", "AGITATED SALINE & DEFINITY")
definity_count <- Log_Sheet %>%
  filter(`AGITATED SALINE & DEFINITY` %in% def) %>%
  group_by(DATE) %>%
  summarize(Definity.N = n()) %>%
  complete(DATE = seq.Date(jan1, today, by = "day")) %>%
  replace_na(list(Definity.N = 0)) %>%
  mutate(Definity.P = round((Definity.N/completed_day_count$Completed)*100, 2)) 

#### Watchman information ####
# list of watchman terms to search for
prew <- c("PRE-WATCHMAN", "PREWATCHMAN")
postw <- c("S/P WATCHMAN", "POST WATCHMAN", "POST-WATCHMAN")
placew <- c("LAA EXCLUSION", "WATCHMAN PLACEMENT")

# create function to search INDICATION for the specified family of strings and create a table summarizing by date
watchcount <- function(x, kind) {
  Log_Sheet %>%
    filter(str_detect(Log_Sheet$INDICATION,
                      paste(kind, collapse = "|"))) %>%
    group_by(DATE) %>%
    summarize(N = n()) %>%
    complete(DATE = seq.Date(jan1, today, by = "day")) %>%
    replace_na(list(N = 0))
}

prewatchman_count <- watchcount(Log_Sheet$INDICATION, kind = prew) # Pre-placement exams
postwatchman_count <- watchcount(Log_Sheet$INDICATION, kind = postw) # Post-placement exams
watchplace_count <- watchcount(Log_Sheet$INDICATION, kind = placew) # Placement procedures

#### Tech table ####
# find unique tech names
tech_names <- str_sort(unique(c(names(tech1), names(tech2)))[-1])
# create empty matrix
zeroes <- rep(0, times = length(DATE)) # vector of zeroes
tech_count <- as.data.frame(replicate(length(tech_names), zeroes)) # duplicate the column of 0s
colnames(tech_count) <- tech_names # add column names

# add the values from tech 1
tech_count[which(tech_names %in% names(tech1))] <- tech_count[which(tech_names %in% names(tech1))] + tech1[-1]
# add the values from tech 2
tech_count[which(tech_names %in% names(tech2))] <- tech_count[which(tech_names %in% names(tech2))] + tech2[-1]
# add date column
tech_count <- tech_count %>% 
  add_column(DATE = seq.Date(jan1, today, by = "day"), 
             .before = first(tech_names))

#### CREATE FINAL DF ####
sum_table <- data.frame(
  completed_day_count,
  exam_count[,c(2:5,7:10)],
  IpOp_count[,2:3],
  covid_count[,2],
  definity_count[,3],
  "STAT" = stat_count$N,
  "CCU" = ccu_count$N,
  "ED" = ed_count$N,
  bav_count[,2],
  "WM.Pre" = prewatchman_count$N,
  "WM.Place" = watchplace_count$N,
  "WM.Post" = postwatchman_count$N,
  tech_count[,2:14]
) %>%
  relocate(Rescheduled, .after = Completed) %>%
  relocate(OP, .after = Rescheduled) %>%
  relocate(IP, .after = OP) %>% 
  relocate(DSE, .after = BAV) %>%
  relocate(PCC, .after = DSE) %>%
  relocate(TEE.Att, .after = TEE)

# define custom ggplot theme for coming plots

theme_np <- function() {
  theme(
    line = element_line(color = "black"),
    rect = element_rect(fill = "#FFFFFF",
                        linetype = 0, colour = NA),
    text = element_text(color = "#333333"),
    axis.title = element_blank(),
    axis.text = element_text(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(color = "#DADADA"),
    axis.line = element_blank(),
    axis.text.x=element_text(size=11),
    axis.text.y=element_text(size=10),
    legend.background = element_rect(),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.box = "vertical",
    panel.background = element_blank(),
    panel.grid = element_line(colour = NULL),
    panel.grid.major = element_line(color = "#DADADA"),
    panel.grid.minor = element_line(color = "#DADADA"),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0, size = rel(1.4), face = "bold"),
    plot.margin = unit(c(1, 1, 1, 1), "lines"),
    strip.background = element_rect())
}
