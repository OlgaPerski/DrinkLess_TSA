# load packages and data --------------------------------------------------

source(here::here("scripts", "0_load_packages_and_data.R"))

# prepare data for analysis -----------------------------------------------

# RQ1 ---------------------------------------------------------------------

# clean data and create day of the week, month of the year, trend, level and slope vars 

df_rq1a_cleaning <- df_rq1a %>%
  mutate(date_of_screen_view = parse_date(as.character(date_of_screen_view), format = ""),
         day = wday(date_of_screen_view, week_start = 1),
         month = month(date_of_screen_view),
         mean_time_on_app = as.numeric(parse_time(str_remove(mean_time_on_app, pattern = "0 days "), format = "%H:%M:%S")/60),
         trend = 1:n(),
         level = case_when(trend < 367 ~ 0,
                                  TRUE ~ 1))
         
slope_0 <- df_rq1a_cleaning %>%
           filter(level == 0) %>%
           mutate(slope = rep(0, times = 366))
         
slope_1 <- df_rq1a_cleaning %>%
           filter(level == 1) %>%
           mutate(slope = 1:n())
         
df_rq1a_clean <- rbind(slope_0, slope_1)

exclude <- c("sum_percentage_unique_screens_across_users", "total_time_on_app", "total_number_sessions", 
             "total_IDs_per_day", "total_alcohol_free_days", "drinkrecord_units", "heavy_drinking_day",
             "week_count", "number_active_users")

df_rq1a_clean <- df_rq1a_clean %>%
  select(-all_of(exclude))

# check distribution of outcome vars and test discrete vars for overdispersion

# mean proportion screens viewed (continuous)

ggplot(df_rq1a_clean, aes(mean_percent_screens_viewed)) + 
  geom_histogram(colour = "black", fill = "white") +
  scale_x_continuous() +
  geom_vline(aes(xintercept = median(mean_percent_screens_viewed)), col='blue', linetype="dashed", size=1) +
  xlab("Mean percent screen views") + 
  ylab("Frequency") +
  theme_minimal()

mean(df_rq1a_clean$mean_percent_screens_viewed)
var(df_rq1a_clean$mean_percent_screens_viewed)

# mean time on app (continuous)

ggplot(df_rq1a_clean, aes(mean_time_on_app)) + 
  geom_histogram(colour = "black", fill = "white") +
  scale_x_continuous() +
  geom_vline(aes(xintercept = median(mean_time_on_app)), col='blue', linetype="dashed", size=1) +
  xlab("Mean time on app") + 
  ylab("Frequency") +
  theme_minimal()

mean(df_rq1a_clean$mean_time_on_app)
var(df_rq1a_clean$mean_time_on_app)

# mean nr of sessions (continuous)

ggplot(df_rq1a_clean, aes(mean_number_sessions)) + 
  geom_histogram(colour = "black", fill = "white") +
  scale_x_continuous() +
  geom_vline(aes(xintercept = median(mean_number_sessions)), col='blue', linetype="dashed", size=1) +
  xlab("Mean number of sessions") + 
  ylab("Frequency") +
  theme_minimal()

mean(df_rq1a_clean$mean_number_sessions)
var(df_rq1a_clean$mean_number_sessions)

# mean units per day (continuous)

ggplot(df_rq1a_clean, aes(mean_units_per_day)) + 
  geom_histogram(colour = "black", fill = "white") +
  scale_x_continuous() +
  geom_vline(aes(xintercept = median(mean_units_per_day)), col='blue', linetype="dashed", size=1) +
  xlab("Mean alcohol units per day") + 
  ylab("Frequency") +
  theme_minimal()

mean(df_rq1a_clean$mean_units_per_day)
var(df_rq1a_clean$mean_units_per_day)

# mean heavy drinking days (continuous)

ggplot(df_rq1a_clean, aes(mean_heavy_drinking_days)) + 
  geom_histogram(colour = "black", fill = "white") +
  scale_x_continuous() +
  geom_vline(aes(xintercept = median(mean_heavy_drinking_days)), col='blue', linetype="dashed", size=1) +
  xlab("Mean heavy drinking days") + 
  ylab("Frequency") +
  theme_minimal()

mean(df_rq1a_clean$mean_heavy_drinking_days)
var(df_rq1a_clean$mean_heavy_drinking_days)

# mean alcohol free days (continuous)

ggplot(df_rq1a_clean, aes(mean_alcohol_free_days)) + 
  geom_histogram(colour = "black", fill = "white") +
  scale_x_continuous() +
  geom_vline(aes(xintercept = median(mean_alcohol_free_days)), col='blue', linetype="dashed", size=1) +
  xlab("Mean alcohol free days") + 
  ylab("Frequency") +
  theme_minimal()

mean(df_rq1a_clean$mean_alcohol_free_days)
var(df_rq1a_clean$mean_alcohol_free_days)

# save clean data as rds file

write_rds(df_rq1a_clean, here("data", "df_rq1_clean.rds"))

# RQ2 ---------------------------------------------------------------------

# tally downloads per day, add missing dates and create day of the week, month of the year, trend, level and slope vars

df_rq2_cleaning <- df_rq2 %>%
  mutate(app_download_date = as.Date(app_download_date)) %>%
  add_row(app_download_date = as.Date("2020-03-24"), number_downloads = 0) %>%
  add_row(app_download_date = as.Date("2020-03-16"), number_downloads = 0) %>%
  add_row(app_download_date = as.Date("2020-03-18"), number_downloads = 0) %>%
  arrange(app_download_date) %>%
  mutate(day = wday(app_download_date, week_start = 1),
         month = month(app_download_date),
         trend = 1:n(),
         level = case_when(trend < 367 ~ 0,
                           TRUE ~ 1))

slope_0 <- df_rq2_cleaning %>%
  filter(level == 0) %>%
  mutate(slope = rep(0, times = 366))

slope_1 <- df_rq2_cleaning %>%
  filter(level == 1) %>%
  mutate(slope = 1:n())

df_rq2_clean <- rbind(slope_0, slope_1)

# check distribution of outcome var and test for overdispersion

ggplot(df_rq2_clean, aes(number_downloads)) + 
  geom_histogram(colour = "black", fill = "white") +
  scale_x_continuous(breaks = round(seq(min(df_rq2_clean$number_downloads), max(df_rq2_clean$number_downloads), by = 10))) +
  geom_vline(aes(xintercept = median(number_downloads)), col='blue', linetype="dashed", size=1) +
  xlab("Number of downloads") + 
  ylab("Frequency") +
  theme_minimal()

a.overdisp <- glm(number_downloads ~ ., data = df_rq2_clean, family = poisson)
dispersiontest(a.overdisp, trafo = 1) # trending towards overdispersion
mean(df_rq2_clean$number_downloads)
var(df_rq2_clean$number_downloads)

# save clean data as rds file

write_rds(df_rq2_clean, here("data", "df_rq2_clean.rds"))

# RQ3a --------------------------------------------------------------------

# add missing dates, replace missing vars with mean values, create day of the week, month of the year, trend, level and slope vars

mean(df_rq3a$AUDIT_score)
mean(df_rq3a$gender)
mean(df_rq3a$employment_type)
mean(df_rq3a$age)
mean(df_rq3a$at_risk)

df_rq3a_cleaning <- df_rq3a %>%
  mutate(download_date = as.Date(download_date)) %>%
  add_row(download_date = as.Date("2020-03-16"), AUDIT_score = 16.46999,
          gender = 0.5414287, employment_type = 0.7062194, age = 44.28075, at_risk = 0.9093583) %>%
  add_row(download_date = as.Date("2020-03-18"), AUDIT_score = 16.46999,
          gender = 0.5414287, employment_type = 0.7062194, age = 44.28075, at_risk = 0.9093583) %>%
  add_row(download_date = as.Date("2020-03-24"), AUDIT_score = 16.46999,
          gender = 0.5414287, employment_type = 0.7062194, age = 44.28075, at_risk = 0.9093583) %>%
  arrange(download_date) %>%
  mutate(day = wday(download_date, week_start = 1),
         month = month(download_date),
         trend = 1:n(),
         level = case_when(trend < 367 ~ 0,
                           TRUE ~ 1))
  
slope_0 <- df_rq3a_cleaning %>%
  filter(level == 0) %>%
  mutate(slope = rep(0, times = 366))

slope_1 <- df_rq3a_cleaning %>%
  filter(level == 1) %>%
  mutate(slope = 1:n())

df_rq3a_clean <- rbind(slope_0, slope_1)

# check distribution of outcome vars and test for overdispersion

# gender

ggplot(df_rq3a_clean, aes(gender)) + 
  geom_histogram(colour = "black", fill = "white") +
  scale_x_continuous() +
  geom_vline(aes(xintercept = median(gender)), col='blue', linetype="dashed", size=1) +
  xlab("Mean gender") + 
  ylab("Frequency") +
  theme_minimal()

a.overdisp <- glm(gender ~ ., data = df_rq3a_clean, family = poisson)
dispersiontest(a.overdisp, trafo = 1)
mean(df_rq3a_clean$gender)
var(df_rq3a_clean$gender)

# age

ggplot(df_rq3a_clean, aes(age)) + 
  geom_histogram(colour = "black", fill = "white") +
  scale_x_continuous() +
  geom_vline(aes(xintercept = median(age)), col='blue', linetype="dashed", size=1) +
  xlab("Mean age") + 
  ylab("Frequency") +
  theme_minimal()

a.overdisp <- glm(age ~ ., data = df_rq3a_clean, family = poisson)
dispersiontest(a.overdisp, trafo = 1)
mean(df_rq3a_clean$age)
var(df_rq3a_clean$age)

# employment type

ggplot(df_rq3a_clean, aes(employment_type)) + 
  geom_histogram(colour = "black", fill = "white") +
  scale_x_continuous() +
  geom_vline(aes(xintercept = median(employment_type)), col='blue', linetype="dashed", size=1) +
  xlab("Mean emplyment type") + 
  ylab("Frequency") +
  theme_minimal()

a.overdisp <- glm(employment_type ~ ., data = df_rq3a_clean, family = poisson)
dispersiontest(a.overdisp, trafo = 1)
mean(df_rq3a_clean$employment_type)
var(df_rq3a_clean$employment_type)

# AUDIT score

ggplot(df_rq3a_clean, aes(AUDIT_score)) + 
  geom_histogram(colour = "black", fill = "white") +
  scale_x_continuous() +
  geom_vline(aes(xintercept = median(AUDIT_score)), col='blue', linetype="dashed", size=1) +
  xlab("Mean AUDIT score") + 
  ylab("Frequency") +
  theme_minimal()

a.overdisp <- glm(AUDIT_score ~ ., data = df_rq3a_clean, family = poisson)
dispersiontest(a.overdisp, trafo = 1)
mean(df_rq3a_clean$AUDIT_score)
var(df_rq3a_clean$AUDIT_score)

# at risk of dependence

ggplot(df_rq3a_clean, aes(at_risk)) + 
  geom_histogram(colour = "black", fill = "white") +
  scale_x_continuous() +
  geom_vline(aes(xintercept = median(at_risk)), col='blue', linetype="dashed", size=1) +
  xlab("Mean at risk") + 
  ylab("Frequency") +
  theme_minimal()

a.overdisp <- glm(at_risk ~ ., data = df_rq3a_clean, family = poisson)
dispersiontest(a.overdisp, trafo = 1)
mean(df_rq3a_clean$at_risk)
var(df_rq3a_clean$at_risk)

# save clean data as rds file

write_rds(df_rq3a_clean, here("data", "df_rq3a_clean.rds"))

# RQ3b-e ------------------------------------------------------------------

# add missing dates, replace missing vars with mean values, create day of the week, month of the year, trend, level and slope vars

exclude <- c("AUDIT_score", "gender", "employment_type", "age", "number_downloads")

df_rq3be_cleaning <- df_rq3be %>%
  select(-all_of(exclude))

mean(df_rq3be_cleaning$total_number_sessions, na.rm = T)
mean(df_rq3be_cleaning$no_days_used, na.rm = T)
mean(df_rq3be_cleaning$proportion_screens_viewed, na.rm = T)
mean(df_rq3be_cleaning$drinkrecord_units, na.rm = T)
mean(df_rq3be_cleaning$heavy_drinking_day, na.rm = T)
mean(df_rq3be_cleaning$alc_free_days, na.rm = T)
mean(df_rq3be_cleaning$time_minutes, na.rm = T)

# replace missing and alcohol units outlier with mean value

a <- df_rq3be_cleaning %>%
  filter(drinkrecord_units < 660742.01608) %>%
  summarise(mean = mean(drinkrecord_units))

df_rq3be_cleaning$drinkrecord_units <- replace(df_rq3be_cleaning$drinkrecord_units, 397, 72.20908)
df_rq3be_cleaning$drinkrecord_units <- replace(df_rq3be_cleaning$drinkrecord_units, 360, 72.20908)
df_rq3be_cleaning$heavy_drinking_day <- replace(df_rq3be_cleaning$heavy_drinking_day, 360, 4.443185)
df_rq3be_cleaning$alc_free_days <- replace(df_rq3be_cleaning$alc_free_days, 360, 10.69769)
df_rq3be_cleaning$alc_free_days <- replace(df_rq3be_cleaning$alc_free_days, 412, 10.69769)

df_rq3be_cleaning_2 <- df_rq3be_cleaning %>%
  mutate(app_download_date = as.Date(app_download_date)) %>%
  add_row(app_download_date = as.Date("2020-03-16"), total_number_sessions = 15.17955,
          no_days_used = 9.657226, proportion_screens_viewed = 0.3055666, drinkrecord_units = 72.20908, 
          heavy_drinking_day = 4.443185, alc_free_days = 10.69769, time_minutes = 39.08025) %>%
  add_row(app_download_date = as.Date("2020-03-18"), total_number_sessions = 15.17955,
          no_days_used = 9.657226, proportion_screens_viewed = 0.3055666, drinkrecord_units = 72.20908, 
          heavy_drinking_day = 4.443185, alc_free_days = 10.69769, time_minutes = 39.08025) %>%
  add_row(app_download_date = as.Date("2020-03-23"), total_number_sessions = 15.17955,
          no_days_used = 9.657226, proportion_screens_viewed = 0.3055666, drinkrecord_units = 72.20908, 
          heavy_drinking_day = 4.443185, alc_free_days = 10.69769, time_minutes = 39.08025) %>%
  add_row(app_download_date = as.Date("2020-03-24"), total_number_sessions = 15.17955,
          no_days_used = 9.657226, proportion_screens_viewed = 0.3055666, drinkrecord_units = 72.20908, 
          heavy_drinking_day = 4.443185, alc_free_days = 10.69769, time_minutes = 39.08025) %>%
  arrange(app_download_date) %>%
  mutate(day = wday(app_download_date, week_start = 1),
         month = month(app_download_date),
         trend = 1:n(),
         level = case_when(trend < 367 ~ 0,
                           TRUE ~ 1))

slope_0 <- df_rq3be_cleaning_2 %>%
  filter(level == 0) %>%
  mutate(slope = rep(0, times = 366))

slope_1 <- df_rq3be_cleaning_2 %>%
  filter(level == 1) %>%
  mutate(slope = 1:n())

df_rq3be_clean <- rbind(slope_0, slope_1)

# check distribution of outcome vars and test for overdispersion

# total nr of sessions

ggplot(df_rq3be_clean, aes(total_number_sessions)) + 
  geom_histogram(colour = "black", fill = "white") +
  scale_x_continuous() +
  geom_vline(aes(xintercept = median(total_number_sessions)), col='blue', linetype="dashed", size=1) +
  xlab("Nr of sessions") + 
  ylab("Frequency") +
  theme_minimal()

a.overdisp <- glm(total_number_sessions ~ ., data = df_rq3be_clean, family = poisson)
dispersiontest(a.overdisp, trafo = 1)
mean(df_rq3be_clean$total_number_sessions)
var(df_rq3be_clean$total_number_sessions)

# nr of days used

ggplot(df_rq3be_clean, aes(no_days_used)) + 
  geom_histogram(colour = "black", fill = "white") +
  scale_x_continuous() +
  geom_vline(aes(xintercept = median(no_days_used)), col='blue', linetype="dashed", size=1) +
  xlab("Nr days used") + 
  ylab("Frequency") +
  theme_minimal()

a.overdisp <- glm(no_days_used ~ ., data = df_rq3be_clean, family = poisson)
dispersiontest(a.overdisp, trafo = 1)
mean(df_rq3be_clean$no_days_used)
var(df_rq3be_clean$no_days_used)

# proportion screens viewed

ggplot(df_rq3be_clean, aes(proportion_screens_viewed)) + 
  geom_histogram(colour = "black", fill = "white") +
  scale_x_continuous() +
  geom_vline(aes(xintercept = median(proportion_screens_viewed)), col='blue', linetype="dashed", size=1) +
  xlab("Proportion screens viewed") + 
  ylab("Frequency") +
  theme_minimal()

a.overdisp <- glm(proportion_screens_viewed ~ ., data = df_rq3be_clean, family = poisson)
dispersiontest(a.overdisp, trafo = 1)
mean(df_rq3be_clean$proportion_screens_viewed)
var(df_rq3be_clean$proportion_screens_viewed)

# time spent (mins)

ggplot(df_rq3be_clean, aes(time_minutes)) + 
  geom_histogram(colour = "black", fill = "white") +
  scale_x_continuous() +
  geom_vline(aes(xintercept = median(time_minutes)), col='blue', linetype="dashed", size=1) +
  xlab("Time spent (mins)") + 
  ylab("Frequency") +
  theme_minimal()

a.overdisp <- glm(time_minutes ~ ., data = df_rq3be_clean, family = poisson)
dispersiontest(a.overdisp, trafo = 1) # evidence of overdispersion
mean(df_rq3be_clean$time_minutes)
var(df_rq3be_clean$time_minutes)

# nr alcohol units

ggplot(df_rq3be_clean, aes(drinkrecord_units)) + 
  geom_histogram(colour = "black", fill = "white") +
  scale_x_continuous() +
  geom_vline(aes(xintercept = median(drinkrecord_units)), col='blue', linetype="dashed", size=1) +
  xlab("Nr alcohol units") + 
  ylab("Frequency") +
  theme_minimal()

a.overdisp <- glm(drinkrecord_units ~ ., data = df_rq3be_clean, family = poisson)
dispersiontest(a.overdisp, trafo = 1) # evidence of overdispersion
mean(df_rq3be_clean$drinkrecord_units)
var(df_rq3be_clean$drinkrecord_units)

# nr heavy drinking days

ggplot(df_rq3be_clean, aes(heavy_drinking_day)) + 
  geom_histogram(colour = "black", fill = "white") +
  scale_x_continuous() +
  geom_vline(aes(xintercept = median(heavy_drinking_day)), col='blue', linetype="dashed", size=1) +
  xlab("Nr heavy drinking days") + 
  ylab("Frequency") +
  theme_minimal()

a.overdisp <- glm(heavy_drinking_day ~ ., data = df_rq3be_clean, family = poisson)
dispersiontest(a.overdisp, trafo = 1)
mean(df_rq3be_clean$heavy_drinking_day)
var(df_rq3be_clean$heavy_drinking_day)

# nr alcohol free days

ggplot(df_rq3be_clean, aes(alc_free_days)) + 
  geom_histogram(colour = "black", fill = "white") +
  scale_x_continuous() +
  geom_vline(aes(xintercept = median(alc_free_days)), col='blue', linetype="dashed", size=1) +
  xlab("Nr alcohol free days") + 
  ylab("Frequency") +
  theme_minimal()

a.overdisp <- glm(alc_free_days ~ ., data = df_rq3be_clean, family = poisson)
dispersiontest(a.overdisp, trafo = 1) # evidence of overdispersion
mean(df_rq3be_clean$alc_free_days)
var(df_rq3be_clean$alc_free_days)

# save clean data as rds file

write_rds(df_rq3be_clean, here("data", "df_rq3be_clean.rds"))
  