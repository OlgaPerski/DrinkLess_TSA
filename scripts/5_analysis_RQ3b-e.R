# load packages and data --------------------------------------------------

source(here::here("scripts", "0_load_packages_and_data.R"))

df_rq3be_clean <- read_rds(here::here("data", "df_rq3be_clean.rds"))

# RQ3b-e - effect of lockdown on app engagement and drink records -------------------------------

# descriptives overall, pre- and post-lockdown

# overall

length(df_rq3be_clean$total_number_sessions)
range(df_rq3be$number_downloads)

round(mean(df_rq3be_clean$total_number_sessions), 3)
round(sd(df_rq3be_clean$total_number_sessions), 3)

round(mean(df_rq3be_clean$no_days_used), 3)
round(sd(df_rq3be_clean$no_days_used), 3)

round(mean(df_rq3be_clean$proportion_screens_viewed), 3)
round(sd(df_rq3be_clean$proportion_screens_viewed), 3)

round(mean(df_rq3be_clean$time_minutes), 3)
round(sd(df_rq3be_clean$time_minutes), 3)

round(mean(df_rq3be_clean$drinkrecord_units), 3)
round(sd(df_rq3be_clean$drinkrecord_units), 3)

round(mean(df_rq3be_clean$heavy_drinking_day), 3)
round(sd(df_rq3be_clean$heavy_drinking_day), 3)

round(mean(df_rq3be_clean$alc_free_days), 3)
round(sd(df_rq3be_clean$alc_free_days), 3)

# pre-lockdown

pre_be <- df_rq3be_clean %>%
  filter(level == 0)

round(mean(pre_be$total_number_sessions), 3)
round(sd(pre_be$total_number_sessions), 3)

round(mean(pre_be$no_days_used), 3)
round(sd(pre_be$no_days_used), 3)

round(mean(pre_be$proportion_screens_viewed), 3)
round(sd(pre_be$proportion_screens_viewed), 3)

round(mean(pre_be$time_minutes), 3)
round(sd(pre_be$time_minutes), 3)

round(mean(pre_be$drinkrecord_units), 3)
round(sd(pre_be$drinkrecord_units), 3)

round(mean(pre_be$heavy_drinking_day), 3)
round(sd(pre_be$heavy_drinking_day), 3)

round(mean(pre_be$alc_free_days), 3)
round(sd(pre_be$alc_free_days), 3)

# post-lockdown

post_be <- df_rq3be_clean %>%
  filter(level == 1)

round(mean(post_be$total_number_sessions), 3)
round(sd(post_be$total_number_sessions), 3)

round(mean(post_be$no_days_used), 3)
round(sd(post_be$no_days_used), 3)

round(mean(post_be$proportion_screens_viewed), 3)
round(sd(post_be$proportion_screens_viewed), 3)

round(mean(post_be$time_minutes), 3)
round(sd(post_be$time_minutes), 3)

round(mean(post_be$drinkrecord_units), 3)
round(sd(post_be$drinkrecord_units), 3)

round(mean(post_be$heavy_drinking_day), 3)
round(sd(post_be$heavy_drinking_day), 3)

round(mean(post_be$alc_free_days), 3)
round(sd(post_be$alc_free_days), 3)

# total nr of sessions ------------------------------------------

# check ACF and PACF to identify plausible values for the AR and MA terms

acf(df_rq3be_clean$total_number_sessions, plot = TRUE)
pacf(df_rq3be_clean$total_number_sessions, plot = TRUE)

# test models with different plausible AR and MA terms and select the best fitting option with the AIC

mod1_sessions <- gamm(total_number_sessions ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                    data = df_rq3be_clean, family = gaussian(link = "identity"))

mod2_sessions <- gamm(total_number_sessions ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                    data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 0))

mod3_sessions <- gamm(total_number_sessions ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                    data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 1))

mod4_sessions <- gamm(total_number_sessions ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                    data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 0))

mod5_sessions <- gamm(total_number_sessions ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                    data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 1))

mod6_sessions <- gamm(total_number_sessions ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                    data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 2))

AIC(mod1_sessions$lme, mod2_sessions$lme, mod3_sessions$lme, mod4_sessions$lme, mod5_sessions$lme, mod6_sessions$lme) # select mod1

# test linear, quadratic and cubic trends for best fitting model and select the best fitting option with the AIC

mod1b_sessions <- gamm(total_number_sessions ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                     data = df_rq3be_clean, family = gaussian(link = "identity"))

mod2b_sessions <- gamm(total_number_sessions ~ trend + level + slope + I(slope^2) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                     data = df_rq3be_clean, family = gaussian(link = "identity"))

mod3b_sessions <- gamm(total_number_sessions ~ trend + level + slope + I(slope^2) + I(slope^3) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                     data = df_rq3be_clean, family = gaussian(link = "identity"))

AIC(mod1b_sessions$lme, mod2b_sessions$lme, mod3b_sessions$lme) # select mod1b

# derive Bs and 95% CIs for best fitting model

new <- summary(mod1b_sessions$gam)
coef <- round(new$p.coef[2:4], 4)
se <- new$se[2:4]
lower <- round(coef-1.96*se, 4)
higher <- round(coef+1.96*se, 4)
p <- round(new$p.pv[2:4],4)
coef
lower
higher
p

# plot values

rq3be_sessions <- ggplot(data = df_rq3be_clean, aes(x = trend, y = total_number_sessions))+
  geom_line(aes(x = trend, y = total_number_sessions), colour = "black")+
  geom_smooth(aes(x = trend, y = fitted(mod1b_sessions$gam)), colour = "red")+
  coord_cartesian(ylim = c(0, 55))+
  labs(x = "Time", y = "Number of logins") +
  scale_x_continuous(breaks = c(9, 39, 70, 100, 131, 162, 192, 223, 253, 284, 315, 344, 375, 405, 436), 
                     labels = c("Apr-19", "May-19", "Jun-19", "Jul-19", "Aug-19", "Sept-19", "Oct-19",
                                "Nov-19", "Dec-19", "Jan-20", "Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20")) +
  geom_vline(xintercept = 367, linetype = "dotted", color = "blue", size = 1) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

if(!file.exists(here("outputs", "rq3be_sessions.png"))) ggsave(rq3be_sessions, filename = here("outputs", "rq3be_sessions.png"), 
                                                            dpi = 320, height = 8, width = 10)

# nr of days used ------------------------------------------

# check ACF and PACF to identify plausible values for the AR and MA terms

# check ACF and PACF to identify plausible values for the AR and MA terms

acf(df_rq3be_clean$no_days_used, plot = TRUE)
pacf(df_rq3be_clean$no_days_used, plot = TRUE)

# test models with different plausible AR and MA terms and select the best fitting option with the AIC

mod1_days <- gamm(no_days_used ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                      data = df_rq3be_clean, family = gaussian(link = "identity"))

mod2_days <- gamm(no_days_used ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                      data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 0))

mod3_days <- gamm(no_days_used ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                      data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 1))

mod4_days <- gamm(no_days_used ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                      data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 0))

mod5_days <- gamm(no_days_used ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                      data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 1))

mod6_days <- gamm(no_days_used ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                      data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 2))

AIC(mod1_days$lme, mod2_days$lme, mod3_days$lme, mod4_days$lme, mod5_days$lme, mod6_days$lme) # select mod1

# test linear, quadratic and cubic trends for best fitting model and select the best fitting option with the AIC

mod1b_days <- gamm(no_days_used ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                       data = df_rq3be_clean, family = gaussian(link = "identity"))

mod2b_days <- gamm(no_days_used ~ trend + level + slope + I(slope^2) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                       data = df_rq3be_clean, family = gaussian(link = "identity"))

mod3b_days <- gamm(no_days_used ~ trend + level + slope + I(slope^2) + I(slope^3) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                       data = df_rq3be_clean, family = gaussian(link = "identity"))

AIC(mod1b_days$lme, mod2b_days$lme, mod3b_days$lme) # select mod1b

# derive Bs and 95% CIs for best fitting model

new <- summary(mod1b_days$gam)
coef <- round(new$p.coef[2:4], 4)
se <- new$se[2:4]
lower <- round(coef-1.96*se, 4)
higher <- round(coef+1.96*se, 4)
p <- round(new$p.pv[2:4],4)
coef
lower
higher
p

# plot values

rq3be_days <- ggplot(data = df_rq3be_clean, aes(x = trend, y = no_days_used))+
  geom_line(aes(x = trend, y = no_days_used), colour = "black")+
  geom_smooth(aes(x = trend, y = fitted(mod1b_days$gam)), colour = "red")+
  coord_cartesian(ylim = c(0, 28))+
  labs(x = "Time", y = "Number of days used") +
  scale_x_continuous(breaks = c(9, 39, 70, 100, 131, 162, 192, 223, 253, 284, 315, 344, 375, 405, 436), 
                     labels = c("Apr-19", "May-19", "Jun-19", "Jul-19", "Aug-19", "Sept-19", "Oct-19",
                                "Nov-19", "Dec-19", "Jan-20", "Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20")) +
  geom_vline(xintercept = 367, linetype = "dotted", color = "blue", size = 1) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

if(!file.exists(here("outputs", "rq3be_days.png"))) ggsave(rq3be_days, filename = here("outputs", "rq3be_days.png"), 
                                                            dpi = 320, height = 8, width = 10)

# proportion screens viewed ------------------------------------------

# check ACF and PACF to identify plausible values for the AR and MA terms

acf(df_rq3be_clean$proportion_screens_viewed, plot = TRUE)
pacf(df_rq3be_clean$proportion_screens_viewed, plot = TRUE)

# test models with different plausible AR and MA terms and select the best fitting option with the AIC

mod1_screens <- gamm(proportion_screens_viewed ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                  data = df_rq3be_clean, family = gaussian(link = "identity"))

mod2_screens <- gamm(proportion_screens_viewed ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                  data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 0))

mod3_screens <- gamm(proportion_screens_viewed ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                  data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 1))

mod4_screens <- gamm(proportion_screens_viewed ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                  data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 0))

mod5_screens <- gamm(proportion_screens_viewed ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                  data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 1))

mod6_screens <- gamm(proportion_screens_viewed ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                  data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 2))

AIC(mod1_screens$lme, mod2_screens$lme, mod4_screens$lme, mod5_screens$lme, mod6_screens$lme) # select mod1

# test linear, quadratic and cubic trends for best fitting model and select the best fitting option with the AIC

mod1b_screens <- gamm(proportion_screens_viewed ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                   data = df_rq3be_clean, family = gaussian(link = "identity"))

mod2b_screens <- gamm(proportion_screens_viewed ~ trend + level + slope + I(slope^2) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                   data = df_rq3be_clean, family = gaussian(link = "identity"))

mod3b_screens <- gamm(proportion_screens_viewed ~ trend + level + slope + I(slope^2) + I(slope^3) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                   data = df_rq3be_clean, family = gaussian(link = "identity"))

AIC(mod1b_screens$lme, mod2b_screens$lme, mod3b_screens$lme) # select mod1b

# derive Bs and 95% CIs for best fitting model

new <- summary(mod1b_screens$gam)
coef <- round(new$p.coef[2:4], 4)
se <- new$se[2:4]
lower <- round(coef-1.96*se, 4)
higher <- round(coef+1.96*se, 4)
p <- round(new$p.pv[2:4],4)
coef
lower
higher
p

# plot values

rq3be_screens <- ggplot(data = df_rq3be_clean, aes(x = trend, y = proportion_screens_viewed))+
  geom_line(aes(x = trend, y = proportion_screens_viewed), colour = "black")+
  geom_smooth(aes(x = trend, y = fitted(mod1b_screens$gam)), colour = "red")+
  coord_cartesian(ylim = c(0.15, 0.6))+
  labs(x = "Time", y = "Percentage screens viewed") +
  scale_x_continuous(breaks = c(9, 39, 70, 100, 131, 162, 192, 223, 253, 284, 315, 344, 375, 405, 436), 
                     labels = c("Apr-19", "May-19", "Jun-19", "Jul-19", "Aug-19", "Sept-19", "Oct-19",
                                "Nov-19", "Dec-19", "Jan-20", "Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20")) +
  geom_vline(xintercept = 367, linetype = "dotted", color = "blue", size = 1) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

if(!file.exists(here("outputs", "rq3be_screens.png"))) ggsave(rq3be_screens, filename = here("outputs", "rq3be_screens.png"), 
                                                           dpi = 320, height = 8, width = 10)

# time spent (mins) ------------------------------------------

# check ACF and PACF to identify plausible values for the AR and MA terms

acf(df_rq3be_clean$time_minutes, plot = TRUE)
pacf(df_rq3be_clean$time_minutes, plot = TRUE)

# test models with different plausible AR and MA terms and select the best fitting option with the AIC

mod1_time <- gamm(time_minutes ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                     data = df_rq3be_clean, family = gaussian(link = "identity"))

mod2_time <- gamm(time_minutes ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                     data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 0))

mod3_time <- gamm(time_minutes ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                     data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 1))

mod4_time <- gamm(time_minutes ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                     data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 0))

mod5_time <- gamm(time_minutes ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                     data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 1))

mod6_time <- gamm(time_minutes ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                     data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 2))

AIC(mod1_time$lme, mod2_time$lme, mod3_time$lme, mod4_time$lme, mod5_time$lme, mod6_time$lme) # select mod3

# test linear, quadratic and cubic trends for best fitting model and select the best fitting option with the AIC

mod1b_time <- gamm(time_minutes ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                      data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 1))

mod2b_time <- gamm(time_minutes ~ trend + level + slope + I(slope^2) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                      data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 1))

mod3b_time <- gamm(time_minutes ~ trend + level + slope + I(slope^2) + I(slope^3) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                      data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 1))

AIC(mod1b_time$lme, mod2b_time$lme, mod3b_time$lme) # select mod1b

# derive Bs and 95% CIs for best fitting model

new <- summary(mod1b_time$gam)
coef <- round(new$p.coef[2:4], 4)
se <- new$se[2:4]
lower <- round(coef-1.96*se, 4)
higher <- round(coef+1.96*se, 4)
p <- round(new$p.pv[2:4],4)
coef
lower
higher
p

# plot values

rq3be_time <- ggplot(data = df_rq3be_clean, aes(x = trend, y = time_minutes))+
  geom_line(aes(x = trend, y = time_minutes), colour = "black")+
  geom_smooth(aes(x = trend, y = fitted(mod1b_time$gam)), colour = "red")+
  coord_cartesian(ylim = c(5, 280))+
  labs(x = "Time", y = "Time spent on app") +
  scale_x_continuous(breaks = c(9, 39, 70, 100, 131, 162, 192, 223, 253, 284, 315, 344, 375, 405, 436), 
                     labels = c("Apr-19", "May-19", "Jun-19", "Jul-19", "Aug-19", "Sept-19", "Oct-19",
                                "Nov-19", "Dec-19", "Jan-20", "Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20")) +
  geom_vline(xintercept = 367, linetype = "dotted", color = "blue", size = 1) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

if(!file.exists(here("outputs", "rq3be_time.png"))) ggsave(rq3be_time, filename = here("outputs", "rq3be_time.png"), 
                                                              dpi = 320, height = 8, width = 10)

# nr alcohol units ------------------------------------------

# check ACF and PACF to identify plausible values for the AR and MA terms

acf(df_rq3be_clean$drinkrecord_units, plot = TRUE)
pacf(df_rq3be_clean$drinkrecord_units, plot = TRUE)

# test models with different plausible AR and MA terms and select the best fitting option with the AIC

mod1_units <- gamm(drinkrecord_units ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                  data = df_rq3be_clean, family = gaussian(link = "identity"))

mod2_units <- gamm(drinkrecord_units ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                  data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 0))

mod3_units <- gamm(drinkrecord_units ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                  data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 1))

mod4_units <- gamm(drinkrecord_units ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                  data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 0))

mod5_units <- gamm(drinkrecord_units ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                  data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 1))

mod6_units <- gamm(drinkrecord_units ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                  data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 2))

AIC(mod1_units$lme, mod2_units$lme, mod3_units$lme, mod4_units$lme, mod5_units$lme, mod6_units$lme) # select mod1

# test linear, quadratic and cubic trends for best fitting model and select the best fitting option with the AIC

mod1b_units <- gamm(drinkrecord_units ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                   data = df_rq3be_clean, family = gaussian(link = "identity"))

mod2b_units <- gamm(drinkrecord_units ~ trend + level + slope + I(slope^2) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                   data = df_rq3be_clean, family = gaussian(link = "identity"))

mod3b_units <- gamm(drinkrecord_units ~ trend + level + slope + I(slope^2) + I(slope^3) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                   data = df_rq3be_clean, family = gaussian(link = "identity"))

AIC(mod1b_units$lme, mod2b_units$lme, mod3b_units$lme) # select mod1b

# derive Bs and 95% CIs for best fitting model

new <- summary(mod1b_units$gam)
coef <- round(new$p.coef[2:4], 4)
se <- new$se[2:4]
lower <- round(coef-1.96*se, 4)
higher <- round(coef+1.96*se, 4)
p <- round(new$p.pv[2:4],4)
coef
lower
higher
p

# plot values

rq3be_units <- ggplot(data = df_rq3be_clean, aes(x = trend, y = drinkrecord_units))+
  geom_line(aes(x = trend, y = drinkrecord_units), colour = "black")+
  geom_smooth(aes(x = trend, y = fitted(mod1b_units$gam)), colour = "red")+
  coord_cartesian(ylim = c(5, 370))+
  labs(x = "Time", y = "Alcohol units") +
  scale_x_continuous(breaks = c(9, 39, 70, 100, 131, 162, 192, 223, 253, 284, 315, 344, 375, 405, 436), 
                     labels = c("Apr-19", "May-19", "Jun-19", "Jul-19", "Aug-19", "Sept-19", "Oct-19",
                                "Nov-19", "Dec-19", "Jan-20", "Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20")) +
  geom_vline(xintercept = 367, linetype = "dotted", color = "blue", size = 1) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

if(!file.exists(here("outputs", "rq3be_units.png"))) ggsave(rq3be_units, filename = here("outputs", "rq3be_units.png"), 
                                                           dpi = 320, height = 8, width = 10)

# nr heavy drinking days ------------------------------------------

# check ACF and PACF to identify plausible values for the AR and MA terms

acf(df_rq3be_clean$heavy_drinking_day, plot = TRUE)
pacf(df_rq3be_clean$heavy_drinking_day, plot = TRUE)

# test models with different plausible AR and MA terms and select the best fitting option with the AIC

mod1_heavy <- gamm(heavy_drinking_day ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                   data = df_rq3be_clean, family = gaussian(link = "identity"))

mod2_heavy <- gamm(heavy_drinking_day ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                   data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 0))

mod3_heavy <- gamm(heavy_drinking_day ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                   data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 1))

mod4_heavy <- gamm(heavy_drinking_day ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                   data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 0))

mod5_heavy <- gamm(heavy_drinking_day ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                   data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 1))

mod6_heavy <- gamm(heavy_drinking_day ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                   data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 2))

AIC(mod1_heavy$lme, mod4_heavy$lme, mod5_heavy$lme, mod6_heavy$lme) # select mod6

# test linear, quadratic and cubic trends for best fitting model and select the best fitting option with the AIC

mod1b_heavy <- gamm(heavy_drinking_day ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                    data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 2))

mod2b_heavy <- gamm(heavy_drinking_day ~ trend + level + slope + I(slope^2) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                    data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 2))

mod3b_heavy <- gamm(heavy_drinking_day ~ trend + level + slope + I(slope^2) + I(slope^3) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                    data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 2))

AIC(mod1b_heavy$lme, mod2b_heavy$lme) # select mod1b

# derive Bs and 95% CIs for best fitting model

new <- summary(mod1b_heavy$gam)
coef <- round(new$p.coef[2:4], 4)
se <- new$se[2:4]
lower <- round(coef-1.96*se, 4)
higher <- round(coef+1.96*se, 4)
p <- round(new$p.pv[2:4],4)
coef
lower
higher
p

# plot values

rq3be_heavy <- ggplot(data = df_rq3be_clean, aes(x = trend, y = heavy_drinking_day))+
  geom_line(aes(x = trend, y = heavy_drinking_day), colour = "black")+
  geom_smooth(aes(x = trend, y = fitted(mod1b_heavy$gam)), colour = "red")+
  coord_cartesian(ylim = c(0, 30))+
  labs(x = "Time", y = "Heavy drinking days") +
  scale_x_continuous(breaks = c(9, 39, 70, 100, 131, 162, 192, 223, 253, 284, 315, 344, 375, 405, 436), 
                     labels = c("Apr-19", "May-19", "Jun-19", "Jul-19", "Aug-19", "Sept-19", "Oct-19",
                                "Nov-19", "Dec-19", "Jan-20", "Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20")) +
  geom_vline(xintercept = 367, linetype = "dotted", color = "blue", size = 1) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

if(!file.exists(here("outputs", "rq3be_heavy.png"))) ggsave(rq3be_heavy, filename = here("outputs", "rq3be_heavy.png"), 
                                                            dpi = 320, height = 8, width = 10)

# nr alcohol free days ------------------------------------------

# check ACF and PACF to identify plausible values for the AR and MA terms

acf(df_rq3be_clean$alc_free_days, plot = TRUE)
pacf(df_rq3be_clean$alc_free_days, plot = TRUE)

# test models with different plausible AR and MA terms and select the best fitting option with the AIC

mod1_free <- gamm(alc_free_days ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                   data = df_rq3be_clean, family = gaussian(link = "identity"))

mod2_free <- gamm(alc_free_days ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                   data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 0))

mod3_free <- gamm(alc_free_days ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                   data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 1))

mod4_free <- gamm(alc_free_days ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                   data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 0))

mod5_free <- gamm(alc_free_days ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                   data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 1))

mod6_free <- gamm(alc_free_days ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                   data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 2))

AIC(mod1_free$lme, mod2_free$lme, mod3_free$lme, mod4_free$lme, mod5_free$lme, mod6_free$lme) # select mod5

# test linear, quadratic and cubic trends for best fitting model and select the best fitting option with the AIC

mod1b_free <- gamm(alc_free_days ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                    data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 1))

mod2b_free <- gamm(alc_free_days ~ trend + level + slope + I(slope^2) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                    data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 1))

mod3b_free <- gamm(alc_free_days ~ trend + level + slope + I(slope^2) + I(slope^3) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                    data = df_rq3be_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 1))

AIC(mod1b_free$lme, mod2b_free$lme, mod3b_free$lme) # select mod1b

# derive Bs and 95% CIs for best fitting model

new <- summary(mod1b_free$gam)
coef <- round(new$p.coef[2:4], 4)
se <- new$se[2:4]
lower <- round(coef-1.96*se, 4)
higher <- round(coef+1.96*se, 4)
p <- round(new$p.pv[2:4],4)
coef
lower
higher
p

# plot values

rq3be_free <- ggplot(data = df_rq3be_clean, aes(x = trend, y = alc_free_days))+
  geom_line(aes(x = trend, y = alc_free_days), colour = "black")+
  geom_smooth(aes(x = trend, y = fitted(mod1b_free$gam)), colour = "red")+
  coord_cartesian(ylim = c(0, 70))+
  labs(x = "Time", y = "Alcohol free days") +
  scale_x_continuous(breaks = c(9, 39, 70, 100, 131, 162, 192, 223, 253, 284, 315, 344, 375, 405, 436), 
                     labels = c("Apr-19", "May-19", "Jun-19", "Jul-19", "Aug-19", "Sept-19", "Oct-19",
                                "Nov-19", "Dec-19", "Jan-20", "Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20")) +
  geom_vline(xintercept = 367, linetype = "dotted", color = "blue", size = 1) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

if(!file.exists(here("outputs", "rq3be_free.png"))) ggsave(rq3be_free, filename = here("outputs", "rq3be_free.png"), 
                                                            dpi = 320, height = 8, width = 10)
