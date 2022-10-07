# load packages and data --------------------------------------------------

source(here::here("scripts", "0_load_packages_and_data.R"))

df_rq1_clean <- read_rds(here::here("data", "df_rq1_clean.rds"))

# RQ1 - effect of lockdown on regular users -------------------------------

# descriptives overall, pre- and post-lockdown

# overall

length(df_rq1_clean$mean_percent_screens_viewed)
range(df_rq1a$total_IDs_per_day)

round(mean(df_rq1_clean$mean_percent_screens_viewed), 3)
round(sd(df_rq1_clean$mean_percent_screens_viewed), 3)

round(mean(df_rq1_clean$mean_time_on_app), 3)
round(sd(df_rq1_clean$mean_time_on_app), 3)

round(mean(df_rq1_clean$mean_number_sessions), 3)
round(sd(df_rq1_clean$mean_number_sessions), 3)

round(mean(df_rq1_clean$mean_units_per_day), 3)
round(sd(df_rq1_clean$mean_units_per_day), 3)

round(mean(df_rq1_clean$mean_heavy_drinking_days), 3)
round(sd(df_rq1_clean$mean_heavy_drinking_days), 3)

round(mean(df_rq1_clean$mean_alcohol_free_days), 3)
round(sd(df_rq1_clean$mean_alcohol_free_days), 3)

# pre-lockdown

pre <- df_rq1_clean %>%
  filter(level == 0)

round(mean(pre$mean_percent_screens_viewed), 3)
round(sd(pre$mean_percent_screens_viewed), 3)

round(mean(pre$mean_time_on_app), 3)
round(sd(pre$mean_time_on_app), 3)

round(mean(pre$mean_number_sessions), 3)
round(sd(pre$mean_number_sessions), 3)

round(mean(pre$mean_units_per_day), 3)
round(sd(pre$mean_units_per_day), 3)

round(mean(pre$mean_heavy_drinking_days), 3)
round(sd(pre$mean_heavy_drinking_days), 3)

round(mean(pre$mean_alcohol_free_days), 3)
round(sd(pre$mean_alcohol_free_days), 3)

# post-lockdown

post <- df_rq1_clean %>%
  filter(level == 1)

round(mean(post$mean_percent_screens_viewed), 3)
round(sd(post$mean_percent_screens_viewed), 3)

round(mean(post$mean_time_on_app), 3)
round(sd(post$mean_time_on_app), 3)

round(mean(post$mean_number_sessions), 3)
round(sd(post$mean_number_sessions), 3)

round(mean(post$mean_units_per_day), 3)
round(sd(post$mean_units_per_day), 3)

round(mean(post$mean_heavy_drinking_days), 3)
round(sd(post$mean_heavy_drinking_days), 3)

round(mean(post$mean_alcohol_free_days), 3)
round(sd(post$mean_alcohol_free_days), 3)

# mean proportion screens viewed ------------------------------------------

# check ACF and PACF to identify plausible values for the AR and MA terms

acf(df_rq1_clean$mean_percent_screens_viewed, plot = TRUE)
pacf(df_rq1_clean$mean_percent_screens_viewed, plot = TRUE)

# test models with different plausible AR and MA terms and select the best fitting option with the AIC

mod1_screens <- gamm(mean_percent_screens_viewed ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
             data = df_rq1_clean, family = gaussian(link = "identity"))

mod2_screens <- gamm(mean_percent_screens_viewed ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
             data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 0))

mod3_screens <- gamm(mean_percent_screens_viewed ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
             data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 1))

mod4_screens <- gamm(mean_percent_screens_viewed ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
             data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 0))

mod5_screens <- gamm(mean_percent_screens_viewed ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
             data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 1))

mod6_screens <- gamm(mean_percent_screens_viewed ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
             data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 2))

AIC(mod1_screens$lme, mod2_screens$lme, mod3_screens$lme, mod4_screens$lme, mod5_screens$lme, mod6_screens$lme) # select mod6

# test linear, quadratic and cubic trends for best fitting model and select the best fitting option with the AIC

mod1b_screens <- gamm(mean_percent_screens_viewed ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
              data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 2))

mod2b_screens <- gamm(mean_percent_screens_viewed ~ trend + level + slope + I(slope^2) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
              data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 2))

mod3b_screens <- gamm(mean_percent_screens_viewed ~ trend + level + slope + I(slope^2) + I(slope^3) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
              data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 2))

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

rq1a_plot <- ggplot(data = df_rq1_clean, aes(x = trend, y = mean_percent_screens_viewed)) +
  geom_line(aes(x = trend, y = mean_percent_screens_viewed), colour = "black") +
  geom_point(aes(x = trend, y = fitted(mod1b_screens$gam), colour = "red")) +
  geom_line(aes(x = trend, y = fitted(mod1b_screens$gam), colour = "red")) +
  coord_cartesian(ylim = c(2, 9)) +
  labs(x = "Time", y = "Mean percent screens viewed") +
  scale_x_continuous(breaks = c(9, 39, 70, 100, 131, 162, 192, 223, 253, 284, 315, 344, 375, 405, 436, 466), 
                     labels = c("Apr-19", "May-19", "Jun-19", "Jul-19", "Aug-19", "Sept-19", "Oct-19",
                                "Nov-19", "Dec-19", "Jan-20", "Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20", "Jul-20")) +
  geom_vline(xintercept = 367, linetype = "dotted", color = "blue", size = 1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 17))

if(!file.exists(here("outputs", "rq1a_plot.png"))) ggsave(rq1a_plot, filename = here("outputs", "rq1a_plot.png"), 
                                                         dpi = 320, height = 8, width = 10)

# mean time on app --------------------------------------------------------

# check ACF and PACF to identify plausible values for the AR and MA terms

acf(df_rq1_clean$mean_time_on_app, plot = TRUE)
pacf(df_rq1_clean$mean_time_on_app, plot = TRUE)

# test models with different plausible AR and MA terms and select the best fitting option with the AIC

mod1_time <- gamm(mean_time_on_app ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                     data = df_rq1_clean, family = gaussian(link = "identity"))

mod2_time <- gamm(mean_time_on_app ~ trend + level + slope + s(day, bs="cc", k=7), 
                     data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 0))

mod3_time <- gamm(mean_time_on_app ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                     data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 1))

mod4_time <- gamm(mean_time_on_app ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                     data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 0))

mod5_time <- gamm(mean_time_on_app ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                     data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 1))

mod6_time <- gamm(mean_time_on_app ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                     data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 2))

AIC(mod1_time$lme, mod2_time$lme, mod3_time$lme, mod4_time$lme, mod5_time$lme, mod6_time$lme) # select mod6

# test linear, quadratic and cubic trends for best fitting model and select the best fitting option with the AIC

mod1b_time <- gamm(mean_time_on_app ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                      data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 2))

mod2b_time <- gamm(mean_time_on_app ~ trend + level + slope + I(slope^2) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                      data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 2))

mod3b_time <- gamm(mean_time_on_app ~ trend + level + slope + I(slope^2) + I(slope^3) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                      data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 2))

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

rq1b_plot <- ggplot(data = df_rq1_clean, aes(x = trend, y = mean_time_on_app)) +
  geom_line(aes(x = trend, y = mean_time_on_app), colour = "black") +
  geom_point(aes(x = trend, y = fitted(mod1b_time$gam)), colour = "red") +
  geom_line(aes(x = trend, y = fitted(mod1b_time$gam)), colour = "red") +
  coord_cartesian(ylim = c(0, 6)) +
  labs(x = "Time", y = "Mean time on app in minutes") +
  scale_x_continuous(breaks = c(9, 39, 70, 100, 131, 162, 192, 223, 253, 284, 315, 344, 375, 405, 436, 466), 
                     labels = c("Apr-19", "May-19", "Jun-19", "Jul-19", "Aug-19", "Sept-19", "Oct-19",
                                "Nov-19", "Dec-19", "Jan-20", "Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20", "Jul-20")) +
  geom_vline(xintercept = 367, linetype = "dotted", color = "blue", size = 1) +
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 17))

if(!file.exists(here("outputs", "rq1b_plot.png"))) ggsave(rq1b_plot, filename = here("outputs", "rq1b_plot.png"), 
                                                         dpi = 320, height = 8, width = 10)

# mean nr of sessions -----------------------------------------------------

# check ACF and PACF to identify plausible values for the AR and MA terms

acf(df_rq1_clean$mean_number_sessions, plot = TRUE)
pacf(df_rq1_clean$mean_number_sessions, plot = TRUE)

# test models with different plausible AR and MA terms and select the best fitting option with the AIC

mod1_sessions <- gamm(mean_number_sessions ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                  data = df_rq1_clean, family = gaussian(link = "identity"))

mod2_sessions <- gamm(mean_number_sessions ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                  data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 0))

mod3_sessions <- gamm(mean_number_sessions ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                  data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 1))

mod4_sessions <- gamm(mean_number_sessions ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                  data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 0))

mod5_sessions <- gamm(mean_number_sessions ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                  data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 1))

mod6_sessions <- gamm(mean_number_sessions ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                  data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 2))

AIC(mod1_sessions$lme, mod2_sessions$lme, mod3_sessions$lme, mod4_sessions$lme, mod6_sessions$lme) # select mod2

# test linear, quadratic and cubic trends for best fitting model and select the best fitting option with the AIC

mod1b_sessions <- gamm(mean_number_sessions ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                   data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 0))

mod2b_sessions <- gamm(mean_number_sessions ~ trend + level + slope + I(slope^2) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                   data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 0))

mod3b_sessions <- gamm(mean_number_sessions ~ trend + level + slope + I(slope^2) + I(slope^3) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                   data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 0))

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

rq1c_plot <- ggplot(data = df_rq1_clean, aes(x = trend, y = mean_number_sessions)) +
  geom_line(aes(x = trend, y = mean_number_sessions), colour = "black") +
  geom_point(aes(x = trend, y = fitted(mod1b_sessions$gam), colour = "red")) +
  geom_line(aes(x = trend, y = fitted(mod1b_sessions$gam), colour = "red")) +
  coord_cartesian(ylim = c(0.2, 1.5)) +
  labs(x = "Time", y = "Mean number of sessions") +
  scale_x_continuous(breaks = c(9, 39, 70, 100, 131, 162, 192, 223, 253, 284, 315, 344, 375, 405, 436, 466), 
                     labels = c("Apr-19", "May-19", "Jun-19", "Jul-19", "Aug-19", "Sept-19", "Oct-19",
                                "Nov-19", "Dec-19", "Jan-20", "Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20", "Jul-20")) +
  geom_vline(xintercept = 367, linetype = "dotted", color = "blue", size = 1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 17))

if(!file.exists(here("outputs", "rq1c_plot.png"))) ggsave(rq1c_plot, filename = here("outputs", "rq1c_plot.png"), 
                                                         dpi = 320, height = 8, width = 10)

# mean units per day ------------------------------------------------------

# check ACF and PACF to identify plausible values for the AR and MA terms

acf(df_rq1_clean$mean_units_per_day, plot = TRUE)
pacf(df_rq1_clean$mean_units_per_day, plot = TRUE)

# test models with different plausible AR and MA terms and select the best fitting option with the AIC

mod1_units <- gamm(mean_units_per_day ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                      data = df_rq1_clean, family = gaussian(link = "identity"))

mod2_units <- gamm(mean_units_per_day ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                      data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 0))

mod3_units <- gamm(mean_units_per_day ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                      data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 1))

mod4_units <- gamm(mean_units_per_day ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                      data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 0))

mod5_units <- gamm(mean_units_per_day ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                      data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 1))

mod6_units <- gamm(mean_units_per_day ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                      data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 2))

AIC(mod1_units$lme, mod2_units$lme, mod3_units$lme, mod4_units$lme, mod5_units$lme, mod6_units$lme) # select mod4

# test linear, quadratic and cubic trends for best fitting model and select the best fitting option with the AIC

mod1b_units <- gamm(mean_units_per_day ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                    data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 0))

mod2b_units <- gamm(mean_units_per_day ~ trend + level + slope + I(slope^2) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                    data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 0))

mod3b_units <- gamm(mean_units_per_day ~ trend + level + slope + I(slope^2) + I(slope^3) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                    data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 0))

AIC(mod1b_units$lme, mod2b_units$lme, mod3b_units$lme) # select mod3b

# derive Bs and 95% CIs for best fitting model

new <- summary(mod3b_units$gam)
coef <- round(new$p.coef[2:6], 4)
se <- new$se[2:6]
lower <- round(coef-1.96*se, 4)
higher <- round(coef+1.96*se, 4)
p <- round(new$p.pv[2:6],4)
coef
lower
higher
p

# plot values

rq1d_plot <- ggplot(data = df_rq1_clean, aes(x = trend, y = mean_units_per_day)) +
  geom_line(aes(x = trend, y = mean_units_per_day), colour = "black") +
  geom_point(aes(x = trend, y = fitted(mod3b_units$gam)), colour = "red") +
  geom_line(aes(x = trend, y = fitted(mod3b_units$gam)), colour = "red") +
  coord_cartesian(ylim = c(0, 11)) +
  labs(x = "Time", y = "Mean alcohol units per day") +
  scale_x_continuous(breaks = c(9, 39, 70, 100, 131, 162, 192, 223, 253, 284, 315, 344, 375, 405, 436, 466), 
                     labels = c("Apr-19", "May-19", "Jun-19", "Jul-19", "Aug-19", "Sept-19", "Oct-19",
                                "Nov-19", "Dec-19", "Jan-20", "Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20", "Jul-20")) +
  geom_vline(xintercept = 367, linetype = "dotted", color = "blue", size = 1) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 17))

if(!file.exists(here("outputs", "rq1d_plot.png"))) ggsave(rq1d_plot, filename = here("outputs", "rq1d_plot.png"), 
                                                         dpi = 320, height = 8, width = 10)

# mean heavy drinking days ------------------------------------------------

# check ACF and PACF to identify plausible values for the AR and MA terms

acf(df_rq1_clean$mean_heavy_drinking_days, plot = TRUE)
pacf(df_rq1_clean$mean_heavy_drinking_days, plot = TRUE)

# test models with different plausible AR and MA terms and select the best fitting option with the AIC

mod1_drinking <- gamm(mean_heavy_drinking_days ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                   data= df_rq1_clean, family = gaussian(link = "identity"))

mod2_drinking <- gamm(mean_heavy_drinking_days ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                   data= df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 0))

mod3_drinking <- gamm(mean_heavy_drinking_days ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                   data= df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 1))

mod4_drinking <- gamm(mean_heavy_drinking_days ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                   data= df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 0))

mod5_drinking <- gamm(mean_heavy_drinking_days ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                   data= df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 1))

mod6_drinking <- gamm(mean_heavy_drinking_days ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                   data= df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 2))

AIC(mod1_drinking$lme, mod2_drinking$lme, mod3_drinking$lme, mod4_drinking$lme, mod5_drinking$lme, mod6_drinking$lme) # select mod4

# test linear, quadratic and cubic trends for best fitting model and select the best fitting option with the AIC

mod1b_drinking <- gamm(mean_heavy_drinking_days ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                    data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 0))

mod2b_drinking <- gamm(mean_heavy_drinking_days ~ trend + level + slope + I(slope^2) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                    data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 0))

mod3b_drinking <- gamm(mean_heavy_drinking_days ~ trend + level + slope + I(slope^2) + I(slope^3) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                    data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 0))

AIC(mod1b_drinking$lme, mod2b_drinking$lme, mod3b_drinking$lme) # select mod3b

# derive Bs and 95% CIs for best fitting model

new <- summary(mod3b_drinking$gam)
coef <- round(new$p.coef[2:6], 4)
se <- new$se[2:6]
lower <- round(coef-1.96*se, 4)
higher <- round(coef+1.96*se, 4)
p <- round(new$p.pv[2:6],4)
coef
lower
higher
p

# plot values

rq1e_plot <- ggplot(data = df_rq1_clean, aes(x = trend, y = mean_heavy_drinking_days)) +
  geom_line(aes(x = trend, y = mean_heavy_drinking_days), colour = "black") +
  geom_point(aes(x = trend, y = fitted(mod3b_drinking$gam)), colour = "red") +
  geom_line(aes(x = trend, y = fitted(mod3b_drinking$gam)), colour = "red") +
  coord_cartesian(ylim = c(0, 0.7)) +
  labs(x = "Time", y = "Mean nr of heavy drinking days") +
  scale_x_continuous(breaks = c(9, 39, 70, 100, 131, 162, 192, 223, 253, 284, 315, 344, 375, 405, 436, 466), 
                     labels = c("Apr-19", "May-19", "Jun-19", "Jul-19", "Aug-19", "Sept-19", "Oct-19",
                                "Nov-19", "Dec-19", "Jan-20", "Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20", "Jul-20")) +
  geom_vline(xintercept = 367, linetype = "dotted", color = "blue", size = 1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 17))

if(!file.exists(here("outputs", "rq1e_plot.png"))) ggsave(rq1e_plot, filename = here("outputs", "rq1e_plot.png"), 
                                                         dpi = 320, height = 8, width = 10)

# mean alcohol free days --------------------------------------------------

# check ACF and PACF to identify plausible values for the AR and MA terms

acf(df_rq1_clean$mean_alcohol_free_days, plot = TRUE)
pacf(df_rq1_clean$mean_alcohol_free_days, plot = TRUE)

# test models with different plausible AR and MA terms and select the best fitting option with the AIC

mod1_free <- gamm(mean_alcohol_free_days ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                      data = df_rq1_clean, family = gaussian(link = "identity"))

mod2_free <- gamm(mean_alcohol_free_days ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                      data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 0))

mod3_free <- gamm(mean_alcohol_free_days ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                      data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 1))

mod4_free <- gamm(mean_alcohol_free_days ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                      data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 0))

mod5_free <- gamm(mean_alcohol_free_days ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                      data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 1))

mod6_free <- gamm(mean_alcohol_free_days ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                      data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 2))

AIC(mod1_free$lme, mod2_free$lme, mod3_free$lme, mod4_free$lme, mod5_free$lme, mod6_free$lme) # select mod5

# test linear, quadratic and cubic trends for best fitting model and select the best fitting option with the AIC

mod1b_free <- gamm(mean_alcohol_free_days ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                       data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 1))

mod2b_free <- gamm(mean_alcohol_free_days ~ trend + level + slope + I(slope^2) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                       data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 1))

mod3b_free <- gamm(mean_alcohol_free_days ~ trend + level + slope + I(slope^2) + I(slope^3) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                       data = df_rq1_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 1))

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

rq1f_plot <- ggplot(data = df_rq1_clean, aes(x = trend, y = mean_alcohol_free_days)) +
  geom_line(aes(x = trend, y = mean_alcohol_free_days), colour = "black") +
  geom_point(aes(x = trend, y = fitted(mod1b_free$gam)), colour = "red") +
  geom_line(aes(x = trend, y = fitted(mod1b_free$gam)), colour = "red") +
  coord_cartesian(ylim = c(0, 1.5)) +
  labs(x = "Time", y = "Mean nr of alcohol free days") +
  scale_x_continuous(breaks = c(9, 39, 70, 100, 131, 162, 192, 223, 253, 284, 315, 344, 375, 405, 436, 466), 
                     labels = c("Apr-19", "May-19", "Jun-19", "Jul-19", "Aug-19", "Sept-19", "Oct-19",
                                "Nov-19", "Dec-19", "Jan-20", "Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20", "Jul-20")) +
  geom_vline(xintercept = 367, linetype = "dotted", color = "blue", size = 1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 17))

if(!file.exists(here("outputs", "rq1f_plot.png"))) ggsave(rq1f_plot, filename = here("outputs", "rq1f_plot.png"), 
                                                         dpi = 320, height = 8, width = 10)
