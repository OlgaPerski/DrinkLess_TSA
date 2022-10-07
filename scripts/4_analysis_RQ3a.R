# load packages and data --------------------------------------------------

source(here::here("scripts", "0_load_packages_and_data.R"))

df_rq3a_clean <- read_rds(here::here("data", "df_rq3a_clean.rds"))

# RQ3a - effect of lockdown on sociodemographic and drinking characteristics -------------------------------

# descriptives overall, pre- and post-lockdown

# overall

round(mean(df_rq3a_clean$gender), 3)
round(sd(df_rq3a_clean$gender), 3)

round(mean(df_rq3a_clean$age), 3)
round(sd(df_rq3a_clean$age), 3)

round(mean(df_rq3a_clean$employment_type), 3)
round(sd(df_rq3a_clean$employment_type), 3)

round(mean(df_rq3a_clean$AUDIT_score), 3)
round(sd(df_rq3a_clean$AUDIT_score), 3)

round(mean(df_rq3a_clean$at_risk), 3)
round(sd(df_rq3a_clean$at_risk), 3)

# pre-lockdown

pre_ses <- df_rq3a_clean %>%
  filter(level == 0)

round(mean(pre_ses$gender), 3)
round(sd(pre_ses$gender), 3)

round(mean(pre_ses$age), 3)
round(sd(pre_ses$age), 3)

round(mean(pre_ses$employment_type), 3)
round(sd(pre_ses$employment_type), 3)

round(mean(pre_ses$AUDIT_score), 3)
round(sd(pre_ses$AUDIT_score), 3)

round(mean(pre_ses$at_risk), 3)
round(sd(pre_ses$at_risk), 3)

# post-lockdown

post_ses <- df_rq3a_clean %>%
  filter(level == 1)

round(mean(post_ses$gender), 3)
round(sd(post_ses$gender), 3)

round(mean(post_ses$age), 3)
round(sd(post_ses$age), 3)

round(mean(post_ses$employment_type), 3)
round(sd(post_ses$employment_type), 3)

round(mean(post_ses$AUDIT_score), 3)
round(sd(post_ses$AUDIT_score), 3)

round(mean(post_ses$at_risk), 3)
round(sd(post_ses$at_risk), 3)

# gender ------------------------------------------

# check ACF and PACF to identify plausible values for the AR and MA terms

acf(df_rq3a_clean$gender, plot = TRUE)
pacf(df_rq3a_clean$gender, plot = TRUE)

# test models with different plausible AR and MA terms and select the best fitting option with the AIC

mod1_gender <- gamm(gender ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                     data = df_rq3a_clean, family = gaussian(link = "identity"))

mod2_gender <- gamm(gender ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                     data = df_rq3a_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 0))

mod3_gender <- gamm(gender ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                     data = df_rq3a_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 1))

mod4_gender <- gamm(gender ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                     data = df_rq3a_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 0))

mod5_gender <- gamm(gender ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                     data = df_rq3a_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 1))

mod6_gender <- gamm(gender ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                     data = df_rq3a_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 2))

AIC(mod1_gender$lme, mod2_gender$lme, mod4_gender$lme, mod5_gender$lme) # select mod2

# test linear, quadratic and cubic trends for best fitting model and select the best fitting option with the AIC

mod1b_gender <- gamm(gender ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                      data = df_rq3a_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 0))

mod2b_gender <- gamm(gender ~ trend + level + slope + I(slope^2) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                      data = df_rq3a_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 0))

mod3b_gender <- gamm(gender ~ trend + level + slope + I(slope^2) + I(slope^3) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                      data = df_rq3a_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 0))

AIC(mod1b_gender$lme, mod2b_gender$lme, mod3b_gender$lme) # select mod1b

# derive Bs and 95% CIs for best fitting model

new <- summary(mod1b_gender$gam)
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

rq3a_gender <- ggplot(data = df_rq3a_clean, aes(x = trend, y = gender)) +
  geom_line(aes(x = trend, y = gender), colour = "black") +
  geom_point(aes(x = trend, y = fitted(mod1b_gender$gam)), colour = "red") +
  geom_line(aes(x = trend, y = fitted(mod1b_gender$gam)), colour = "red") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Time", y = "% Female") +
  scale_x_continuous(breaks = c(9, 39, 70, 100, 131, 162, 192, 223, 253, 284, 315, 344, 375, 405, 436, 466), 
                     labels = c("Apr-19", "May-19", "Jun-19", "Jul-19", "Aug-19", "Sept-19", "Oct-19",
                                "Nov-19", "Dec-19", "Jan-20", "Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20", "Jul-20")) +
  geom_vline(xintercept = 367, linetype = "dotted", color = "blue", size = 1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 17))

if(!file.exists(here("outputs", "rq3a_gender.png"))) ggsave(rq3a_gender, filename = here("outputs", "rq3a_gender.png"), 
                                                         dpi = 320, height = 8, width = 10)

# age ------------------------------------------

# check ACF and PACF to identify plausible values for the AR and MA terms

acf(df_rq3a_clean$age, plot = TRUE)
pacf(df_rq3a_clean$age, plot = TRUE)

# test models with different plausible AR and MA terms and select the best fitting option with the AIC

mod1_age <- gamm(age ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                    data = df_rq3a_clean, family = gaussian(link = "identity"))

mod2_age <- gamm(age ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                    data = df_rq3a_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 0))

mod3_age <- gamm(age ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                    data = df_rq3a_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 1))

mod4_age <- gamm(age ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                    data = df_rq3a_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 0))

mod5_age <- gamm(age ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                    data = df_rq3a_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 1))

mod6_age <- gamm(age ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                    data = df_rq3a_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 2))

AIC(mod1_age$lme, mod2_age$lme, mod3_age$lme, mod4_age$lme, mod5_age$lme, mod6_age$lme) # select mod3

# test linear, quadratic and cubic trends for best fitting model and select the best fitting option with the AIC

mod1b_age <- gamm(age ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                     data = df_rq3a_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 1))

mod2b_age <- gamm(age ~ trend + level + slope + I(slope^2) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                     data = df_rq3a_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 1))

mod3b_age <- gamm(age ~ trend + level + slope + I(slope^2) + I(slope^3) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                     data = df_rq3a_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 1))

AIC(mod1b_age$lme, mod2b_age$lme, mod3b_age$lme) # select mod1b

# derive Bs and 95% CIs for best fitting model

new <- summary(mod1b_age$gam)
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

rq3a_age <- ggplot(data = df_rq3a_clean, aes(x = trend, y = age)) +
  geom_line(aes(x = trend, y = age), colour = "black") +
  geom_point(aes(x = trend, y = fitted(mod1b_age$gam)), colour = "red") +
  geom_line(aes(x = trend, y = fitted(mod1b_age$gam)), colour = "red") +
  coord_cartesian(ylim = c(20, 62)) +
  labs(x = "Time", y = "Mean age") +
  scale_x_continuous(breaks = c(9, 39, 70, 100, 131, 162, 192, 223, 253, 284, 315, 344, 375, 405, 436, 466), 
                     labels = c("Apr-19", "May-19", "Jun-19", "Jul-19", "Aug-19", "Sept-19", "Oct-19",
                                "Nov-19", "Dec-19", "Jan-20", "Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20", "Jul-20")) +
  geom_vline(xintercept = 367, linetype = "dotted", color = "blue", size = 1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 17))

if(!file.exists(here("outputs", "rq3a_age.png"))) ggsave(rq3a_age, filename = here("outputs", "rq3a_age.png"), 
                                                            dpi = 320, height = 8, width = 10)

# employment type ------------------------------------------

# check ACF and PACF to identify plausible values for the AR and MA terms

acf(df_rq3a_clean$employment_type, plot = TRUE)
pacf(df_rq3a_clean$employment_type, plot = TRUE)

# test models with different plausible AR and MA terms and select the best fitting option with the AIC

mod1_employment <- gamm(employment_type ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                 data = df_rq3a_clean, family = gaussian(link = "identity"))

mod2_employment <- gamm(employment_type ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                 data = df_rq3a_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 0))

mod3_employment <- gamm(employment_type ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                 data = df_rq3a_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 1))

mod4_employment <- gamm(employment_type ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                 data = df_rq3a_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 0))

mod5_employment <- gamm(employment_type ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                 data = df_rq3a_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 1))

mod6_employment <- gamm(employment_type ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                 data = df_rq3a_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 2))

AIC(mod1_employment$lme, mod2_employment$lme, mod3_employment$lme, mod4_employment$lme, mod5_employment$lme, mod6_employment$lme) # select mod1

# test linear, quadratic and cubic trends for best fitting model and select the best fitting option with the AIC

mod1b_employment <- gamm(employment_type ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                  data = df_rq3a_clean, family = gaussian(link = "identity"))

mod2b_employment <- gamm(employment_type ~ trend + level + slope + I(slope^2) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                  data = df_rq3a_clean, family = gaussian(link = "identity"))

mod3b_employment <- gamm(employment_type ~ trend + level + slope + I(slope^2) + I(slope^3) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                  data = df_rq3a_clean, family = gaussian(link = "identity"))

AIC(mod1b_employment$lme, mod2b_employment$lme, mod3b_employment$lme) # select mod1b

# derive Bs and 95% CIs for best fitting model

new <- summary(mod1b_employment$gam)
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

rq3a_employment <- ggplot(data = df_rq3a_clean, aes(x = trend, y = employment_type)) +
  geom_line(aes(x = trend, y = employment_type), colour = "black") +
  geom_point(aes(x = trend, y = fitted(mod1b_employment$gam)), colour = "red") +
  geom_line(aes(x = trend, y = fitted(mod1b_employment$gam)), colour = "red") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Time", y = "% Employment type") +
  scale_x_continuous(breaks = c(9, 39, 70, 100, 131, 162, 192, 223, 253, 284, 315, 344, 375, 405, 436, 466), 
                     labels = c("Apr-19", "May-19", "Jun-19", "Jul-19", "Aug-19", "Sept-19", "Oct-19",
                                "Nov-19", "Dec-19", "Jan-20", "Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20", "Jul-20")) +
  geom_vline(xintercept = 367, linetype = "dotted", color = "blue", size = 1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 17))

if(!file.exists(here("outputs", "rq3a_employment.png"))) ggsave(rq3a_employment, filename = here("outputs", "rq3a_employment.png"), 
                                                         dpi = 320, height = 8, width = 10)

# AUDIT score ------------------------------------------

# check ACF and PACF to identify plausible values for the AR and MA terms

acf(df_rq3a_clean$AUDIT_score, plot = TRUE)
pacf(df_rq3a_clean$AUDIT_score, plot = TRUE)

# test models with different plausible AR and MA terms and select the best fitting option with the AIC

mod1_AUDIT <- gamm(AUDIT_score ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                        data = df_rq3a_clean, family = gaussian(link = "identity"))

mod2_AUDIT <- gamm(AUDIT_score ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                        data = df_rq3a_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 0))

mod3_AUDIT <- gamm(AUDIT_score ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                        data = df_rq3a_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 1))

mod4_AUDIT <- gamm(AUDIT_score ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                        data = df_rq3a_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 0))

mod5_AUDIT <- gamm(AUDIT_score ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                        data = df_rq3a_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 1))

mod6_AUDIT <- gamm(AUDIT_score ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                        data = df_rq3a_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 2))

AIC(mod1_AUDIT$lme, mod2_AUDIT$lme, mod3_AUDIT$lme, mod4_AUDIT$lme, mod5_AUDIT$lme, mod6_AUDIT$lme) # select mod1

# test linear, quadratic and cubic trends for best fitting model and select the best fitting option with the AIC

mod1b_AUDIT <- gamm(AUDIT_score ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                         data = df_rq3a_clean, family = gaussian(link = "identity"))

mod2b_AUDIT <- gamm(AUDIT_score ~ trend + level + slope + I(slope^2) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                         data = df_rq3a_clean, family = gaussian(link = "identity"))

mod3b_AUDIT <- gamm(AUDIT_score ~ trend + level + slope + I(slope^2) + I(slope^3) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                         data = df_rq3a_clean, family = gaussian(link = "identity"))

AIC(mod1b_AUDIT$lme, mod2b_AUDIT$lme, mod3b_AUDIT$lme) # select mod1b

# derive Bs and 95% CIs for best fitting model

new <- summary(mod1b_AUDIT$gam)
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

rq3a_AUDIT <- ggplot(data = df_rq3a_clean, aes(x = trend, y = AUDIT_score)) +
  geom_line(aes(x = trend, y = AUDIT_score), colour = "black") +
  geom_point(aes(x = trend, y = fitted(mod1b_AUDIT$gam)), colour = "red") +
  geom_line(aes(x = trend, y = fitted(mod1b_AUDIT$gam)), colour = "red") +
  coord_cartesian(ylim = c(8, 30)) +
  labs(x = "Time", y = "Mean AUDIT score") +
  scale_x_continuous(breaks = c(9, 39, 70, 100, 131, 162, 192, 223, 253, 284, 315, 344, 375, 405, 436, 466), 
                     labels = c("Apr-19", "May-19", "Jun-19", "Jul-19", "Aug-19", "Sept-19", "Oct-19",
                                "Nov-19", "Dec-19", "Jan-20", "Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20", "Jul-20")) +
  geom_vline(xintercept = 367, linetype = "dotted", color = "blue", size = 1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 17))

if(!file.exists(here("outputs", "rq3a_AUDIT.png"))) ggsave(rq3a_AUDIT, filename = here("outputs", "rq3a_AUDIT.png"), 
                                                                dpi = 320, height = 8, width = 10)

# at risk of dependence ------------------------------------------

# check ACF and PACF to identify plausible values for the AR and MA terms

acf(df_rq3a_clean$at_risk, plot = TRUE)
pacf(df_rq3a_clean$at_risk, plot = TRUE)

# test models with different plausible AR and MA terms and select the best fitting option with the AIC

mod1_at_risk <- gamm(at_risk ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                   data = df_rq3a_clean, family = gaussian(link = "identity"))

mod2_at_risk <- gamm(at_risk ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                   data = df_rq3a_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 0))

mod3_at_risk <- gamm(at_risk ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                   data = df_rq3a_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 1))

mod4_at_risk <- gamm(at_risk ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                   data = df_rq3a_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 0))

mod5_at_risk <- gamm(at_risk ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                   data = df_rq3a_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 1))

mod6_at_risk <- gamm(at_risk ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                   data = df_rq3a_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 2, q = 2))

AIC(mod1_at_risk$lme, mod2_at_risk$lme, mod3_at_risk$lme, mod4_at_risk$lme, mod5_at_risk$lme, mod6_at_risk$lme) # select mod2

# test linear, quadratic and cubic trends for best fitting model and select the best fitting option with the AIC

mod1b_at_risk <- gamm(at_risk ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                    data = df_rq3a_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 0))

mod2b_at_risk <- gamm(at_risk ~ trend + level + slope + I(slope^2) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                    data = df_rq3a_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 0))

mod3b_at_risk <- gamm(at_risk ~ trend + level + slope + I(slope^2) + I(slope^3) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
                    data = df_rq3a_clean, family = gaussian(link = "identity"), correlation = corARMA(p = 1, q = 0))

AIC(mod1b_at_risk$lme, mod2b_at_risk$lme, mod3b_at_risk$lme) # select mod2b

# derive Bs and 95% CIs for best fitting model

new <- summary(mod2b_at_risk$gam)
coef <- round(new$p.coef[2:5], 4)
se <- new$se[2:5]
lower <- round(coef-1.96*se, 4)
higher <- round(coef+1.96*se, 4)
p <- round(new$p.pv[2:5],4)
coef
lower
higher
p

# plot values

rq3a_at_risk <- ggplot(data = df_rq3a_clean, aes(x = trend, y = at_risk)) +
  geom_line(aes(x = trend, y = at_risk), colour = "black") +
  geom_point(aes(x = trend, y = fitted(mod2b_at_risk$gam)), colour = "red") +
  geom_line(aes(x = trend, y = fitted(mod2b_at_risk$gam)), colour = "red") +
  coord_cartesian(ylim = c(0.35, 1)) +
  labs(x = "Time", y = "% At risk of alcohol dependence") +
  scale_x_continuous(breaks = c(9, 39, 70, 100, 131, 162, 192, 223, 253, 284, 315, 344, 375, 405, 436, 466), 
                     labels = c("Apr-19", "May-19", "Jun-19", "Jul-19", "Aug-19", "Sept-19", "Oct-19",
                                "Nov-19", "Dec-19", "Jan-20", "Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20", "Jul-20")) +
  geom_vline(xintercept = 367, linetype = "dotted", color = "blue", size = 1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 17))

if(!file.exists(here("outputs", "rq3a_at_risk.png"))) ggsave(rq3a_at_risk, filename = here("outputs", "rq3a_at_risk.png"), 
                                                           dpi = 320, height = 8, width = 10)
