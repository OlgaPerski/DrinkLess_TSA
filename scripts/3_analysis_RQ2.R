# load packages and data --------------------------------------------------

source(here::here("scripts", "0_load_packages_and_data.R"))

df_rq2_clean <- read_rds(here::here("data", "df_rq2_clean.rds"))

# RQ2 - number of downloads pre- and post-lockdown ------------------------

# descriptives overall, pre- and post-lockdown

# overall

length(df_rq2_clean$number_downloads)
range(df_rq2_clean$number_downloads)

round(mean(df_rq2_clean$number_downloads), 3)
round(sd(df_rq2_clean$number_downloads), 3)

median(df_rq2_clean$number_downloads)
IQR(df_rq2_clean$number_downloads)

# pre-lockdown

pre_downloads <- df_rq2_clean %>%
  filter(level == 0)

round(mean(pre_downloads$number_downloads), 3)
round(sd(pre_downloads$number_downloads), 3)

median(pre_downloads$number_downloads)
IQR(pre_downloads$number_downloads)

# post-lockdown

post_downloads <- df_rq2_clean %>%
  filter(level == 1)

round(mean(post_downloads$number_downloads), 3)
round(sd(post_downloads$number_downloads), 3)

median(post_downloads$number_downloads)
IQR(post_downloads$number_downloads)

# check ACF and PACF to identify plausible values for the AR and MA terms

acf(df_rq2_clean$number_downloads, plot = TRUE)
pacf(df_rq2_clean$number_downloads, plot = TRUE)

# test models with different plausible AR and MA terms and select the best fitting option with the AIC

mod1_downloads <- gamm(number_downloads ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
             data = df_rq2_clean, family = nb)

mod2_downloads <- gamm(number_downloads ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
             data = df_rq2_clean, family = nb, correlation = corARMA(p = 1, q = 0))

mod3_downloads <- gamm(number_downloads ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
             data = df_rq2_clean, family = nb, correlation = corARMA(p = 1, q = 1)) # did not converge

mod4_downloads <- gamm(number_downloads ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
             data = df_rq2_clean, family = nb, correlation = corARMA(p = 2, q = 0))

mod5_downloads <- gamm(number_downloads ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
             data = df_rq2_clean, family = nb, correlation = corARMA(p = 2, q = 1)) # did not converge

mod6_downloads <- gamm(number_downloads ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
             data = df_rq2_clean, family = nb, correlation = corARMA(p = 2, q = 2)) # did not converge

AIC(mod1_downloads$lme, mod2_downloads$lme, mod4_downloads$lme) # select mod1

# test linear, quadratic and cubic trends for best fitting model and select the best fitting option with the AIC

mod1b_downloads <- gamm(number_downloads ~ trend + level + slope + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
              data = df_rq2_clean, family = nb)

mod2b_downloads <- gamm(number_downloads ~ trend + level + slope + I(slope^2) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
              data = df_rq2_clean, family = nb)

mod3b_downloads <- gamm(number_downloads ~ trend + level + slope + I(slope^2) + I(slope^3) + s(month, bs="cc", k=12) + s(day, bs="cc", k=7), 
              data = df_rq2_clean, family = nb)

AIC(mod1b_downloads$lme, mod2b_downloads$lme, mod3b_downloads$lme) # select mod3b

# derive IRRs and 95% CIs for best fitting model

new <- summary(mod3b_downloads$gam)
coef <- new$p.coef[2:6]
a <- round(exp(coef),4)
se <- new$se[2:6]
lower <- coef-1.96*se
higher <- coef+1.96*se
b <- round(exp(lower),4)
c <- round(exp(higher),4)
p <- round(new$p.pv[2:6],3)
a
b
c
p

# plot values

rq2_plot <- ggplot(data = df_rq2_clean, aes(x = trend, y = number_downloads)) +
  geom_line(aes(x = trend, y = number_downloads), colour = "black") +
  geom_point(aes(x = trend, y = fitted(mod3b_downloads$gam)), colour = "red") +
  geom_line(aes(x = trend, y = fitted(mod3b_downloads$gam)), colour = "red") +
  coord_cartesian(ylim = c(0, 300)) +
  labs(x = "Time", y = "Number of downloads") +
  scale_x_continuous(breaks = c(9, 39, 70, 100, 131, 162, 192, 223, 253, 284, 315, 344, 375, 405, 436, 466), 
                     labels = c("Apr-19", "May-19", "Jun-19", "Jul-19", "Aug-19", "Sept-19", "Oct-19",
                                "Nov-19", "Dec-19", "Jan-20", "Feb-20", "Mar-20", "Apr-20", "May-20", "Jun-20", "Jul-20")) +
  geom_vline(xintercept = 367, linetype = "dotted", color = "blue", size = 1) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 17))

if(!file.exists(here("outputs", "rq2_plot.png"))) ggsave(rq2_plot, filename = here("outputs", "rq2_plot.png"), 
                                                         dpi = 320, height = 8, width = 10)
