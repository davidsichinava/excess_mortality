library(tidyverse)
library(mgcv)
library(lubridate)
library(extrafont)

extrafont::loadfonts(device="win")

# Mortality data: https://github.com/akarlinsky/world_mortality

weekly_data <- read_csv("https://raw.githubusercontent.com/akarlinsky/world_mortality/main/world_mortality.csv")%>%
  filter(country_name == "Georgia")

train <- weekly_data[weekly_data$year < 2020, ] %>%
  mutate(period = as.numeric(row.names(.)))

test <- weekly_data[weekly_data$year == 2020, ] %>%
  mutate(period = as.numeric(row.names(.)))

# Adjust for seasonality as in Weinberger et al, 2020 and predict potential number of deaths
# https://github.com/weinbergerlab/excess_pi_covid
# https://github.com/weinbergerlab/excess_pi_covid/blob/master/us_pi_excess.Rmd

## Predict deaths using nonparametric GAM model (poisson)

pred_ed_mod <- gam(deaths~s(time, bs='cc')+year, data=train, family='poisson')

pred_ed <- predict(pred_ed_mod, type='response', newdata=test) %>%
  data.frame()%>%
  setNames("deaths_pred")%>%
  mutate(time=as.numeric(row.names(.)),
         year=2020,
         deaths_pred=round(deaths_pred, 0))

#  Edit this if you want to build charts with Georgian labels (only UNIX* systems)
# Sys.setlocale(category = "LC_ALL", locale = "Georgian")

weekly_data %>%
  filter(year==2020)%>%
  # mutate(weeks=paste0(year, stringi::stri_pad_left(time, 2, 0)))%>%

  left_join(pred_ed, by=c("year", "time"))%>%
  mutate(period=as.numeric(row.names(.)),
         dif = deaths-deaths_pred,
         dif_color = ifelse(dif>=0, 1, 2),
         week_dates=as.Date(paste0(time, "/1/", year), "%m/%d/%Y"),
         which_is_more = ifelse(deaths>deaths_pred, deaths, deaths_pred),
         location = which_is_more-abs(dif/2),
         # week_dates=as.Date("2020-01-01")+months(time),
         dif_color = factor(dif_color, levels=c(1, 2), labels=c("პროგნოზირებული", "ემპირიული")),
         )%>%
  ggplot()+
  geom_segment(aes(y=deaths, yend=deaths_pred, x=week_dates, xend=week_dates, color=factor(dif_color)),
               size=2, alpha=0.5)+
  scale_color_manual(values=c("red", "blue"))+
  geom_point(aes(week_dates, deaths), color="red", size=2)+
  geom_point(aes(week_dates, deaths_pred), color="blue", size=2)+
  geom_text(aes(week_dates, location, label=dif, family = "FiraGO"), nudge_x = 10)+
  geom_vline(xintercept=as.Date("2020-09-01"),
             color="grey", linetype = "longdash")+
  annotate("rect", xmin=as.Date("2020-03-31"), xmax=as.Date("2020-05-23"), ymin=-Inf, ymax=Inf,
           alpha=.3, fill="lightblue", family = "FiraGO")+
  annotate("text", x=as.Date("2020-04-27"), y=600, label="საგანგებო მდგომარეობა", family = "FiraGO")+
  annotate("text", x=as.Date("2020-09-01"), y=600, label="სექტემბერი", family = "FiraGO")+
  scale_x_date(date_labels = "%m")+
  # ylim(0, 1200)+
  theme_bw()+
  labs(
    title = "ჭარბი სიკვდილიანობა, 2020 წლის იანვარი-დეკემბერი",
    y = "სიკვდილიანობა",
       x = "თვეები")+
  theme(
    text = element_text(family= "FiraGO"),
    legend.position = "none",
    axis.title.x = element_blank()
  )

ggsave("ka_excess_deaths.png", device = "png", height=5, width=8)

weekly_data %>%
  filter(year==2020)%>%
  # mutate(weeks=paste0(year, stringi::stri_pad_left(time, 2, 0)))%>%
  
  left_join(pred_ed, by=c("year", "time"))%>%
  mutate(period=as.numeric(row.names(.)),
         dif = deaths-deaths_pred,
         dif_color = ifelse(dif>=0, 1, 2),
         week_dates=as.Date(paste0(time, "/1/", year), "%m/%d/%Y"),
         which_is_more = ifelse(deaths>deaths_pred, deaths, deaths_pred),
         location = which_is_more-abs(dif/2),
         # week_dates=as.Date("2020-01-01")+months(time),
         dif_color = factor(dif_color, levels=c(1, 2), labels=c("Predicted", "Empirical")),
  )%>%
  ggplot()+
  geom_segment(aes(y=deaths, yend=deaths_pred, x=week_dates, xend=week_dates, color=factor(dif_color)),
               size=2, alpha=0.5)+
  scale_color_manual(values=c("red", "blue"))+
  geom_point(aes(week_dates, deaths), color="red", size=2)+
  geom_point(aes(week_dates, deaths_pred), color="blue", size=2)+
  geom_text(aes(week_dates, location, label=dif, family = "FiraGO"), nudge_x = 10)+
  geom_vline(xintercept=as.Date("2020-09-01"),
             color="grey", linetype = "longdash")+
  annotate("rect", xmin=as.Date("2020-03-31"), xmax=as.Date("2020-05-23"), ymin=-Inf, ymax=Inf,
           alpha=.3, fill="lightblue", family = "FiraGO")+
  annotate("text", x=as.Date("2020-04-27"), y=600, label="Lockdown", family = "FiraGO")+
  annotate("text", x=as.Date("2020-09-01"), y=600, label="September", family = "FiraGO")+
  scale_x_date(date_labels = "%m")+
  # ylim(0, 1200)+
  theme_bw()+
  labs(
    title = "Excess mortality, January-December, 2020",
    y = "Deaths",
    x = "Months")+
  theme(
    text = element_text(family= "FiraGO"),
    legend.position = "none",
    axis.title.x = element_blank()
  )

ggsave("en_excess_deaths.png", device = "png", height=5, width=8)

sum(pred_ed$deaths_pred) - sum(weekly_data$deaths[weekly_data$year == 2020])

weekly_data %>%
  filter(time >= 10)%>%
  group_by(year)%>%
  summarize(sum(deaths))

pred_ed %>%
  filter(time >= 10)%>%
  summarize(sum(deaths_pred))

