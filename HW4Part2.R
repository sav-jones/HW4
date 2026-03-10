install.packages("MASS")
install.packages(c("ggplot2", "tidyverse", "lubridate"))

#Question 4 HW4:
library(MASS)
install.packages("ggplot2")
library(ggplot2)

cars93 <- MASS::Cars93
ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x, color = "#0072B2") +
  scale_x_continuous(
    name = "price (USD)",
    breaks = c(20, 40, 60),
    labels = c("$20,000", "$40,000", "$60,000")
  ) +
  scale_y_continuous(name = "fuel-tank capacity\n(US gallons)")

#Create 3 plots
#LM
ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(method = "lm", se = TRUE, color = "#8fe388") +
  ggtitle("Smoothing with LM") +
  theme(plot.title = element_text(size = 14, color = "#8fe388")) +
  xlab("price (USD)") +
  ylab("fuel-tank capacity (US gallons)")

#GLM
ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(method = "glm", se = TRUE, color = "#fe8d6d") +
  ggtitle("Smoothing with GLM") +
  theme(plot.title = element_text(size = 14, color = "#fe8d6d")) +
  xlab("price (USD)") +
  ylab("fuel-tank capacity (US gallons)")

#GAM
ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(method = "gam", se = TRUE, color = "#7c6bea") +
  ggtitle("Smoothing with GAM") +
  theme(plot.title = element_text(size = 14, color = "#7c6bea")) +
  xlab("price (USD)") +
  ylab("fuel-tank capacity (US gallons)")


#Question 5 HW4
# Load dataset
library(tidyverse)
library(lubridate)

load("./preprint_growth.rda")  
head(preprint_growth)

biorxiv_growth <- preprint_growth %>%
  filter(archive == "bioRxiv") %>%
  filter(count > 0)

preprints <- preprint_growth %>%
  filter(archive %in% c("bioRxiv", "arXiv q-bio", "PeerJ Preprints")) %>%
  filter(count > 0) %>%
  mutate(archive = factor(archive, levels = c("bioRxiv", "arXiv q-bio", "PeerJ Preprints")))

preprints_final <- filter(preprints, date == ymd("2017-01-01"))

ggplot(preprints) +
  aes(date, count, color = archive, fill = archive) +
  geom_line(size = 1) +
  scale_y_continuous(
    limits = c(0, 600), expand = c(0, 0),
    name = "preprints / month",
    sec.axis = dup_axis(
      breaks = preprints_final$count,       # Use counts to position labels
      labels = c("arXivq-bio", "PeerJPreprints", "bioRxiv"),
      name = NULL
    )
  ) +
  scale_x_date(
    name = "year",
    limits = c(min(biorxiv_growth$date), ymd("2017-01-01"))
  ) +
  scale_color_manual(values = c("#0072b2", "#D55E00", "#009e73"), name = NULL) +
  theme(legend.position = "none")

#a)
preprint_full <- preprint_growth %>%
  drop_na() %>%
  filter(count > 0, year(date) > 2004)
#b)
preprints_filtered <- preprint_full %>%
  filter(archive %in% c("bioRxiv", "F1000Research"))
#c-f)
ggplot(preprints_filtered, aes(x = date, y = count, color = archive)) +
geom_line(size = 1) +
  scale_color_manual(values = c("bioRxiv" = "#7c6bea", "F1000Research" = "#fe8d6d")) +
  theme(legend.position = "right") +
  scale_x_date(name = "Date", limits = c(ymd("2014-02-01"), max(preprints_filtered$date))) +
  scale_y_continuous(name = "Preprints / month") +
  ggtitle("Preprint Counts") +
  theme(plot.title = element_text(size = 14, color = "black"))
