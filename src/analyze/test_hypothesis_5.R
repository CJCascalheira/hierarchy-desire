# Dependencies
library(Rmisc)
library(tidyverse)
library(car)
library(psych)
library(lsr)

# Import
forced <- read_csv("data/forced_response.csv")

# Collapse Asian and South Asian, non-theoretically relevant ethnicities
forced <- within(forced, {
  ethnicity <- dplyr::recode(ethnicity, "South Asian" = "Asian",
                             "Middle Eastern" = "Other", "Mixed" = "Other",
                             "Native American" = "Other")
})

# Default theme
theme_set(theme_minimal())

# HYPOTHESIS 5 ------------------------------------------------------------

# Cronbach's alpha
forced %>%
  select(starts_with("si_")) %>%
  alpha()

# Calculate social identity score
si_score <- forced %>%
  select(starts_with("si_")) %>%
  rowSums()

# Add social identity to ethnicity data frame
si_and_ethnicity <- forced %>%
  select(id, ethnicity) %>%
  mutate(si_score = si_score,
         ethnicity = factor(ethnicity))

# Is the design unbalanced? No
si_and_ethnicity %>%
  count(ethnicity)

# One-way between subjects ANOVA
si_aov <- aov(si_score ~ ethnicity, data = si_and_ethnicity)
options(contrasts = c("contr.sum", "contr.poly"))
Anova(si_aov, type = "III")

# Effect size, eta-squared
etaSquared(si_aov, type = 3, anova = TRUE)

# Means and SD by ethnicity
(si_desc <- si_and_ethnicity %>%
  group_by(ethnicity) %>%
  summarize(M = mean(si_score),
            SD = sd(si_score)))

# Precise means
si_desc$M

# Confidence intervals
group.CI(si_score ~ ethnicity, data = si_and_ethnicity, ci = 0.95)

# Pairwise t-tests with Bonferroni correction
pairwise.t.test(si_and_ethnicity$si_score, si_and_ethnicity$ethnicity,
                p.adjust.method = "bonferroni")

# ANOVA ASSUMPTIONS -------------------------------------------------------

# Outliers?
ggplot(si_and_ethnicity, aes(x = ethnicity, y = si_score)) +
  geom_boxplot()

# Normality?
aov_residuals <- residuals(si_aov)
shapiro.test(aov_residuals)
plot(si_aov, 2)

# Homoscedasticity?
leveneTest(si_score ~ ethnicity, data = si_and_ethnicity)
plot(si_aov, 1)

# VISUALIZATION -----------------------------------------------------------

# Construct the APA theme
apa_theme <- theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 12, family = "sans"),
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title = element_text(face = "bold"))

# Visualize the plot
figure_hypothesis_5 <- ggplot(si_and_ethnicity, aes(x = ethnicity, y = si_score)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 55), expand = c(0, 0)) +
  labs(x = "Participant Ethnicity",
       y = "Social Identity Salience") +
  apa_theme

# Save plot
ggsave("data/results/figure_si-scores-vs-ethnicity.png", plot = figure_hypothesis_5,
       width = 6, height = 4)