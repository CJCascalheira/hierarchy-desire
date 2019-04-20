# Dependencies
library(Rmisc)
library(tidyverse)
library(ez)
library(broom)

# Import
forced <- read_csv("data/forced_response.csv")
forced_long <- read_csv("data/forced_long.csv")
free_long <- read_csv("data/free_long.csv")

# Collapse Asian and South Asian, non-theoretically relevant ethnicities
forced <- within(forced, {
  ethnicity <- recode(ethnicity, "South Asian" = "Asian",
                      "Middle Eastern" = "Other", "Mixed" = "Other",
                      "Native American" = "Other")
})

# Set defaults
theme_set(theme_minimal())
options(scipen = 999)

# HYPOTHESIS 1 ------------------------------------------------------------

# Verify repeated measure dataset
table(forced_long$ethnicity, forced_long$masculinity)
table(free_long$ethnicity, free_long$masculinity)

# Participant ethnicity
ethnic <- forced %>%
  select(id, participant_ethnic = ethnicity)

# Convert to factors, add participant ethnicity
forced_long1 <- forced_long %>%
  left_join(ethnic, by = "id") %>%
  mutate(
    id = factor(id),
    ethnicity = factor(ethnicity),
    masculinity = factor(masculinity),
    participant_ethnic = factor(participant_ethnic)
  )

free_long1 <- free_long %>%
  left_join(ethnic, by = "id") %>%
  mutate(
    id = factor(id),
    ethnicity = factor(ethnicity),
    masculinity = factor(masculinity),
    participant_ethnic = factor(participant_ethnic)
  )

# MIXED ANOVA FORCED ------------------------------------------------------

# Mixed ANOVA for forced-choice
(mixed_forced <- ezANOVA(data = forced_long1,
        wid = id,
        between = .(participant_ethnic),
        within = .(ethnicity, masculinity),
        dv = n,
        type = 3,
        detailed = TRUE,
        return_aov = TRUE))

# Calculate sum of squares total
SS_total <- tidy(mixed_forced$aov)[3:11, ] %>%
  select(term, sumsq) %>%
  filter(term == "Residuals") %>%
  bind_rows(
    mixed_forced$ANOVA[3:8, ] %>%
      select(term = Effect, sumsq = SSn) %>%
      as_tibble()
  ) %>%
  summarize(SS_total = sum(sumsq)) %>%
  pull()

# Eta-square and correlation
(eta_r_fc <- mixed_forced$ANOVA[3:8, ] %>%
  select(term = Effect, sumsq = SSn) %>%
  as_tibble() %>%
  mutate(eta = sumsq / SS_total,
         r = sqrt(eta)))

# MIXED ANOVA FREE --------------------------------------------------------

# Mixed ANOVA for free-response
mixed_free <- ezANOVA(data = free_long1,
        wid = id,
        between = .(participant_ethnic),
        within = .(ethnicity, masculinity),
        dv = n,
        type = 3,
        detailed = TRUE,
        return_aov = TRUE)

# Calculate sum of squares total
SS_total <- tidy(mixed_free$aov)[3:11, ] %>%
  select(term, sumsq) %>%
  filter(term == "Residuals") %>%
  bind_rows(
    mixed_free$ANOVA[3:8, ] %>%
      select(term = Effect, sumsq = SSn) %>%
      as_tibble()
  ) %>%
  summarize(SS_total = sum(sumsq)) %>%
  pull()

# Eta-square and correlation
(eta_r_fr <- mixed_free$ANOVA[3:8, ] %>%
  select(term = Effect, sumsq = SSn) %>%
  as_tibble() %>%
  mutate(eta = sumsq / SS_total,
         r = sqrt(eta)))

# FISHER Z-TO-R -----------------------------------------------------------

# Note, the Fisher z-to-r transformation compared the correlation 
# coefficients for both the forced-choice and free-response values for
# ethnicity, masculinity, and ethnicity:masculinity using a web-hosted
# algorithm by Richard Lowry: http://vassarstats.net/rdiff.html. This 
# algorithm returned Z- and p-values to determine the significance of 
# difference between the two photo selection tasks.

# HYPOTHESIS 2 ------------------------------------------------------------

# Descriptive stats of masculinity
free_long1 %>%
  group_by(masculinity) %>%
  summarize(M = mean(n),
            SD = sd(n))

# Confidence intervals
group.CI(n ~ masculinity, data = free_long1, ci = 0.95)

# Significant?
pairwise.t.test(free_long1$n, free_long1$masculinity, paired = TRUE)

# HYPOTHESIS 3 ------------------------------------------------------------

# Descriptive stats of interaction
free_long1 %>%
  group_by(ethnicity, masculinity) %>%
  summarize(M = mean(n),
            SD = sd(n)) %>%
  arrange(desc(M))

# Confidence intervals
group.CI(n ~ masculinity * ethnicity, data = free_long1, ci = 0.95)

# Significant?
pairwise.t.test(free_long1$n,
                interaction(free_long1$ethnicity, free_long1$masculinity),
                paired = TRUE,
                p.adjust.method = "bonferroni")

# Prepare data to visualize
e <- as.character(unique(free_long1$ethnicity))
m <- as.character(unique(free_long1$masculinity))

# Isolate each level combination
for (i in 1:length(e)) {
  
  for (j in 1:length(m)) {
    
    e_m <- free_long1 %>%
      filter(ethnicity == e[i], masculinity == m[j])
    
    assign(paste0(e[i], "_", m[j]), e_m)
  }
}

# Create list of level combinations
prototypes <- list(asian_em$n, asian_hm$n, black_em$n, black_hm$n,
                   latino_em$n, latino_hm$n, white_em$n, white_hm$n)

names(prototypes) <- c("asian_em", "asian_hm", "black_em", "black_hm",
                       "latino_em", "latino_hm", "white_em", "white_hm")

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

# Create visualization
figure_hypothesis_3 <- map(prototypes, psych::describe) %>%
  bind_rows() %>%
  as_tibble() %>%
  # Select necessary statistics
  select(mean, se) %>%
  mutate(
    ethnicity = c(rep("Asian", 2), rep("Black", 2),
                  rep("Latino", 2), rep("White", 2)),
    masculinity = rep(c("Effeminate", "Heteronormative"), 4)
  ) %>%
  # Render graph
  ggplot(aes(x = ethnicity, y = mean, group = masculinity)) +
    geom_line(aes(linetype = masculinity), size = 0.75) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.07) +
    geom_point(size = 2.5, position = position_dodge(width = 0.01)) +
    geom_point(size = 1.5,
               position = position_dodge(width = 0.01),
               color = "white") +
    expand_limits(y = c(0, 2.5)) +
    scale_y_continuous(breaks = seq(0, 2.5, 0.5), expand = c(0, 0)) +
    guides(linetype = guide_legend("")) +
    labs(x = "Ethnicity", y = "Preference for Prototype") +
    apa_theme +
    theme(legend.position = c(0.16, 0.99))

# Save plot
ggsave("data/results/figure_preference-for-prototype.png",
       plot = figure_hypothesis_3, width = 6, height = 4)

# HYPOTHESIS 4 ------------------------------------------------------------

# Split on between-subjects variable
Asian <- free_long1 %>%
  filter(participant_ethnic == "Asian")

Black <- free_long1 %>%
  filter(participant_ethnic == "Black")

Latino <- free_long1 %>%
  filter(participant_ethnic == "Latino")

White <- free_long1 %>%
  filter(participant_ethnic == "White")

# Pairwise comparisons among Asians
with(Asian, pairwise.t.test(n,
                            ethnicity,
                            paired = TRUE,
                            p.adjust.method = "bonferroni"))

Asian %>%
  group_by(ethnicity) %>%
  summarize(M = mean(n),
            SD = sd(n)) %>%
  arrange(desc(M))

# Confidence intervals
group.CI(n ~ ethnicity, data = Asian, ci = 0.95)

# Pairwise comparisons among Blacks
with(Black, pairwise.t.test(n,
                            ethnicity,
                            paired = TRUE,
                            p.adjust.method = "bonferroni"))

Black %>%
  group_by(ethnicity) %>%
  summarize(M = mean(n),
            SD = sd(n)) %>%
  arrange(desc(M)) 

# Confidence intervals
group.CI(n ~ ethnicity, data = Black, ci = 0.95)

# Pairwise comparisons among Latinos
with(Latino, pairwise.t.test(n,
                            ethnicity,
                            paired = TRUE,
                            p.adjust.method = "bonferroni"))

Latino %>%
  group_by(ethnicity) %>%
  summarize(M = mean(n),
            SD = sd(n)) %>%
  arrange(desc(M))

# Confidence intervals
group.CI(n ~ ethnicity, data = Latino, ci = 0.95)

# Pairwise comparisons among Whites
with(White, pairwise.t.test(n,
                            ethnicity,
                            paired = TRUE,
                            p.adjust.method = "bonferroni"))

White %>%
  group_by(ethnicity) %>%
  summarize(M = mean(n),
            SD = sd(n)) %>%
  arrange(desc(M)) 

# Confidence intervals
group.CI(n ~ ethnicity, data = White, ci = 0.95)

# Total number of participants in each group
forced %>%
  count(ethnicity)
