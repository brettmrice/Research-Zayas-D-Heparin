library(tidyverse)
# library(psych)

APTT_2024_stats <- APTT_2024_Clean |>
  # select(!c(Test:DT_Complete)) |>
  distinct() |>
  mutate(ID = as.character(ID),
         Cohort = "2024",
         Result = as.character(Result))

APTT_2025_stats <- APTT_2025_Clean |>
  # select(!c(Test:DT_Complete)) |>
  distinct() |>
  mutate(ID = as.character(ID),
         Cohort = "2025",
         Result = as.character(Result))

APTT_both <- bind_rows(APTT_2024_stats, APTT_2025_stats) |>
  filter(!is.na(Test)) |>
  mutate(Test = ifelse(Test == "APTT", "APTT", "Anti-Xa")) |>
  filter(
    DT_Drawn >= DT_Hep_Start,
    DT_Drawn <= DT_Hep_Stop
  ) |>
  filter(!is.na(Result))

# Number of patient encounters
APTT_both |>
  distinct(Cohort, ID) |>
  # all patient encounters
  # summarise(n = n())
  # by cohort
  summarise(n = n(), .by = Cohort)

#  Heparin therapy durations
APTT_both_unique_encounters <- APTT_both |>
  distinct(Cohort, ID, .keep_all = TRUE) 
APTT_both_unique_encounters|>
  # by cohort
  summarise(
    median = median(Hep_Duration), 
    IQR = IQR(Hep_Duration), 
    Q1 = quantile(Hep_Duration, 0.25), 
    Q3 = quantile(Hep_Duration, 0.75),
    .by = Cohort)

# compare durations
mwu_durations <- wilcox.test(Hep_Duration ~ Cohort, data = APTT_both_unique_encounters, exact = FALSE, conf.int = TRUE)
mwu_durations
#  effect size
abs(qnorm(mwu_durations$p.value / 2)) / sqrt(nrow(APTT_both_unique_encounters))

# histogram of durations
# APTT_both_unique_encounters |>
#   mutate(Hep_Duration = ifelse(Hep_Duration > 20, 20, Hep_Duration)) |> # cap durations at 30 days for better visualization
#   ggplot(aes(x = Hep_Duration, fill = Cohort)) +
#   geom_histogram(position = "dodge", col="white") +
#   labs(x = "Heparin Therapy Duration (days)",
#        y = "Count") +
#   theme_minimal() +
#   scale_fill_manual(values = c("2024" = "#A5ACAF", "2025" = "#002f55"))

# column chart of durations
APTT_both_unique_encounters |>
  mutate(Hep_Duration = ifelse(Hep_Duration > 20, 20, Hep_Duration)) |>
  summarise(Count = n(), .by = c(Cohort, Hep_Duration)) |>
  right_join(
    expand_grid(
      Cohort = c("2024", "2025"),
      Hep_Duration = 0:20
    ),
    by = c("Cohort", "Hep_Duration")
  ) |> 
  mutate(Count = replace_na(Count, 0)) |>
  
  ggplot(aes(x = Hep_Duration, y = Count, fill = Cohort)) +
  geom_col(position = position_dodge2(preserve = "single"), just = 0.5) +
  labs(x = "Duration (days)",
       y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("2024" = "#A5ACAF", "2025" = "#002f55")) +
  scale_x_continuous(
    breaks = seq(0, 20, by = 5), 
    minor_breaks = seq(0, 20, by = 5),
    labels = c(seq(0, 19, by = 5), "20+"), 
  )
# save svg as 400x300 for publication

# aptt and anti-xa usage
APTT_both |>
  # all tests
  # summarise(n = n())
  # by cohort
  summarise(n_Test = n(), n_APTT = sum(Test == "APTT"), n_AntiXa = sum(Test != "APTT"), .by = Cohort) |>
  mutate(Percent_APTT = round(n_APTT/sum(n_APTT, n_AntiXa)*100, 1),
         Percent_AntiXa = round(n_AntiXa/sum(n_APTT, n_AntiXa)*100, 1), .by = Cohort) |>
  mutate(Perc_n = round(n_Test/sum(n_Test)*100, 1), .after = n_Test)

#  chi-squared test for OOR proportions
chisq.test(APTT_both$Cohort, APTT_both$Test)
# cramer's V effect size, same formula for phi coefficient when 2x2 table
sqrt(chisq.test(APTT_both$Cohort, APTT_both$Test)$statistic / sum(table(APTT_both$Cohort, APTT_both$Test)))





#  OOR results
APTT_OOR <- APTT_both |>
  filter(Test == 'APTT') |>
  mutate(Result = ifelse(Cohort == "2024" & (Result == '>120' | Result == '120'), 'OOR', as.character(Result)),
         Result = ifelse(Cohort == "2025" & (Result == '>400.0'), 'OOR', as.character(Result))) |>
  mutate(OOR = ifelse(Result == 'OOR', 1, 0))

# total APTT tests
APTT_OOR |>
  # all tests
  summarise(n = n())
  # by cohort
  summarise(n = n(), .by = Cohort)
APTT_OOR |>
  summarise(n_OOR = sum(OOR == 1), n_Total = n(), .by = Cohort) |>
  mutate(Percent_OOR = round(n_OOR/n_Total*100, 1))

#  chi-squared test for OOR proportions
chisq.test(APTT_OOR$Cohort, APTT_OOR$OOR)
fisher.test(APTT_OOR$Cohort, APTT_OOR$OOR)
# cramer's V effect size, same formula for phi coefficient when 2x2 table
sqrt(chisq.test(APTT_OOR$Cohort, APTT_OOR$OOR)$statistic / sum(table(APTT_OOR$Cohort, APTT_OOR$OOR)))
# phi coefficient effect size
# phi(table(APTT_OOR$Cohort, APTT_OOR$OOR))

# cohens h coefficient effect size
# 2*(asin(sqrt(0.122))) - 2*(asin(sqrt(0.046)))

# total from 2025 that were 120 to 400
APTT_OOR |>
  filter(Cohort == "2025") |>
  mutate(OOR_value = as.numeric(Result)) |>
  summarise(n_120_to_400 = sum(OOR_value >= 120 & OOR_value < 400, na.rm = TRUE), n_Total = n()) |>
  mutate(Percent_120_to_400 = round(n_120_to_400/n_Total*100, 1))






# test frequency per patient
APTT_both_freq <- APTT_both |>
  summarise(n_Tests = n(), .by = c(ID, Cohort, Test))

APTT_to_ANTIXA <- APTT_both_freq |>
  pivot_wider(names_from = Test, values_from = n_Tests) |>
  mutate(across(c(APTT, `Anti-Xa`), ~replace_na(., 0))) |>
  mutate(
    Total = APTT + `Anti-Xa`,
    Ratio = round(APTT / `Anti-Xa`, 10) |> as.numeric())
  # filter(Ratio > 0)

APTT_to_ANTIXA |>
  summarise(
    median = median(Total), 
    IQR = IQR(Total), 
    Q1 = quantile(Total, 0.25), 
    Q3 = quantile(Total, 0.75), 
    .by = c(Cohort))

# total assay frequency per patient
mwu_all_frequency <- wilcox.test(Total ~ Cohort, data = APTT_to_ANTIXA, exact = FALSE, conf.int = TRUE)
mwu_all_frequency
#  effect size
abs(qnorm(mwu_all_frequency$p.value / 2)) / sqrt(nrow(APTT_to_ANTIXA))

APTT_to_ANTIXA |>
  summarise(
    median = median(APTT), 
    IQR = IQR(APTT), 
    Q1 = quantile(APTT, 0.25), 
    Q3 = quantile(APTT, 0.75), 
    .by = c(Cohort))

# total APTT frequency per patient
mwu_aptt_frequency <- wilcox.test(APTT ~ Cohort, data = APTT_to_ANTIXA, exact = FALSE, conf.int = TRUE)
mwu_aptt_frequency
#  effect size
abs(qnorm(mwu_aptt_frequency$p.value / 2)) / sqrt(nrow(APTT_to_ANTIXA))


# time interval for 2024 OOR results compared to 2025 120-400
# any assay completed next
Time_to_Next_Any_OOR <- APTT_OOR |>
  arrange(Cohort, ID, Sample_Seq, desc(Test)) |>
  filter(SSeq == 1) |>
  mutate(Next_Interval = interval(DT_Complete, lead(DT_Complete, 1))/hours(1), .by = c(ID, Cohort)) |>
  filter(!is.na(Next_Interval))

# all assays
Time_to_Next <- Time_to_Next_Any_OOR
Time_to_Next |>
  summarise(
    median = median(Next_Interval), 
    IQR = IQR(Next_Interval), 
    Q1 = quantile(Next_Interval, 0.25), 
    Q3 = quantile(Next_Interval, 0.75), 
    .by = c(Cohort))

# total APTT frequency per patient
mwu_next_interval <- wilcox.test(Next_Interval ~ Cohort, data = Time_to_Next, exact = FALSE, conf.int = TRUE)
mwu_next_interval
#  effect size
abs(qnorm(mwu_next_interval$p.value / 2)) / sqrt(nrow(Time_to_Next))


# assays with OOR or within 120 - 400
Time_to_Next <- Time_to_Next_Any_OOR |>
  mutate(OOR_Result = as.numeric(Result)) |>
  filter(
    (Cohort == "2024" & Result == "OOR") |
    (Cohort == "2025" & !is.na(OOR_Result) & OOR_Result >= 120 & OOR_Result < 400)
  ) 
Time_to_Next |>
  summarise(
    median = median(Next_Interval), 
    IQR = IQR(Next_Interval), 
    Q1 = quantile(Next_Interval, 0.25), 
    Q3 = quantile(Next_Interval, 0.75), 
    .by = c(Cohort))

# total APTT frequency per patient
mwu_next_interval <- wilcox.test(Next_Interval ~ Cohort, data = Time_to_Next, exact = FALSE, conf.int = TRUE)
mwu_next_interval
#  effect size
abs(qnorm(mwu_next_interval$p.value / 2)) / sqrt(nrow(Time_to_Next))


# APTT assay completed next
Time_to_Next_APTT <- APTT_both |>
  arrange(Cohort, ID, Sample_Seq) |>
  filter(SSeq == 1, Test == "APTT") |>
  mutate(Next_Interval = interval(DT_Complete, lead(DT_Complete, 1))/hours(1), .by = c(ID, Cohort)) |>
  filter(!is.na(Next_Interval))

Time_to_Next_APTT |>
  summarise(
    median = median(Next_Interval), 
    IQR = IQR(Next_Interval), 
    Q1 = quantile(Next_Interval, 0.25), 
    Q3 = quantile(Next_Interval, 0.75), 
    .by = c(Cohort))

# total APTT frequency per patient
mwu_next_interval <- wilcox.test(Next_Interval ~ Cohort, data = Time_to_Next_APTT, exact = FALSE, conf.int = TRUE)
mwu_next_interval
#  effect size
abs(qnorm(mwu_next_interval$p.value / 2)) / sqrt(nrow(Time_to_Next_APTT))
