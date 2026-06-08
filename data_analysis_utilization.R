library(tidyverse)

APTT_2024_stats <- APTT_2024_Clean_All |>
  distinct() |>
  mutate(ID = as.character(ID),
         Cohort = "2024")

APTT_2025_stats <- APTT_2025_Clean |>
  distinct() |>
  mutate(ID = as.character(ID),
         Cohort = "2025")

Assays_both <- bind_rows(APTT_2024_stats, APTT_2025_stats) |>
  mutate(Test = ifelse(Test == "APTT", "APTT", "Anti-Xa")) |>
  filter(
    DT_Drawn >= DT_Hep_Start,
    DT_Drawn <= DT_Hep_Stop
  ) |>
  filter(!is.na(Result))


# unique encounters
Assays_both |>
  distinct(Cohort, ID) |>
  summarise(n = n(), .by = c(Cohort)) |>
  mutate(Percent = round(n/sum(n)*100, 1)) |>
  mutate(Total = sum(n), .before = n)

# number of assays
Assays_both |>
  summarise(n = n(), .by = c(Cohort, ID, Test)) |>
  summarise(n = sum(n), .by = c(Cohort, Test)) |>
  mutate(Percent = round(n/sum(n)*100, 1), .by = c(Cohort)) |>
  mutate(Total_Cohort = sum(n), .before = n, .by = c(Cohort)) |>
  mutate(Total_All = sum(n), .before = Total_Cohort)

chisq.test(Assays_both$Cohort, Assays_both$Test)
sqrt(chisq.test(Assays_both$Cohort, Assays_both$Test)$statistic / sum(table(Assays_both$Cohort, Assays_both$Test)))



Corr_Assays <- Assays_both |>
  arrange(Cohort, ID, Sample_Seq, desc(Test)) |>
  mutate(
    Next_Interval = interval(DT_Complete, lead(DT_Complete, 1))/hours(1),
    lead_1_test = lead(Test, 1), 
    lead_1_Result = lead(Result, 1), 
    .by = c(ID, Cohort)
) |>
  filter(
    !is.na(Next_Interval),
    Test != lead_1_test,
    Next_Interval <= 2,
    Next_Interval >= 0
)

Corr_Assays |>
  distinct(Cohort, ID) |>
  nrow()

Corr_Assays_Spearman <- Corr_Assays |>
  mutate(
    Result = str_replace(Result, ">", ""),
    Result = str_replace(Result, "<", ""),
    Result = as.numeric(Result),
    lead_1_Result = str_replace(lead_1_Result, ">", ""),
    lead_1_Result = str_replace(lead_1_Result, "<", ""),
    lead_1_Result = as.numeric(lead_1_Result),
    APTT_Result = ifelse(Test == "APTT", Result, lead_1_Result),
    AntiXa_Result = ifelse(Test == "Anti-Xa", Result, lead_1_Result),
    APTT_TR = case_when(
        APTT_Result < 45 ~ 1,
        APTT_Result > 65 ~ 3,
        TRUE ~ 2
    ),
    AntiXa_TR = case_when(
        AntiXa_Result < 0.3 ~ 1,
        AntiXa_Result > 0.7 ~ 3,
        TRUE ~ 2
    )
)
cor(Corr_Assays_Spearman$Result, Corr_Assays_Spearman$lead_1_Result, method = "spearman")
cor(Corr_Assays_Spearman$APTT_TR, Corr_Assays_Spearman$AntiXa_TR, method = "spearman")

# all values
Corr_Assays_Spearman |>
  mutate(
  ) |>
  ggplot(aes(x = APTT_Result, y = AntiXa_Result)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

# values within reportable range
Corr_Assays_Spearman |>
  mutate(
    APTT_Result = ifelse(Test == "APTT", Result, lead_1_Result),
    AntiXa_Result = ifelse(Test == "Anti-Xa", Result, lead_1_Result)
  ) |>
#   filter(
#     APTT_Result < 120,
#     AntiXa_Result > 0.04,
#     AntiXa_Result < 1.99
#   ) |>
  ggplot(aes(x = APTT_TR, y = AntiXa_TR)) +
  geom_point(size = ) +
  geom_smooth(method = "lm", se = TRUE)




