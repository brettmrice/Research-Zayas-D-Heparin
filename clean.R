library(tidyverse)

hep_protocol_raw <- readxl::read_xlsx('data/2024.xlsx', sheet = 1)
hep_protocol_clean <- hep_protocol_raw |>
  transmute(ID = `Medical Record Number`,
            Admit_DT = ymd_hms(`Admit Date & Time`),
            Discharge_DT = ymd_hms(`Discharge Date & Time`),
            LOS_days = round(interval(Admit_DT, Discharge_DT)/days(1), 1),
            Hep_Start_DT = ymd_hms(`Heparin Start Date/Time`),
            Hep_End_DT = ymd_hms(`Heparin End Date & Time`),
            Hep_hours = round(interval(Hep_Start_DT, Hep_End_DT)/hours(1), 1))

aptt_raw <- readxl::read_xlsx('data/2024.xlsx', sheet = 2)
aptt_clean <- aptt_raw |>
  transmute(ID = `Medical Record Number`,
            Test = `Test Name`,
            Result = Result,
            Result_DT = ymd_hms(`Resulted Date/Time`))

antiXa_raw <- readxl::read_xlsx('data/2024.xlsx', sheet = 3)
antiXa_clean <- antiXa_raw |>
  transmute(ID = `Medical Record Number`,
            Test = `Test Name`,
            Result = Result,
            Result_DT = ymd_hms(`Resulted Date/Time`))

hep_protocol_24_aptt <- hep_protocol_clean |>
  left_join(aptt_clean, by = "ID", relationship = "many-to-many")
hp24aptt <- hep_protocol_24_aptt |>
  group_by(ID, Admit_DT) |>
  arrange(Admit_DT, Hep_Start_DT, Result_DT) |>
  mutate(RN = row_number(),
         Valid = case_when(Result_DT >= Hep_Start_DT &
                             Result_DT <= Hep_End_DT ~ 1,
                           TRUE ~ 0)) |>
  ungroup() |>
  filter(Hep_hours >= 24)

hep_protocol_24_antiXa <- hep_protocol_clean |>
  left_join(antiXa_clean, by = "ID", relationship = "many-to-many")
hp24antiXa <- hep_protocol_24_antiXa |>
  group_by(ID, Admit_DT) |>
  arrange(Admit_DT, Hep_Start_DT, Result_DT) |>
  mutate(RN = row_number(),
         Valid = case_when(Result_DT >= Hep_Start_DT &
                             Result_DT <= Hep_End_DT ~ 1,
                           TRUE ~ 0)) |>
  ungroup() |>
  filter(Hep_hours >= 24)

hep_protocol_data <- hp24aptt |>
  bind_rows(hp24antiXa) |>
  group_by(ID) |>
  arrange(Admit_DT, Hep_Start_DT, Result_DT) |>
  filter(!is.na(Test)) |>
  mutate(RN = row_number()) |>
  ungroup() |>
  rename("Test_Seq" = "RN")

write_csv(hep_protocol_data,
          quote = 'none',
          file = 'heparin_2024.csv')
