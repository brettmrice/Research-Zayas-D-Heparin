library(tidyverse)

aptt_24_raw <- read_delim('data/data_in_use/APTT_2024.txt',
                      delim = '\t',
             show_col_types = FALSE)
aptt_24 <- aptt_24_raw|>
  transmute(ID = `Alias - Person MRN`,
            DOB = `Date/Time - Birth`,
            Gender = Gender,
            Race = Race,
            Test = `Discrete Assay`,
            Result = `Result - Numeric`,
            Result_Comment = `Comment - Result`,
            DT_Drawn = `Date/Time - Drawn`,
            DT_InLab = `Date/Time - In Lab`,
            DT_Verify = `Date/Time - Verified`,
            DT_Complete = `Date/Time - Complete`) |>
  mutate(Result_Comment = ifelse(!is.na(mdy_hms(lead(Race, 1))), 
                                 paste0(Result_Comment, ' ', lead(ID, 1)),
                                 Result_Comment),
         DT_Drawn = ifelse(!is.na(mdy_hms(lead(Race, 1))), 
                           lead(DOB, 1),
                           DT_Drawn),
         DT_InLab = ifelse(!is.na(mdy_hms(lead(Race, 1))), 
                           lead(DOB, 1),
                           DT_InLab),
         DT_Verify = ifelse(!is.na(mdy_hms(lead(Race, 1))), 
                           lead(Gender, 1),
                           DT_Verify),
         DT_Complete = ifelse(!is.na(mdy_hms(lead(Race, 1))), 
                              lead(Race, 1),
                              DT_Complete)) |>
  filter(Test != '\r',
         DT_Drawn != 1) |>
  mutate(across(contains('DT_'), mdy_hms)) |>
  mutate(DOB = mdy_hms(DOB) |> as_date(),
         ID = as.numeric(ID),
         Result_Comment = ifelse(grepl('fail', Result_Comment, ignore.case = TRUE),
                                 Result_Comment,
                                 NA_character_)) |>
  select(!c(Gender, Race)) |>
  filter(month(DT_InLab) >= 2) |>
  mutate(
    Result = as.character(Result),
    Result = case_when(
      (Test == 'APTT' & (Result == '0' | Result == '120.00')) ~ '>120',
      TRUE ~ Result
    )
  )

aptt_24_hepprot <- read_delim('data/data_in_use/Heparin_Protocol_2024.txt', 
                              delim = '\t',
                              show_col_types = FALSE) |>
  ungroup() |>
  transmute(ID = `Person - Medical Record Number`,
            DT_Admit = `Admit Dt/Tm`,
            DT_Dischage = `Discharge DT/TM`,
            Hep_Protocol = `Path Description`,
            DT_Hep_Start = `Order Current Start Dt/Tm`,
            DT_Hep_ProjStop = `Order Projected Stop DT/TM`,
            DT_Hep_Complete = `Order Complete Dt/Tm`,
            DT_Hep_Discontinue = `Order Discontinue Dt/Tm`) |>
  mutate(across(contains('DT_'), mdy_hms)) |>
  summarise(Hep_Protocol = glue::glue_collapse(Hep_Protocol, sep = '; '),
            DT_Hep_Start = as_date(min(DT_Hep_Start, na.rm = TRUE)),
            DT_Hep_Stop = max(DT_Hep_ProjStop, DT_Hep_Discontinue, na.rm = TRUE) |> as_date(),
            .by = c(ID, DT_Admit, DT_Dischage))

APTT_2024 <- aptt_24_hepprot |>
  left_join(aptt_24, by = join_by(ID), relationship = "many-to-many") |>
  filter(!is.na(DOB)) |>
  filter(DT_InLab >= DT_Admit &
           DT_InLab <= DT_Dischage) |>
  distinct() |>
  mutate(Age = round(interval(DOB, DT_Admit)/years(1)),
         Hep_Duration = round(interval(DT_Hep_Start, DT_Hep_Stop)/days(1), 1),
         LOS_Days_Admit_to_Discharge = round(interval(DT_Admit, DT_Dischage)/days(1), 1),
         LOS_Days_HepStart_to_Discharge = round(interval(DT_Hep_Start, DT_Dischage)/days(1), 1)) |>
  filter(Age >= 18,
         as_date(DT_InLab) >= DT_Hep_Start,
         as_date(DT_InLab) <= DT_Hep_Stop,
         Hep_Duration >= 1) |>
  select(!c(DOB, DT_Verify, Age)) |>
  group_by(ID, DT_Admit) |>
  arrange(DT_Admit, DT_Hep_Start, DT_Complete) |>
  mutate(Sample_Seq = row_number()) |>
  ungroup() |>
  arrange(DT_Admit, ID) |>
  relocate(Sample_Seq, .before = Result)
APTT_2024_hid <- APTT_2024 |> 
  summarise(N = n(), .by = c(ID, DT_Admit)) |> 
  filter(N > 1) |>
  select(!N) |>
  mutate(Psuedo_ID = row_number(), .before = DT_Admit) |>
  left_join(APTT_2024, by = join_by(ID, DT_Admit)) |>
  filter(month(DT_Hep_Start) >= 2)
# write_delim(APTT_2024_hid, delim = '\t', file = 'APTT_2024.txt') 

# filter those encounters where APTT was used
APTT_2024_w_APTT <- APTT_2024_hid |>
  filter(Test == 'APTT') |>
  distinct(ID, DT_Admit) |>
  left_join(APTT_2024_hid, by = join_by(ID, DT_Admit))

# correct sequence of samples for patients with multiple samples with same In Lab datetime
APTT_2024_Clean <- APTT_2024_w_APTT |>
  mutate(
    SSeq = row_number(), 
    .by = c(ID, DT_Admit, DT_InLab)) |>
  filter(SSeq <= 2) |>
  mutate(
    SSeq2 = row_number(), 
    .by = c(ID, DT_Admit, SSeq)) |>
  mutate(
    SSeq3 = ifelse(SSeq == 1, SSeq2, lag(SSeq2, 1)),
  )

  
  


aptt_25_raw <- NULL
for(f in list.files('data/data_in_use/', pattern = 'APTT_ANTXA', full.names = TRUE)) {
  aptt_25_raw <- bind_rows(aptt_25_raw, readxl::read_xlsx(f, guess_max = Inf))
}
aptt_25 <- aptt_25_raw |>
  transmute(ID = str_split_i(`Patient/MRN`, ' \\(', 2) |> 
              str_replace_all(c('\\)' = '')),
            DOB = `DOB`,
            Test = `Component`,
            Instrument_ID = `Instrument ID`,
            Result = `Value`,
            Result_Comment = '',
            DT_Drawn = `Draw Session Instant`,
            DT_InLab = `First Receive Instant`,
            DT_Verify = `First Verified Instant`,
            DT_Complete = `Last Verified Instant`) |>
  rowwise() |>
  mutate(Test = ifelse(grepl('\r\n', Test), 
                       toupper(str_split_i(Test, '\r\n', 3)),
                       toupper(Test)),
         Result = ifelse(grepl('\r\n', Result), 
                         str_split_i(Result, '\r\n', 3),
                         Result),
         Result = str_split_i(Result, ' ', 1)) |>
  ungroup() |>
  select(!c(DT_Verify)) |>
  filter(month(DT_InLab) >= 2) |>
  mutate(Result = replace_na(Result, '>400.0'))

aptt_25_hepprot_raw <- readxl::read_xlsx('data/data_in_use/Heparin_Protocol_2025.xlsx', sheet = 1)
aptt_25_hepprot <- aptt_25_hepprot_raw |>
  transmute(ID = as.character(`MRN`),
            DT_Admit = HOSP_ADMSN_TIME,
            DT_Dischage = HOSP_DISCH_TIME,
            Hep_Protocol = DISPLAY_NAME...10,
            DT_Hep_Order = `ORDER_DATE`,
            DT_Hep_Start = `START_DATE`,
            DT_Hep_Complete = `END_DATE`) |>
  filter(grepl('infusion', Hep_Protocol),
         !grepl('thymoglobulin', Hep_Protocol),
         if_all(contains('DT_'), ~ !is.na(.))) |>
  summarise(Hep_Protocol = glue::glue_collapse(Hep_Protocol, sep = '; '),
            DT_Hep_Start = min(DT_Hep_Order, DT_Hep_Start, na.rm = TRUE),
            DT_Hep_Stop = min(DT_Hep_Complete, na.rm = TRUE),
            .by = c(ID, DT_Admit, DT_Dischage))
  
APTT_2025 <- aptt_25_hepprot |>
  left_join(aptt_25, by = join_by(ID), relationship = "many-to-many") |>
  filter(!is.na(DOB)) |>
  filter(DT_InLab >= DT_Admit &
           DT_InLab <= DT_Dischage) |>
  distinct() |>
  mutate(Age = round(interval(DOB, DT_Admit)/years(1)),
         Hep_Duration = round(interval(DT_Hep_Start, DT_Hep_Stop)/days(1), 1),
         LOS_Days_Admit_to_Discharge = round(interval(DT_Admit, DT_Dischage)/days(1), 1),
         LOS_Days_HepStart_to_Discharge = round(interval(DT_Hep_Start, DT_Dischage)/days(1), 1)) |>
  filter(Age >= 18,
         as_date(DT_InLab) >= DT_Hep_Start,
         # as_date(DT_InLab) <= DT_Hep_Stop,
         Hep_Duration >= 1) |>
  select(!c(DOB, Age)) |>
  group_by(ID, DT_Admit) |>
  arrange(DT_Admit, DT_Hep_Start, DT_Complete) |>
  mutate(Sample_Seq = row_number()) |>
  ungroup() |>
  arrange(DT_Admit, ID) |>
  relocate(Sample_Seq, .before = Result)
APTT_2025_hid <- APTT_2025 |>
  summarise(N = n(), .by = c(ID, DT_Admit)) |> 
  filter(N > 1) |>
  select(!N) |>
  mutate(Psuedo_ID = row_number(), .before = DT_Admit) |>
  left_join(APTT_2025, by = join_by(ID, DT_Admit)) |>
  filter(month(DT_Hep_Start) >= 2)
# write_delim(APTT_2025_hid, delim = '\t', file = 'APTT_2025.txt')

# filter those encounters where APTT was used
APTT_2025_w_APTT <- APTT_2025_hid |>
  filter(Test == 'APTT') |>
  distinct(ID, DT_Admit) |>
  left_join(APTT_2025_hid, by = join_by(ID, DT_Admit))

# correct sequence of samples for patients with multiple samples with same In Lab datetime
APTT_2025_Clean <- APTT_2025_w_APTT |>
  mutate(
    SSeq = row_number(), 
    .by = c(ID, DT_Admit, DT_InLab)) |>
  filter(SSeq <= 2) |>
  mutate(
    SSeq2 = row_number(), 
    .by = c(ID, DT_Admit, SSeq)) |>
  mutate(
    SSeq3 = ifelse(SSeq == 1, SSeq2, lag(SSeq2, 1)),
  )
