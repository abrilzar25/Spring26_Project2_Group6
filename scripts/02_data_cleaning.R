# Day 2: Task 1 - Variable Selection & Missing Value Check

library(tidyverse)

data_import_R_01 <- read_csv("data/NHIS _Data_2021.csv")

data_import_R_01_subset <- data_import_R_01 %>%
  select(
    DEMENEV_A, COPDEV_A, DIBEV_A, HYPEV_A, DEPEV_A,
    CANEV_A, AGEP_A, SEX_A, HISPALLP_A, MARSTAT_A,
    EDUCP_A, SMKCIGST_A, PHSTAT_A, LSATIS4R_A, RATCAT_A,
    BMICAT_A, WEIGHTLBTC_A, HEIGHTTC_A
  )

# Remove missing / nonresponse values using variable-specific codebook values
clean_data_R_02 <- data_import_R_01_subset %>%
  filter(
    !DEMENEV_A %in% c(7, 8, 9),
    !COPDEV_A %in% c(7, 8, 9),
    !DIBEV_A %in% c(7, 8, 9),
    !HYPEV_A %in% c(7, 8, 9),
    !DEPEV_A %in% c(7, 8, 9),
    !CANEV_A %in% c(7, 8, 9),
    !AGEP_A %in% c(97, 98, 99),
    !SEX_A %in% c(7, 9),
    !HISPALLP_A %in% c(97, 98, 99),
    !MARSTAT_A %in% c(7, 8, 9),
    !EDUCP_A %in% c(97, 98, 99),
    !SMKCIGST_A %in% c(7, 8, 9),
    !PHSTAT_A %in% c(7, 8, 9),
    !LSATIS4R_A %in% c(7, 8, 9),
    !RATCAT_A %in% c(7, 8, 9),
    !BMICAT_A %in% c(9),
    !WEIGHTLBTC_A %in% c(996, 997, 998, 999),
    !HEIGHTTC_A %in% c(96, 97, 98, 99)
  )

# Check cleaned data
summary(clean_data_R_02)
sum(is.na(clean_data_R_02))

# Day 2: Task 2 - Recoding Variables

clean_data_R_02 <- clean_data_R_02 %>%
  mutate(
    EDUCP_A = case_when(
      EDUCP_A >= 0 & EDUCP_A <= 3 ~ 1,
      EDUCP_A == 4 ~ 2,
      EDUCP_A >= 5 & EDUCP_A <= 7 ~ 3,
      EDUCP_A >= 8 & EDUCP_A <= 10 ~ 4
    ),
    EDUCP_A = factor(
      EDUCP_A,
      levels = c(1, 2, 3, 4),
      labels = c(
        "Less than High School",
        "High School Graduate",
        "Some College Education",
        "College Graduate or Better"
      )
    )
  )

# Verify recoding
summary(clean_data_R_02)
table(clean_data_R_02$EDUCP_A)

write_csv(clean_data_R_02, "data/nhis_clean.csv")







