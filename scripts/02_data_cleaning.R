# Day 2: Task 1 - Variable Selection & Missing Value Check
  
library(tidyverse)

NHIS_Data_2021 <- read_csv("data/NHIS _Data_2021.csv")

# Select only variables asked for
data_import_R_02_subset <- NHIS_Data_2021 |>
  select(
    AGEP_A,
    WEIGHTLBTC_A,
    HEIGHTTC_A,
    SEX_A,
    HISPALLP_A,
    EDUCP_A,
    PHSTAT_A,
    LSATIS4R_A
  )
View(data_import_R_02_subset)

# Remove missing / nonresponse values using variable-specific codebook values
clean_data_R_02 <- data_import_R_02_subset |>
  filter(
    !AGEP_A %in% c(97, 98, 99),
    !SEX_A %in% c(7, 9),
    !HISPALLP_A %in% c(97, 98, 99),
    !EDUCP_A %in% c(97, 98, 99),
    !PHSTAT_A %in% c(7, 8, 9),
    !LSATIS4R_A %in% c(7, 8, 9),
    !WEIGHTLBTC_A %in% c(996, 997, 998, 999),
    !HEIGHTTC_A %in% c(96, 97, 98, 99)
  ) 
View(clean_data_R_02) # check

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







