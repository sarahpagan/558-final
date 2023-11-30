library(readr)
library(dplyr)
library(tidyr)
library(forcats)
library(performanceEstimation)

CC <- read_csv("risk_factors_cervical_cancer.csv", na = c("?")) |>
  rename(Partners = `Number of sexual partners`,
         First = `First sexual intercourse`,
         Pregnancies = `Num of pregnancies`,
         Smokes_years = `Smokes (years)`,
         Contraceptives = `Hormonal Contraceptives`,
         Contraceptives_years = `Hormonal Contraceptives (years)`,
         IUD_years = `IUD (years)`,
         STDs_number = `STDs (number)`,
         STD_condylomatosis = `STDs:condylomatosis`,
         STD_cervical_condlomatosis = `STDs:cervical condylomatosis`,
         STD_vaginal_condylomatosis = `STDs:vaginal condylomatosis`,
         STD_vulvo_perineal_condylomatosis = `STDs:vulvo-perineal condylomatosis`,
         STD_syphilis = `STDs:syphilis`,
         STD_pelvic_inflammatory_disease = `STDs:pelvic inflammatory disease`,
         STD_genital_herpes = `STDs:genital herpes`,
         STD_molluscum_contagiosum = `STDs:molluscum contagiosum`,
         STD_AIDS = `STDs:AIDS`,
         STD_HIV = `STDs:HIV`,
         STD_HepatitisB = `STDs:Hepatitis B`,
         STD_HPV = `STDs:HPV`,
         Dx_Cancer = `Dx:Cancer`,
         Dx_CIN = `Dx:CIN`,
         Dx_HPV = `Dx:HPV`) |>
  select(-`Smokes (packs/year)`,
         -`STDs: Number of diagnosis`,
         -`STDs: Time since first diagnosis`,
         -`STDs: Time since last diagnosis`,
         - Dx) |>
  ### Create indicator variable for cervical cancer
  mutate(Cancer = case_when(Hinselmann == 1 |
                              Schiller == 1 |
                              Citology == 1 |
                              Biopsy == 1 ~ 1, .default = 0),
         .keep = "unused") |>
  drop_na() |>
  ### Re-code Boolean variables as factors
  mutate(Smokes = as_factor(Smokes),
         IUD = as_factor(IUD),
         STDs = as_factor(STDs),
         STD_condylomatosis = as_factor(STD_condylomatosis),
         STD_cervical_condlomatosis = as_factor(STD_cervical_condlomatosis),
         STD_vaginal_condylomatosis = as_factor(STD_vaginal_condylomatosis),
         STD_vulvo_perineal_condylomatosis = as_factor(STD_vulvo_perineal_condylomatosis),
         STD_syphilis = as_factor(STD_syphilis),
         STD_pelvic_inflammatory_disease = as_factor(STD_pelvic_inflammatory_disease),
         STD_genital_herpes = as_factor(STD_genital_herpes),
         STD_molluscum_contagiosum = as_factor(STD_molluscum_contagiosum),
         STD_AIDS = as_factor(STD_AIDS),
         STD_HIV = as_factor(STD_HIV),
         STD_HepatitisB = as_factor(STD_HepatitisB),
         STD_HPV = as_factor(STD_HPV),
         Dx_Cancer = as_factor(Dx_Cancer),
         Dx_CIN = as_factor(Dx_CIN),
         Dx_HPV = as_factor(Dx_HPV),
         Cancer = as_factor(Cancer))

### Use SMOTE to create a more balanced data set
new_data <- smote(Cancer ~ ., CC, perc.over = 2, perc.under = 2)
table(new_data$Cancer)

write_csv(new_data, "CC.csv")

x <- c(1, 5)



