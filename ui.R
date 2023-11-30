library(shiny)
library(readr)
library(dplyr)
library(forcats)
library(shinyWidgets)

### Read in Data and re-code Boolean variables as factors
data <- read_csv("CC.csv") |>
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
           Cancer = as_factor(Cancer)) |>
    ### Drop factors with only one level (no observations)
    select(-STD_cervical_condlomatosis,
           -STD_pelvic_inflammatory_disease,
           -STD_molluscum_contagiosum,
           -STD_AIDS,
           -STD_HepatitisB)


variables <- c("Age", "Partners", "First", "Pregnancies", "Smokes",
               "Smokes_years", "Contraceptives", "Contraceptives_years",
               "IUD", "IUD_years", "STDs", "STDs_number", "STD_condylomatosis",
               "STD_vaginal_condylomatosis", "STD_vulvo_perineal_condylomatosis",
               "STD_syphilis", "STD_genital_herpes", "STD_HIV", "STD_HPV",
               "Dx_Cancer", "Dx_CIN", "Dx_HPV") 

fluidPage(
    
    titlePanel("Title"),
    
    tabsetPanel(
        tabPanel("About"),
        tabPanel("Data Exploration"),
        tabPanel("Modeling",
                 
                 tabsetPanel(
                     tabPanel("Model Info"),
                     tabPanel("Model Fitting",
                              sidebarPanel(
                                  sliderInput("slider",
                                              "Choose Data Partition",
                                              min = 10,
                                              max = 100,
                                              value = 75),
                                  textOutput("split"),
                                  br(),
                                  h4("Logistic Regression"),
                                  multiInput("multi",
                                             "Select Variables",
                                             variables,
                                             options = list(
                                                 enable_search = FALSE)),
                                  br(),
                                  h4("Random Forest"),
                                  numericRangeInput("range",
                                                    "Select Range of Variables to Sample",
                                                    value = c(1, 5),
                                                    min = 1,
                                                    max = 10),
                                  actionButton("button", "Run Models")),
                              mainPanel(
                                  h2("Logistic Regression Results"),
                                  verbatimTextOutput("glm"),
                                  h2("Random Forest Results"),
                                  verbatimTextOutput("rf")
                              )),
                     tabPanel("Prediction")
                 )
        )
    )
)