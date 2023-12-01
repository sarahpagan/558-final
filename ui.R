library(shiny)
library(readr)
library(dplyr)
library(forcats)
library(shinyWidgets)

### Read in Data and re-code Boolean variables as factors
data <- read_csv("CC.csv") |>
    mutate(Smokes = as_factor(Smokes),
           Contraceptives = as_factor(Contraceptives),
           IUD = as_factor(IUD),
           STDs = as_factor(STDs),
           STD_condylomatosis = as_factor(STD_condylomatosis),
           STD_cervical_condylomatosis = as_factor(STD_cervical_condylomatosis),
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
    select(-STD_cervical_condylomatosis,
           -STD_vaginal_condylomatosis,
           -STD_pelvic_inflammatory_disease,
           -STD_AIDS,
           -STD_HepatitisB)


variables <- c("Age", "Partners", "First", "Pregnancies", "Smokes",
               "Smokes_years", "Contraceptives", "Contraceptives_years",
               "IUD", "IUD_years", "STDs", "STDs_number", "STD_condylomatosis",
               "STD_vulvo_perineal_condylomatosis", "STD_syphilis",
               "STD_genital_herpes", "STD_molluscum_contagiosum", "STD_HIV",
               "STD_HPV", "Dx_Cancer", "Dx_CIN", "Dx_HPV") 

fluidPage(
    
    titlePanel("Title"),
    
    tabsetPanel(
        tabPanel("About"),
        tabPanel("Data Exploration"),
        tabPanel("Modeling",
                 
                 tabsetPanel(
                     tabPanel("Model Info"),
                     
                     ### Model fitting
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
                                  numericInput("cvfolds",
                                               "Select Number of CV Folds",
                                                    value = 5,
                                                    min = 2,
                                                    max = 10),
                                  br(),
                                  actionButton("button", "Run Models")),
                              mainPanel(
                                  h3(textOutput("glmt")),
                                  verbatimTextOutput("glm"),
                                  h3(textOutput("rft")),
                                  verbatimTextOutput("rf"),
                                  fluidRow(
                                      column(6,
                                             h3(textOutput("glm_test_title")),
                                             verbatimTextOutput("glm_test"))))),
                                      
                     ### Prediction
                     tabPanel("Prediction",
                              h3("Enter predictor values below to make a prediction"),
                              h4("Note: yes = 1 and no = 0"),
                              br(),
                              fluidRow(
                                  column(3,
                                         numericInput("age", "Age",
                                                      value = 50, min = 1, max = 100),
                                         numericInput("partners", "Number of sexual partners",
                                                      value = 0, min = 0, max = 100),
                                         numericInput("first", "Age of first sexual intercourse",
                                                      value = 0, min = 0, max = 100),
                                         numericInput("preg", "Number of pregnancies",
                                                      value = 0, min = 0, max = 100),
                                         numericInput("smokes", "Smokes",
                                                      value = 0, min = 0, max = 1),
                                         numericInput("smokeyrs", "Years of smoking",
                                                      value = 0, min = 0, max = 100)),
                                  column(3, 
                                         numericInput("cont", "Use of hormonal contraceptives",
                                                      value = 0, min = 0, max = 1),
                                         numericInput("contyrs", "Years of hormonal contraceptive use",
                                                      value = 0, min = 0, max = 100),
                                         numericInput("iud", "Use of IUD",
                                                      value = 0, min = 0, max = 1),
                                         numericInput("iudyrs", "Years with IUD",
                                                      value = 0, min = 0, max = 100),
                                         numericInput("stds", "Any STDs",
                                                      value = 0, min = 0, max = 1),
                                         numericInput("stdsyrs", "Number of STDs",
                                                      value = 0, min = 0, max = 10)),
                                  column(3,
                                         numericInput("c", "Condylomatosis",
                                                      value = 0, min = 0, max = 1),
                                         numericInput("vpc", "Vulvo Perineal Condylomatosis",
                                                      value = 0, min = 0, max = 1),
                                         numericInput("s", "Syphilis",
                                                      value = 0, min = 0, max = 1),
                                         numericInput("gh", "Genital Herpes",
                                                      value = 0, min = 0, max = 1),
                                         numericInput("mc", "Molluscum Contagiosum",
                                                      value = 0, min = 0, max = 1),
                                         numericInput("hiv", "HIV",
                                                      value = 0, min = 0, max = 1)),    
                                  column(3, 
                                         numericInput("hpv", "HPV",
                                                      value = 0, min = 0, max = 1),
                                         numericInput("c", "Ever diagnosed with cancer",
                                                      value = 0, min = 0, max = 1),
                                         numericInput("cin", "Ever diagnosed with CIN",
                                                      value = 0, min = 0, max = 1),
                                         numericInput("dhpv", "Ever diagnosed with HPV",
                                                      value = 0, min = 0, max = 1),
                                         br(),
                                         actionButton("predict", "Make Prediction"))),
                              
                              fluidRow(
                                  column(6,
                                         h3(textOutput("glmpt")),
                                         h5(textOutput("sub")),
                                         verbatimTextOutput("glm_pred")),
                                  column(6,
                                         h3(textOutput("rfpt")),
                                         h5(textOutput("sub2")),
                                         verbatimTextOutput("rf_pred")))
                     )
                 )
        )
    )
)
                                        