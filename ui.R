library(shiny)
library(shinyWidgets)

######## Defining objects

variables <- c("Age", "Partners", "First", "Pregnancies", "Smokes",
               "Smokes_years", "Contraceptives", "Contraceptives_years",
               "IUD", "IUD_years", "STDs", "STDs_number", "STD_condylomatosis",
               "STD_vulvo_perineal_condylomatosis", "STD_syphilis",
               "STD_genital_herpes", "STD_molluscum_contagiosum", "STD_HIV",
               "STD_HPV", "Dx_Cancer", "Dx_CIN", "Dx_HPV")

numeric_names  <- c("Age", "Number of Sexual Partners", "Number of Pregnancies",
                    "Age of First Sexual Intercourse", "Years of Smoking",
                    "Years of Hormonal Contraceptive Use", "Years with IUD",
                    "Number of STDs")

numeric_vals <- c("Age", "Partners", "Pregnancies", "First", "Smokes_years",
                  "Contraceptives_years", "IUD_years", "STDs_number")

cat_names <- c("Smokes", "Use of Hormonal Contraceptive", "Use of IUD", "Any STDs")

cat_vals <- c("Smokes", "Contraceptives", "IUD", "STDs")

facet_names <- c("None", "Smokes", "Use of Hormonal Contraceptive",
                 "Use of IUD", "Any STDs")

facet_vals <- c("None", "Smokes", "Contraceptives", "IUD", "STDs")

##########################

fluidPage(
    
    titlePanel("Cervical Cancer Risk"),
    
    tabsetPanel(
        tabPanel("About",
                 mainPanel(
                     h3("About the App"),
                     includeMarkdown("text/about.Rmd"),
                     img(src = "cancer.jpg"),
                     br(),
                     h4("The variables available for modeling are:"),
                     includeMarkdown("text/variables.Rmd"))),
                    

        tabPanel("Data Exploration",
                
                 h3("Numeric Summaries"),
                 fluidRow(
                     column(2,
                            br(),
                            radioButtons("numeric", 
                                         "Select Variable to Summarize",
                                         choiceNames = numeric_names,
                                         choiceValues = numeric_vals),
                            br(),
                            materialSwitch("group",
                                           "Group by Cancer Occurence",
                                           value = TRUE)),
                     column(5, plotOutput("plot1")),
                     column(5, plotOutput("plot2"))),
                 
                 fluidRow(column(2, br()),
                          column(10, dataTableOutput("table"))),
                 br(),
                 
                 h3("Categorical Summaries"),
                 fluidRow(column(2,
                                 radioButtons("cat",
                                              "Select Variable to Summarize",
                                              choiceNames = cat_names,
                                              choiceValues = cat_vals),
                                 radioButtons("facet",
                                              "Select Faceting Variable",
                                              choiceNames = facet_names,
                                              choiceValues = facet_vals)),
                          column(5, plotOutput("plot3")),
                          column(5, plotOutput("plot4")))),
        
        tabPanel("Modeling",
                 
                 tabsetPanel(
                     tabPanel("Modeling Info",
                              fluidPage(
                                  br(),
                                  h4("The two approaches used to model cervical cancer risk are"),
                                  h3("logistic regression and random forest."),
                                  br(),
                                  fluidRow(
                                      withMathJax(),
                                      column(6,
                                             includeMarkdown("text/logistic_info.Rmd")),
                                      column(6,
                                             includeMarkdown("text/rf_info.Rmd")))
                                  )),
                     
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
                                                    "Select Number of 
                                                    Variables to Sample",
                                                    value = c(1, 5),
                                                    min = 1,
                                                    max = 10),
                                  numericInput("cvfolds",
                                               "Select Number of Cross Validation Folds",
                                                    value = 5,
                                                    min = 2,
                                                    max = 10),
                                  br(),
                                  actionButton("button", "Run Models")),
                              mainPanel(
                                  h3(textOutput("glmt")),
                                  verbatimTextOutput("glm"),
                                  h3(textOutput("rft")),
                                  fluidRow(
                                      column(6, verbatimTextOutput("rf")),
                                       column(6, plotOutput("imp"))),
                                  fluidRow(
                                      column(6,
                                             h3(textOutput("glm_test_title")),
                                             verbatimTextOutput("glm_test")),
                                      column(6,
                                             h3(textOutput("rf_test_title")),
                                             verbatimTextOutput("rf_test"))))),
    
                     tabPanel("Prediction",
                              h3("Enter values below to make a prediction"),
                              h4("Note: predictions are made using the most recent fitted models"),
                              h4("yes = 1 and no = 0"),
                              br(),
                              fluidRow(
                                  column(3,
                                         numericInput("age",
                                                      "Age",
                                                      value = 50,
                                                      min = 1,
                                                      max = 100),
                                         numericInput("partners",
                                                      "Number of sexual partners",
                                                      value = 0,
                                                      min = 0,
                                                      max = 100),
                                         numericInput("first",
                                                      "Age of first sexual intercourse",
                                                      value = 0,
                                                      min = 0,
                                                      max = 100),
                                         numericInput("preg",
                                                      "Number of pregnancies",
                                                      value = 0,
                                                      min = 0,
                                                      max = 100),
                                         numericInput("smokes",
                                                      "Smokes",
                                                      value = 0,
                                                      min = 0,
                                                      max = 1),
                                         numericInput("smokeyrs",
                                                      "Years of smoking",
                                                      value = 0,
                                                      min = 0,
                                                      max = 100)),
                                  column(3, 
                                         numericInput("cont",
                                                      "Use of hormonal contraceptives",
                                                      value = 0,
                                                      min = 0,
                                                      max = 1),
                                         numericInput("contyrs",
                                                      "Years of hormonal 
                                                      ontraceptive use",
                                                      value = 0,
                                                      min = 0,
                                                      max = 100),
                                         numericInput("iud",
                                                      "Use of IUD",
                                                      value = 0,
                                                      min = 0,
                                                      max = 1),
                                         numericInput("iudyrs",
                                                      "Years with IUD",
                                                      value = 0,
                                                      min = 0,
                                                      max = 100),
                                         numericInput("stds",
                                                      "Any STDs",
                                                      value = 0,
                                                      min = 0,
                                                      max = 1),
                                         numericInput("stdsyrs",
                                                      "Number of STDs",
                                                      value = 0,
                                                      min = 0,
                                                      max = 10)),
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
                                        