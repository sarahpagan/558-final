# ST 558 Final Project

## An R Shiny App for Modeling Cervical Cancer Risk

The goal of this application is to explore and model data on cervical cancer risk factors from the University of California Irvine Machine Learning Repository. The application utilizes synthetic minority oversampling, logistic regression, and random forest techniques to fit binary classification models for cervical cancer outcomes. Data partitioning and model training are performed using the `caret` package.

The following is a list of R packages required to run the Shiny application:

1.  `shiny`
2.  `shinyWidgets`
3.  `readr`
4.  `tidyverse`
5.  `caret`

To install all packages, run the following code:

```         
install.packages(c("shiny", "shinyWidgets", "readr", "tidyverse", "caret"))
```

Once all packages are installed, run the app with the following code:

```         
shiny::runGitHub(repo = "558-final", username = "sarahpagan")
```
