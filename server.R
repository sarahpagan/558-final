library(shiny)
library(caret)

function(input, output, session) {
 
    ### Model Fitting   
    output$split <- renderText({
        paste0(input$slider, "% of the data is allocated to training")
    })
    
    index <- eventReactive(input$button, {
        createDataPartition(data$Cancer, p = input$slider/100, list = FALSE)
    })
    
    trainData <- eventReactive(input$button, {
        data[index(), ]
    })
    
    testData <- eventReactive(input$button, {
        data[-index(), ]
    })
    
    vars <- eventReactive(input$button, {
        input$multi
    })
    
    mtry <- eventReactive(input$range, {
        input$range
    })
    
    output$glm <- renderPrint({
        formula <- paste("Cancer", collpase = "~", 
                         paste(vars(), collapse = "+"))
        logitfit <- glm(formula, data = trainData(), family = binomial)
        summary(logitfit)
    })
    
    output$rf  <-  renderPrint({
        rf_fit <- train(Cancer ~ ., data = trainData(),
                       method = "rf",
                       family = "binomial",
                       metric = "Accuracy",
                       trControl = trainControl(method = "cv",
                                                number = 5),
                       tuneGrid = data.frame(mtry = mtry()[1]:mtry()[2]))
        list(rf_fit$results, confusionMatrix(rf_fit))
    })
}