library(shiny)
library(caret)
library(forcats)

function(input, output, session) {
 
######## Model fitting   
    
    output$split <- renderText({
        paste0(input$slider, "% of the data is allocated to training")
    })
    
    index <- eventReactive(input$button, {
        createDataPartition(data$Cancer, p = input$slider/100, list = FALSE)
    })
    
    train_data <- eventReactive(input$button, {
        data[index(), ]
    })
    
    test_data <- eventReactive(input$button, {
        data[-index(), ]
    })
    
    logit_title <- eventReactive(input$button, {
        "Logistic Regression Model"
    })
    
    logit_fit <- eventReactive(input$button, {
        formula <- paste("Cancer", collpase = "~", 
                         paste(input$multi, collapse = "+"))
        glm(formula, data = train_data(), family = binomial)
    })
    
    rf_title <- eventReactive(input$button, {
        "Random Forest Model"
    })
    
    rf_fit <- eventReactive(input$button, {
        mtry <- input$range
        train(Cancer ~ ., data = train_data(),
              method = "rf",
              family = "binomial",
              metric = "Accuracy",
              trControl = trainControl(method = "cv",
                                       number = input$cvfolds),
              tuneGrid = data.frame(mtry = mtry[1]:mtry[2]))
    })
    
    logit_test_title <- eventReactive(input$button, {
        "Logistic Regression Test Results"
    })
    
    logit_test <- eventReactive(input$button, {
        test <- test_data()
        preds <- predict(logit_fit(), newdata = test, type = "response")
        classify <- ifelse(preds > 0.5, 1, 0)
        confusionMatrix(as_factor(classify), test$Cancer)
    })
    
    output$glmt <- renderText({
        logit_title()
    })
    
    output$glm <- renderPrint({
        logit_results <- logit_fit()
        summary(logit_results)
    })
    
    output$rft <- renderText({
        rf_title()
    })
    
    output$rf  <-  renderPrint({
        rf_fit()
    })
    
    output$glm_test_title <- renderText({
        logit_test_title()
    })
    
    output$glm_test <- renderPrint({
        logit_test()
    })
    
####################
    
    
######## Prediction
    
    new <- eventReactive(input$predict, {
        data.frame(
            Age = input$age,
            Partners = input$partners,
            First = input$first,
            Pregnancies = input$preg,
            Smokes = as_factor(input$smokes),
            Smokes_years = input$smokeyrs,
            Contraceptives = as_factor(input$cont),
            Contraceptives_years = input$contyrs,
            IUD = as_factor(input$iud),
            IUD_years = input$iudyrs,
            STDs = as_factor(input$stds),
            STDs_number = input$stdsyrs,
            STD_condylomatosis = as_factor(input$c),
            STD_vulvo_perineal_condylomatosis = as_factor(input$vpc),
            STD_syphilis = as_factor(input$s),
            STD_genital_herpes = as_factor(input$gh),
            STD_molluscum_contagiosum = as_factor(input$mc),
            STD_HIV = as_factor(input$hiv),
            STD_HPV = as_factor(input$hpv),
            Dx_Cancer = as_factor(input$c),
            Dx_CIN = as_factor(input$cin),
            Dx_HPV = as_factor(input$dhpv))
    })
    
    glmp_title <- eventReactive(input$predict, {
        "Logistic Regression Model Predction"
    })
    
    sub_title <- eventReactive(input$predict, {
        "Probability of cervical cancer:"
    })
    
    rfp_title <- eventReactive(input$predict, {
        "Random Forest Model Predction"
    })
    
    sub_title2 <- eventReactive(input$predict, {
        "Probability of cervical cancer:"
    })
    
    output$glmpt <- renderText({
        glmp_title()
    })
    
    output$sub <- renderText({
        sub_title()
    })
    
    output$rfpt <- renderText({
        rfp_title()
    })
    
    output$sub2 <- renderText({
        sub_title2()
    })
    
    output$glm_pred <- renderPrint({
        predict(logit_fit(), newdata = new(), type = "response")
    })
    
    output$rf_pred <- renderPrint({
        predict(rf_fit(), newdata = new(), type = "prob")
    })
}