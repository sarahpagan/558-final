library(shiny)
library(readr)
library(dplyr)
library(forcats)
library(caret)
library(tidyr)
library(ggplot2)

function(input, output, session) {
    
######## Read in Data
    
    data <- read_csv("data/CC.csv") |>
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

######################

######## Data Exploration
    
    output$plot1 <- renderPlot({
        g <- ggplot(data, aes(x = get(input$numeric)))
        g + geom_boxplot() +
            xlab(input$numeric) +
            labs(title = paste0("Boxplot of ", input$numeric))
    })
    
    output$plot2 <- renderPlot({
        g <- ggplot(data, aes(x = get(input$numeric)))
        
        if(input$group == TRUE){
        g + geom_density(aes(fill = Cancer), alpha = 0.6) +
                scale_fill_manual(values = c("cyan2", "deeppink")) +
                xlab(input$numeric) +
                labs(title = paste0("Density of ",
                                    input$numeric,
                                    " by Occurence of Cervical Cancer"))}
        else{
            g + geom_density(fill = "deeppink", alpha = 0.75) +
                xlab(input$numeric) +
                labs(title = paste0("Density of ",
                                    input$numeric))}
    })
    
    output$table <- renderDataTable ({
        
        if(input$group == TRUE){
        data |>
            select(input$numeric, Cancer) |>
            group_by(Cancer) |>
            summarise(Min = min(get(input$numeric)),
                      Max = max(get(input$numeric)),
                      Mean = round(mean(get(input$numeric)), 2),
                      Variance = round(var(get(input$numeric)), 2),
                      "Standard Deviation" = round(sd(get(input$numeric)), 2))}
        else{data |>
                select(input$numeric) |>
                summarise(Min = min(get(input$numeric)),
                          Max = max(get(input$numeric)),
                          Mean = round(mean(get(input$numeric)), 2),
                          Variance = round(var(get(input$numeric)), 2),
                          "Standard Deviation" = round(sd(get(input$numeric)), 2))}
        },options = list(dom = "t",
                         searching  = FALSE))

    output$plot3 <- renderPlot({
        
        validate(need(input$cat != input$facet,
                      "Please choose two different variables"))
        
        g <- ggplot(data, aes(x = get(input$cat)))
        
        if(input$facet == "None"){
            g + geom_bar(aes(fill = Cancer), position = "dodge") + 
                scale_fill_manual(values = c("cyan2", "deeppink")) +
                xlab(input$cat) +
                labs(title = paste0("Occurence of Cervical Cancer by ",
                                    input$cat))
            }else{
                g + geom_bar(aes(fill = Cancer), position = "dodge") +
                    facet_wrap(~get(input$facet)) +
                    scale_fill_manual(values = c("cyan2", "deeppink")) +
                    xlab(input$cat) +
                    labs(title = paste0("Occurence of Cervical Cancer by ",
                                        input$cat),
                         subtitle = paste0("Faceted by ", input$facet))}
    })
    
    output$plot4 <- renderPlot ({
        donut <- data |>
            select(STD_condylomatosis:STD_HPV, Dx_HPV) |>
            rename(HIV = STD_HIV,
                   HPV = STD_HPV,
                   Condylomatosis = STD_condylomatosis,
                   "Genital Herpes" = STD_genital_herpes,
                   "Molluscum Contagiosum" = STD_molluscum_contagiosum,
                   Syphilis = STD_syphilis,
                   "Vulvo Perineal Condylomatosis" = STD_vulvo_perineal_condylomatosis) |>
            mutate(HPV = Dx_HPV)|>
            pivot_longer(Condylomatosis:HPV,
                         names_to = "name",
                         values_to = "value") |>
            filter(value == 1) |>
            group_by(name) |>
            summarise(count = n()) |>
            mutate(frac = count/sum(count),
                   ymax = cumsum(frac),
                   ymin = c(0, head(ymax, n=-1)),
                   position = (ymax + ymin)/2)
        
        ggplot(donut, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=name)) +
            geom_rect() +
            coord_polar(theta="y")+
            xlim(c(2, 4)) +
            theme_void() +
            geom_label(x=3.5,
                       aes(y=position,
                           label=count),
                       size=6,
                       show.legend = FALSE) +
            scale_fill_manual(values = c("violetred1",
                                         "orange",
                                         "yellow",
                                         "seagreen2",
                                         "cyan",
                                         "violet",
                                         "slateblue"),
                              name = "STD") + 
            labs(title = "Prevalence of STDs",
                 subtitle = paste0("Total STD count across all patients = ",
                                   sum(donut$count)))
    })
 
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
        data[index(), ]
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
        withProgress(message = "Computing Models...", {
        mtry <- input$range
        train(Cancer ~ ., data = train_data(),
              method = "rf",
              family = "binomial",
              metric = "Accuracy",
              trControl = trainControl(method = "cv",
                                       number = input$cvfolds),
              tuneGrid = data.frame(mtry = mtry[1]:mtry[2]))
        })
    })
    
    var_imp <- eventReactive (input$button, {
        varImp(rf_fit(), scale = FALSE)
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
    
    rf_test <- eventReactive(input$button, {
        test <- test_data()
        preds <- predict(rf_fit(), newdata = test)
        confusionMatrix(preds, test$Cancer)
    })
    
    rf_test_title <- eventReactive(input$button, {
        "Random Forest Test Results"
    })
    
    output$glmt <- renderText({
        logit_title()
    })
    
    output$glm <- renderPrint({
        lf <- logit_fit()
        summary(lf)
    })
    
    output$rft <- renderText({
        rf_title()
    })
    
    output$rf  <-  renderPrint({
        rf_fit()
    })
    
    output$imp <- renderPlot({
        var_imp <- var_imp()
        plot(var_imp, top = 10)
    })
    
    output$glm_test_title <- renderText({
        logit_test_title()
    })
    
    output$glm_test <- renderPrint({
        logit_test()
    })
    
    output$rf_test_title <- renderText({
        logit_test_title()
    })
    
    output$rf_test <- renderPrint({
        rf_test()
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