# some libs for shinyapps to deploy correctly
library(shiny)
library(curl)
library(e1071)
library(data.table)
library(caret)
# models for caret, need here explicitly for shinyapps deployment
library(randomForest) # for rf
library(gbm)    # for gbm
library(shinyalert)
library(mboost) # for glmboost
library(klaR)   # for nb
library(plyr)
library(dplyr)
library(psych)
library(purrr)
library(pROC)
library(yardstick)
library(kableExtra)
library(ROSE)

library(smotefamily)
options(shiny.maxRequestSize=170*1024^2) 

disable <- function(x) {
  if (inherits(x, 'shiny.tag')) {
    if (x$name %in% c('input', 'select'))
      x$attribs$disabled <- 'disabled'
    x$children <- disable(x$children)
  }
  else if (is.list(x) && length(x) > 0) {
    for (i in 1:length(x))
      x[[i]] <- disable(x[[i]])
  }
  x
}

shinyServer(
  function(input, output) {
    
    ## Data Upload
    dataInput <- reactive({
      req(input$upload)
      
      ext <- tools::file_ext(input$upload$name)
      switch(ext,
             csv = vroom::vroom(input$upload$datapath, delim = ","),
             validate("Invalid file; Please upload a .csv file")
      )
    })
    output$target <- renderUI({
      selectInput('target', 'Select the Outcome/Target Variable', 
                  choices=as.list(colnames(dataInput())))
    })
    output$files <- renderTable(input$upload)
    # output$head <- DT::renderDT(
    #   dataInput(), extensions = 'Buttons', filter = "top", rownames=F,
    #   
    # )
    
    # In the server function
    output$gridSearchParams <- renderUI({
      req(input$machAlgorithm)
      
      switch(input$machAlgorithm,
             "Random Forest" = {
               tagList(
                 numericInput("rf_ntree", "Number of trees", value = 500, min = 1),
                 numericInput("rf_mtry", "Number of variables to possibly split at in each node", value = 3, min = 1),
                 numericInput("rf_nodesize", "Minimum size of terminal nodes", value = 5, min = 1)
               )
             },
             "SVM" = {
               tagList(
                 selectInput("svm_kernel", "Kernel", choices = c("linear", "radial", "polynomial")),
                 numericInput("svm_cost", "Cost", value = 1, min = 0.1),
                 numericInput("svm_gamma", "Gamma (for radial and polynomial kernels)", value = 1, min = 0.1)
               )
             },
             "Logistic Regression" = {
               tagList(
                 numericInput("lr_alpha", "Alpha (for regularization)", value = 0, min = 0),
                 selectInput("lr_family", "Family", choices = c("binomial", "gaussian"))
               )
             },
             "XGBoost" = {
               tagList(
                 numericInput("xgb_eta", "Learning rate", value = 0.3, min = 0.01, max = 1, step = 0.01),
                 numericInput("xgb_max_depth", "Max tree depth", value = 6, min = 1, step = 1),
                 numericInput("xgb_subsample", "Subsample ratio", value = 1, min = 0.1, max = 1, step = 0.1),
                 numericInput("xgb_colsample_bytree", "Column sample by tree", value = 1, min = 0.1, max = 1, step = 0.1)
               )
             },
             # Add more algorithms as needed
             NULL  # Default case if no algorithm is selected
      )
    })
    output$head <- DT::renderDT({
      datatable(dataInput(), extensions = 'Buttons', filter = "top",options = list(
        scrollX = TRUE,
        pageLength = 10  # Adjust the page length as needed
      ))
    })
    
    
    encode_ordinal <- function(x, order = unique(x)) {
      x <- as.numeric(factor(x, levels = order, exclude = NULL))
      x
    }
    
    data <- reactiveVal()
    
    observeEvent(input$upload, ignoreNULL = T, ignoreInit = T, {
      file1 <- input$upload
      if (is.null(file1)){return()}
      data(read.csv(file=file1$datapath))
    })
    
    observeEvent(input$drop, {
      claim <- data()
      drop <- c(input$dropCols)
      claim <- claim[, !names(claim) %in% drop]
      data(claim)
    })
    
    observeEvent(input$preprocess, {
      claim <- data()
      claim <- claim[complete.cases(claim),]
      
      data(claim)
    })
    
    
    observeEvent(input$categoricalconversion, {
      claim <- data()
      colTypes <- map(claim, class)
      for (col in colnames(claim)) {
        if (colTypes[col] == 'character') {
          claim[[col]] <- encode_ordinal(claim[[col]])
        }
      }
      data(claim)
    })
    
    
    
    
    ## Data Summary
    output$dropSelected <- renderUI({
      selectInput("dropCols", "Select the Columns you want to drop:",
                  choices=as.list(colnames(data())), multiple = T)
    })
    
    
    output$PredictorsSummaryOut <- renderDT({
      numeric_data <- data() %>% select_if(is.numeric)
      
      # Use describe() and round() to limit the summary to three digits
      summary_data <- describe(numeric_data)
      summary_data[] <- lapply(summary_data, function(x) if(is.numeric(x)) round(x, 3) else x)
      
      # Remove 'vars' and 'n' columns from the summary_data
      summary_data <- summary_data[, !(names(summary_data) %in% c("vars", "n"))]
      
      datatable(
        summary_data,
        options = list(pageLength = 10)  # Adjust the page length as needed
      )
    })
    
    
    output$nonNumericOutput <- renderUI({
      
      non_numeric_features <- colnames(data() %>% select_if(function(x) !is.numeric(x)))
      selectInput("Sekectcatfeature", "Select categorical feature",
                  choices=as.list(non_numeric_features), selected = non_numeric_features[1])
      
      
    })
    
    
    
    
    observeEvent(input$undersampleButton, {
      req(data())  # Ensure data is available
      # print("ffffffffffffffffffffffff")
      print(input$target)
      df <- data()
      
      counts <- table(data()[[input$target]])
      
      minority_class <- names(counts)[which.min(counts)]
      minority_count <- counts[minority_class]
      # print("ffffffffffffffffffffffff")
      print(minority_class)
      print(minority_count)
      # Sample the majority class
      majority_class <- names(counts)[which.max(counts)]
      # print(majority_class)
      # print(colnames(df))
      majority_data <- df[df$y == "1", ]
      minority_data <- df[df$y == "2",]
      
      # Undersample the majority class
      undersampled_majority <- majority_data %>% sample_n(minority_count)
      
      # Combine the undersampled majority with the minority class
      balanced_data <- bind_rows(undersampled_majority, minority_data)      
      # Update the data reactive value
      data(balanced_data)
    })
    
    
    
    observeEvent(input$oversampleButton, {
      # Ensure that data is available
      req(data())
      
      # Retrieve the current dataset
      current_data <- data()
      
      # Ensure the target variable is selected and available
      req(input$target)
      target_var <- input$target
      
      # Ensure the user has specified the number of samples
      req(input$numSamples)
      desired_samples <- as.numeric(input$numSamples)
      
      
      smote_result <- SMOTE(current_data[, -which(names(current_data) == target_var)],  # Exclude target_var
                            current_data[[target_var]],                                # Target column
                            K = 5,    # Number of nearest neighbors
                            dup_size = (desired_samples))  # Amount of oversampling
      
      # Check the output of SMOTE
      print(dim(smote_result$data))  # Check dimensions of generated data
      print(length(smote_result$class))  # Check length of generated classes
      
      # Check if any samples were generated
      if (nrow(smote_result$data) > 0 && length(smote_result$class) > 0) {
        # Combine the SMOTE-generated data with the original data
        oversampled_data <- cbind(smote_result$data, target_var = smote_result$class)
        
        # Update the data with the oversampled data
        data(oversampled_data)
      } 
      else {
        
        showModal(modalDialog(
          title = "Error",
          "SMOTE did not generate any samples. Please check your input data.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        # shinyalert(
        #   title = "Error", 
        #   text = "SMOTE did not generate any samples. Please check your input data.", 
        #   type = "error", 
        #   closeOnClickOutside = TRUE,
        #   callbackR = function() {
        #     # Optional: add any action to perform after closing the alert
        #   }
        # )
        # shinyalert("Error", "SMOTE did not generate any samples. Please check your input data.", type = "error", closeOnClickOutside = TRUE)
      }
      
      
    })
    
    
    
    
    
    
    output$NonNumericalSummaryOut <- renderDT({
      non_numeric_data <- data() %>% select_if(function(x) !is.numeric(x))
      
      # Variable sélectionnée dans le selectInput
      selected_variable <- input$Sekectcatfeature
      
      # Si aucune variable n'est sélectionnée, utilise la première variable non numérique
      if (is.null(selected_variable) || length(selected_variable) == 0) {
        selected_variable <- names(non_numeric_data)[1]
      }
      
      # Vérifie si des variables non numériques existent
      if (length(names(non_numeric_data)) > 0) {
        # Crée et stocke la table de fréquence pour la variable sélectionnée
        freq_table <- table(non_numeric_data[[selected_variable]])
        table_output <- data.frame(Category = names(freq_table), Frequency = as.vector(freq_table))
        
        # Affiche la table de fréquence
        datatable(
          data = table_output,
          options = list(pageLength = 10),
          caption = sprintf("Frequency Table for %s", selected_variable)
        )
      } else {
        
      }
    })
    
    
    
    output$NullPercentageOut <- renderDT({
      null_percentage <- data.frame(
        Feature = names(data()),
        NullPercentage = colMeans(is.na(data())) * 100
      )
      
      
      null_percentage$NullPercentage <- format(round(null_percentage$NullPercentage, 2), nsmall = 2)
      
      
      # Reorder the data frame in descending order of NullPercentage
      null_percentage <- null_percentage %>% arrange(desc(NullPercentage))
      
      datatable(
        null_percentage,
        options = list(pageLength = 10),
        rownames = FALSE  # Exclude row names from being treated as a separate column
      )
    })
    
    
    
    output$OutcomeSummaryOut <- renderTable({ 
      table(data()[[input$target]])
    }, rownames = FALSE)
    
    
    ## Explore Data
    # pairs plot - always
    output$expPairsPlot <- renderPlot({
      featurePlot(x = data(), 
                  y = data()[input$target], 
                  plot = 'pairs', auto.key = list(columns = 2))
    })
    output$expXaxisVarSelector <- renderUI({
      selectInput('expXaxisVar', 'Variable on x-axis', 
                  choices = as.list(colnames(data())), selected = colnames(data())[1])
    })
    # generate variable selectors for individual plots
    getYaxisVarSelector <- function(geom) { 
      # wy = wtih y, wo = without y (or disable)
      widget <- selectInput('expYaxisVar', 'Variable on y-axis', 
                            choices = as.list(colnames(data())), selected = colnames(data())[2])
      wy <- widget
      woy <- disable(widget)
      switch(geom,
             point = wy,
             boxplot = wy,
             histogram = woy,
             density = woy,
             jitter = wy
      )
    }
    output$expYaxisVarSelector <- renderUI({
      getYaxisVarSelector(input$singlePlotGeom)
    })
    output$expColorVarSelector <- renderUI({
      selectInput('expColorVar', 'Variable to color by', 
                  choices = as.list(c('None', colnames(data()))),
                  selected = input$target)
    })
    # create ggplot statement based on geom
    add_ggplot <- function(geom) {
      gx <- ggplot(data(), aes_string(x = input$expXaxisVar))
      gxy <- ggplot(data(), aes_string(x = input$expXaxisVar, y = input$expYaxisVar))
      switch(geom,
             point = gxy,
             boxplot = gxy,
             histogram = gx,
             density = gx,
             jitter = gxy
      )
    }
    # create ggplot geom
    add_geom <- function(geom) {
      switch(geom,
             point = geom_point(aes_string(color = input$expColorVar)),
             boxplot = geom_boxplot(aes_string(color = input$expColorVar)),
             histogram = geom_histogram(aes_string(color = input$expColorVar)),
             density = geom_density(aes_string(color = input$expColorVar)),
             jitter = geom_jitter(aes_string(color = input$expColorVar))
      )
    }
    output$expSinglePlot <- renderPlot({
      g <- add_ggplot(input$singlePlotGeom) + add_geom(input$singlePlotGeom)
      print(g)
    })
    # ...
    
    # Ajoute cette partie pour les calculs sans affichage basés sur les variables
    observeEvent(c(input$expXaxisVar, input$expYaxisVar, input$statisticalTest), {
      
      # Calculs basés sur la méthode et les variables sélectionnées
      if (!is.null(input$statisticalTest)) {
        
        if (input$statisticalTest == "Correlation Matrix") {
          
          correlation_matrix <- cor(data()[, c(input$expXaxisVar, input$expYaxisVar)], use = "complete.obs")
          output$metricResults <- renderPrint({
            cat("Correlation Matrix:\n")
            print(correlation_matrix)
          })
          
        }
        else if (input$statisticalTest == 'ANOVA') {
          anova_result <- aov(data()[[input$expYaxisVar]] ~ data()[[input$expXaxisVar]])
          output$metricResults <- renderPrint({
            cat("ANOVA Results:\n")
            print(summary(anova_result))
          })
          
        }
        else if (input$statisticalTest == 'Chi-squared (khi2)') {
          cross_table <- table(data()[[input$expXaxisVar]], data()[[input$expYaxisVar]])
          chi_square_result <- chisq.test(cross_table)
          output$metricResults <- renderPrint({
            cat("Chi-Square Results:\n")
            print(chi_square_result)
          })
          
        }
        # Ajoute d'autres conditions ici selon les nouvelles métriques que tu veux afficher
      }
    }, ignoreInit = TRUE)
    
    
    
    ## Prediction Model
    f <- reactive({
      as.formula(paste(input$target, "~."))
    })
    # create feature selection
    output$featureSelectInput <- renderUI({
      # Get the column names of the dataset excluding the target variable
      feature_choices <- colnames(data())[!(colnames(data()) %in% c(input$target))]
      
      # Create a selectInput with all features
      selectInput(
        'featureSelect', 
        'Select features to generate model', 
        choices = as.list(feature_choices),
        multiple = TRUE,
        selected = feature_choices  # Select the first three features by default
      )
    })
    
    
    output$machAlgorithm <- renderUI({
      selectInput('machLearnAlgorithm', 
                  'Select the machine learning algorithm',
                  if (input$mltype == "reg") {
                    choices = c('Linear Regression' = 'lm',
                                'Random Forest' = 'rf',
                                'SVM (Linear Kernel)' = 'svmLinear',
                                'SVM (Radial Kernel)' = 'svmRadial')
                  } else {
                    choices = c(
                                'Random Forest' = 'rf',
                                'SVM (Linear Kernel)' = 'svmLinear',
                                'SVM (Radial Kernel)' = 'svmRadial')
                  }
      ) 
    })
    
    
    
    
    #split the data into train and test
    splitSlider <- reactive({
      input$fracTrain / 100
    })
    trainRowIndex <- reactive({
      sample(1:nrow(data()), splitSlider() * nrow(data()))
    })
    
    trainData <- reactive({
      train_dt <- data()
      train_dt <- train_dt[trainRowIndex(),]
    })
    
    testData <- reactive({
      test_dt <- data()
      test_dt <- test_dt[-trainRowIndex(),]
    })
    
    output$cntTrain <- renderText({
      paste0("Training set: ", nrow(trainData()), " records")
    })
    output$cntTest <- renderText({
      paste0("Test set: ", nrow(testData()), " records")
    })
    grid_params <- list()
    
    
    
    # apply model to training set with GridSearch, handling multi-class with multinom
    applyModel <- function(modelType, features) {
      df <- trainData()
      
      if (input$mltype == "clf") {
        df[[input$target]] <- as.factor(df[[input$target]])
        metric <- 'Accuracy'
      } else {
        metric <- 'RMSE'
      }
      
      # Handle multi-class cases
      if (modelType == 'glm' && length(unique(df[[input$target]])) > 2) {
        modelType <- 'multinom'
      }
      
      # Define hyperparameter grids for different models
      grid <- NULL
      if (modelType == 'rf') {
        grid <- expand.grid(
          mtry = c(floor(sqrt(length(features))), floor(length(features)/3), floor(length(features)/2))
        )
      } else if (modelType == 'glm' || modelType == 'multinom') {
        grid <- expand.grid(
          alpha = c(0, 0.5, 1)
        )
      } else if (modelType == 'svmLinear') {
        grid <- expand.grid(
          C = c(0.1, 1, 10)
        )
      } else if (modelType == 'svmRadial') {
        grid <- expand.grid(
          C = c(0.1, 1, 10),
          sigma = c(0.01, 0.1, 1)
        )
      } else if (modelType == 'lm') {
        grid <- NULL  # Linear regression doesn't typically use hyperparameter tuning
      }
      
      grid_params <- grid
      
      # Display grid parameters
      output$grid_params <- renderText({
        if (!is.null(grid)) {
          grid_text <- paste("Grid Parameters:\n", paste(capture.output(print(grid)), collapse = "\n"))
          return(grid_text)
        }
        return("No grid parameters for this model.")
      })
      
      # Train the model with GridSearch
      train(f(), 
            data = select(df, one_of(c(input$target, features))), 
            method = modelType, 
            preProcess = input$preProcessMethods, 
            tuneGrid = grid, 
            metric = metric,
            trControl = trainControl(method = "cv", number = 5),
            verbose = F)
    }

      
      
    
    # reactive functions to run and evaluate model
    runModel <- reactive({
      applyModel(input$machLearnAlgorithm, input$featureSelect)
    })
    
    
    
    
    output$featureImportance <- renderPrint({
      model <- runModel()  # Assuming `runModel()` returns your trained model
      
      if (input$machLearnAlgorithm %in% c('rf', 'glm', 'multinom', 'lm')) {
        v <- varImp(model, scale = TRUE)[["importance"]]
        v$Overall <- v$Overall / sum(v$Overall)
        
        imp <- as.data.frame(v)
        imp <- data.frame(names = rownames(imp), overall = imp$Overall)
        imp <- imp[order(imp$overall, decreasing = TRUE),]
        
        print(imp)
      } else if (input$machLearnAlgorithm == 'svmLinear') {
  # Extraire les coefficients et le support vectoriel pour un SVM linéaire
  weights <- colSums(model$finalModel@xmatrix[[1]] * model$finalModel@coef[[1]])
  coef_names <- names(weights)
  
  # Vérifier si les poids sont bien extraits et non NULL
  if (is.null(weights) || length(weights) == 0) {
    stop("Les coefficients n'ont pas pu être extraits.")
  }
  
  # Calculer l'importance des coefficients
  importance <- abs(weights)
  importance <- importance / max(importance, na.rm = TRUE)
  
  # Créer un data frame avec les noms de variables et leur importance
  importance_df <- data.frame(
    Variable = coef_names, 
    Importance = importance
  )
  
  # Trier par importance décroissante
  importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]
  
  # Afficher le tableau des coefficients d'importance
  print(importance_df)
}

      

else if (input$machLearnAlgorithm == 'svmRadial') {
        cat("Feature importance is not directly available for SVM with radial kernel.\n")
        cat("Consider using a permutation importance method or other model-agnostic techniques for feature importance.")
      }
    }, width = 10000)
    
    # The applyModel function remains the same as in the previous version
    
    
    
    
    
    
    
    # summary of final model
    output$finalModel <- renderPrint({
      print(runModel())
    }, width = 10000)
    
    
    
    evalModel <- function(testData, features) {
      predictions <- predict(runModel(), select(testData, one_of(features)))
      truthes <- testData[, input$target]
      
      if (input$mltype == "clf") {
        # Classification case
        confusion_matrix <- confusionMatrix(predictions, as.factor(truthes))
        
        
        # Print and return both data frames
        print(confusion_matrix)
        print(confusion_matrix$byClass)
        
      } else {
        # Regression case
        truthes <- as.numeric(truthes)
        
        # Your existing regression metrics
        rmse <- sqrt(mean((truthes - predictions)^2))
        r2 <- cor(truthes, predictions)^2
        mae <- mean(abs(truthes - predictions))
        mase <- mean(abs(truthes - predictions))/mean(abs(diff(truthes)))
        adjr2 <- 1 - (1 - r2)*(nrow(testData) - 1)/(nrow(testData) - length(features) - 1)
        
        # Return a data frame with both regression and classification metrics
        print(data.frame("RMSE"=rmse, "R2"=r2, "MAE"=mae, "MASE"=mase, "AdjR2"=adjr2))
      }
    }
    
    
    # Server code to download the model
    output$downloadModel <- downloadHandler(
      filename = function() {
        paste("your_model", Sys.Date(), ".rds", sep = "")  # Naming the file as 'your_model_date.rds'
      },
      content = function(file) {
        # Save the model to a temporary file
        model <- runModel()  # get the model
        saveRDS(model, file)  # save the model to 'file'
      }
    )
    
    
    #training data accuracy
    output$inSampleAccuracy <- renderPrint({
      df <- trainData()
      df <- if (input$mltype == "clf") {
        # Convert target variable from numeric to factor
        df[, input$target] <- as.factor(df[, input$target])
        df
      }
      else
        df
      evalModel(df, input$featureSelect)
    }, width = 10000)
    
    output$inSamplePlot <- renderPlot({
      df <- trainData()
      df <- if (input$mltype == "clf") {
        # Convert target variable from numeric to factor
        df[, input$target] <- as.factor(df[, input$target])
        df
      }
      else
        df
      predictions <- predict(runModel(), select(df, one_of(input$featureSelect)))
      truthes <- df[, input$target]
      truth_predicted <- data.frame(
        obs = truthes,
        pred = predictions
      )
      if (input$mltype == "clf") {
        # plot confusion matrix
        cm <- conf_mat(truth_predicted, obs, pred)
        autoplot(cm, type='heatmap', scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")) + 
          ggtitle("Confusion Matrix (Train)") +
          theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5))
      }
      else {
        # Plot residuals vs fitted values using carat
        ggplot(truth_predicted, aes(x=pred, y=obs)) +
          geom_point() +
          geom_abline(intercept=0, slope=1, color="red") +
          labs(x="Predicted", y="Observed") +
          ggtitle("Residuals vs Fitted Values (Train)") +
          theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5))
      }
    })
    
    
    #testing data accuracy
    output$outOfSampleAccuracy <- renderPrint({
      df <- testData()
      df <- if (input$mltype == "clf") {
        # Convert target variable from numeric to factor
        df[, input$target] <- as.factor(df[, input$target])
        df
      }
      else
        df
      evalModel(df, input$featureSelect)
    }, width = 10000)
    
    output$residualsplottest <-renderPlot({
      df <- testData()
      
      if (input$mltype != "clf") {
        
        predictions <- predict(runModel(), select(df, one_of(input$featureSelect)))
        truthes <- df[, input$target]
        truth_predicted <- data.frame(
          obs = truthes,
          pred = predictions
        )
        
        residuals <- truthes - predictions
        standard_deviation <- sd(residuals)
        
        ggplot(truth_predicted, aes(x = pred, y = obs - pred)) +
          geom_point() +
          geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
          geom_hline(yintercept = 2 * standard_deviation, linetype = "dashed", color = "blue") +
          geom_hline(yintercept = -2 * standard_deviation, linetype = "dashed", color = "blue") +
          labs(x = "Predicted", y = "Residuals") +
          ggtitle("Residuals Plot (Test)") +
          theme(text = element_text(size = 20), plot.title = element_text(hjust = 0.5))
      }
      else {
        df[, input$target] <- as.factor(df[, input$target])
        predictions <- predict(runModel(), select(df, one_of(input$featureSelect)))
        truthes <- df[, input$target]
        
        roc_curve <- roc(truthes, as.numeric(predictions))
        plot(roc_curve, col = "blue", main = "ROC Curve", lwd = 2)
        # Calculate and display the AUC
        auc_value <- auc(roc_curve)
        legend("bottomright", legend = paste("AUC =", round(auc_value, 2)), col = "blue", lwd = 2)
        
        
      }
      
    })
    
    
    output$residualsplottrain <-renderPlot({
      df <- trainData()
      if (input$mltype != "clf") {
        
        predictions <- predict(runModel(), select(df, one_of(input$featureSelect)))
        truthes <- df[, input$target]
        truth_predicted <- data.frame(
          obs = truthes,
          pred = predictions
        )
        
        residuals <- truthes - predictions
        standard_deviation <- sd(residuals)
        
        ggplot(truth_predicted, aes(x = pred, y = obs - pred)) +
          geom_point() +
          geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
          geom_hline(yintercept = 2 * standard_deviation, linetype = "dashed", color = "blue") +
          geom_hline(yintercept = -2 * standard_deviation, linetype = "dashed", color = "blue") +
          labs(x = "Predicted", y = "Residuals") +
          ggtitle("Residuals Plot (Train)") +
          theme(text = element_text(size = 20), plot.title = element_text(hjust = 0.5))
      }
      else {
        df[, input$target] <- as.factor(df[, input$target])
        predictions <- predict(runModel(), select(df, one_of(input$featureSelect)))
        truthes <- df[, input$target]
        
        roc_curve <- roc(truthes, as.numeric(predictions))
        plot(roc_curve, col = "blue", main = "ROC Curve", lwd = 2)
        
        auc_value <- auc(roc_curve)
        legend("bottomright", legend = paste("AUC =", round(auc_value, 2)), col = "blue", lwd = 2)
        
      }
      
    })
    
    
    
    
    output$outOfSamplePlot <- renderPlot({
      df <- testData()
      df <- if (input$mltype == "clf") {
        # Convert target variable from numeric to factor
        df[, input$target] <- as.factor(df[, input$target])
        df
      }
      else
        df
      predictions <- predict(runModel(), select(df, one_of(input$featureSelect)))
      truthes <- df[, input$target]
      truth_predicted <- data.frame(
        obs = truthes,
        pred = predictions
      )
      if (input$mltype == "clf") {
        # plot confusion matrix
        cm <- conf_mat(truth_predicted, obs, pred)
        autoplot(cm, type='heatmap', scale_fill_gradient(low="#D6EAF8", high="#2E86C1")) + 
          ggtitle("Confusion Matrix (Test)") +
          theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5))
      }
      else {
        # Plot residuals vs fitted values using carat
        ggplot(truth_predicted, aes(x=pred, y=obs)) +
          geom_point() +
          geom_abline(intercept=0, slope=1, color="red") +
          labs(x="Predicted", y="Observed")  +
          ggtitle("Residuals vs Fitted Values (Test)") +
          theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5)) 
      }
      
    })
    
  }
  
)