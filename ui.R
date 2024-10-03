library(shiny)
library(DT)
library(shinyWidgets)
library(pROC)
library(plotly)



# Disable shiny widget, from:
# https://groups.google.com/forum/#!topic/shiny-discuss/uSetp4TtW-s
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
# function to generate the complete model summary in a single string
completeModelSummary <- function() {
  'TODO - This will be the model summary'
}


shinyUI(
  fluidPage(
    titlePanel('Data Analysis Dashboard'),
    
    navbarPage('',

               
               # upload the data and display a preview
               tabPanel('1. Upload',
                        fluidPage(
                          titlePanel(p("Upload the Data", style = "color:#3474A7")),
                          sidebarLayout(
                            sidebarPanel(
                              fileInput(inputId = "upload",
                                        label = "Upload data (.csv files only)",
                                        accept = c(".csv")),
                              br(),
                              uiOutput("target")
                            ),
                            mainPanel(
                              #   h4('Data Uploaded:'),
                              #   tableOutput("files"),
                              # br(), br(),
                              h4('Data Preview:'),
                              DTOutput("head")
                            )
                          )
                        )
               ),
               
               tabPanel('2. Data Summary',
                        titlePanel(p("Data Summary", style = "color:#3474A7")),
                        sidebarLayout(
                          sidebarPanel(
                            uiOutput(outputId = "dropSelected"),
                            actionButton("drop", "Drop Columns"),
                            br(), br(),
                            h4("Press the buttons below for simple pre-processing :", style = 'color: black;'),
                            actionButton("preprocess", "Remove missing data"),
                            actionButton("categoricalconversion", "Oridinal-Encoding Categorical Features "),
                            br(), br(),
                            # Numeric input for specifying the number of samples after oversampling
                            numericInput("numSamples", 
                                         label = "Oversampling x Times:", 
                                         value = 1,  # Default value, adjust as necessary
                                         min = 1),     # Minimum value, adjust as necessary
                            # this part here is not going to show in main branch normalement. 
                            actionButton("oversampleButton", "Oversample Minority Class"),
                            
                            br(), br(),
                            #h4("Null Values Percentage",style = 'color: black;'),
                            DTOutput('NullPercentageOut'),
                          ),
                          
                          mainPanel(
                            h4('Numerical Feature'),
                            DTOutput('PredictorsSummaryOut'),
                            br(), 
                            h4('Categorical Feature'),
                            uiOutput("nonNumericOutput"),

                           
                            DTOutput('NonNumericalSummaryOut'),
                            br(),
                            h4('Target'),
                            tableOutput('OutcomeSummaryOut')
                          )
                        )
               ),
               
               # explore the data
               tabPanel('3. Explore Data',
                        sidebarLayout(
                          sidebarPanel(
                            selectInput('singlePlotGeom', 'Select plot type', 
                                        choices=c('point', 'boxplot', 'histogram', 'density', 'jitter'),
                                        selected='jitter'),
                            uiOutput('expXaxisVarSelector'),
                            uiOutput('expYaxisVarSelector'),
                            uiOutput('expColorVarSelector'),
                            selectInput('statisticalTest', 'Select statistical test', 
                                        choices=c('Chi-squared (khi2)', 'ANOVA', 'Correlation Matrix'),
                                        selected='Correlation Matrix'),
                            
                          ),
                          
                          
                          mainPanel(
                            fluidRow(
                              # Colonne vide pour décaler le prettySwitch à droite
                              column(width = 10),
                              
                              # Utilise prettySwitch à la place de toggleState
                              column(width = 2, align = "right",
                                     prettySwitch('toggle', 'show Pairs Plot', value = FALSE)
                              )
                            ),
                            # Utilise une condition pour afficher le contenu en fonction de l'état du toggle switch
                            # Utilise une condition pour afficher le contenu en fonction de l'état du toggle switch
                            conditionalPanel(
                              condition = "input.toggle == true",
                              fluidRow(
                                column(width = 12,
                                       wellPanel(
                                         h4('Pairs Plot (seulement les variables avec une variance non nulle)'),
                                         plotOutput('expPairsPlot', width='100%', height='600px')
                                       )
                                )
                              )
                            ),
                            
                            conditionalPanel(
                              condition = "input.toggle == false",
                              fluidRow(
                                column(width = 12,
                                       wellPanel(
                                         h4('One and Two Variable Plot'),
                                         plotOutput('expSinglePlot'),
                                         h4('Metric results'),
                                         verbatimTextOutput("metricResults")
                                       )
                                )
                              )
                            )
                            
                            
                          )
                          
                        )
               ),
               # build model
               tabPanel('4. Build Prediction Model',
                        sidebarLayout(
                          sidebarPanel(
                            selectInput('preProcessMethods', 'Select data preprocessing method(s)',
                                        choices=c(
                                          'Center Data' = 'center', 
                                          'Scale Data' = 'scale', 
                                          'Box Cox Transform Data' = 'BoxCox',
                                          'Yeo-Johnson Transform Data' = 'YeoJohnson',
                                          'Principle Component Analysis (95% variance)' = 'pca'
                                        ),
                                        selected='center', 
                                        multiple=TRUE
                            ),
                            
                            
                            uiOutput('featureSelectInput'),
                            sliderInput("fracTrain", label = h4("Train Split %"), min=10, max=100, value=75, step=10),
                            br(),
                            radioButtons('mltype', "Choose the type of the task:",
                                         choices = c( "Classification"="clf","Regression"="reg"), 
                                         selected = "clf"),
                            uiOutput('machAlgorithm'),
                            # Button for downloading the model
                            downloadButton("downloadModel", "Save Model"),
                            
                            
                            
                          ),
                          mainPanel(
                            h4('Training / Test Split'),
                            verbatimTextOutput('cntTrain'),
                            verbatimTextOutput('cntTest'),
                            h4('Final model fit'),
                            verbatimTextOutput('finalModel'),
                            h4('Feature Importance'),
                            verbatimTextOutput('featureImportance')
                          )
                        )
               ),
               
               # Evaluate model
               tabPanel('5. Model Evaluation',
                        fluidRow(
                          column(6,
                                 wellPanel(
                                   h4('Estimated In Sample Accuracy (within training data)'),
                                   verbatimTextOutput('inSampleAccuracy'),
                                   plotOutput("inSamplePlot"),
                                   conditionalPanel(
                                     condition = "output.residualsplottrain != NULL",
                                     plotOutput("residualsplottrain")  
                                   ),
                                 )
                          ),
                          column(6,
                                 wellPanel(
                                   h4('Estimated Out of Sample Accuracy (within verification data)'),
                                   verbatimTextOutput('outOfSampleAccuracy'),
                                   
                                   plotOutput("outOfSamplePlot"),
                                   conditionalPanel(
                                     condition = "output.residualsplottest != NULL",
                                     plotOutput("residualsplottest")  
                                   ),
                                   
                                   
                                   
                                 )
                          )
                        )
               )
    )
    
  )
)