library(shiny)
library(DT)
library(shinyWidgets)
library(pROC)
library(plotly)
library(shinyalert)




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
# useShinyalert()

shinyUI(
  
  fluidPage(
    # useShinyalert(),
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
             h4("Press the button below for Removing missing data :", style = 'color: black;'),
             actionButton("preprocess", "Remove missing data"),
             br(), br(),
             h4("Press the button below for Oridinal-Encoding :", style = 'color: black;'),
             
             actionButton("categoricalconversion", "Oridinal-Encoding Categorical Features "),
             br(), br(),
             
             # Numeric input for specifying the number of samples after oversampling
             numericInput("numSamples", 
                          label = "Oversamplification / Undersamplification x Times:", 
                          value = 1,  # Default value, adjust as necessary
                          min = 1),     # Minimum value, adjust as necessary
             
             # Oversampling button
             actionButton("oversampleButton", "Oversample Minority Class"),
             actionButton("undersampleButton", "Undersample Majority Class "),
             
             
             br(), br(),
             
             # # Undersampling method and button (added beside the oversample button)
             # fluidRow(
             #   column(6,
             #          selectInput("undersampleMethod", 
             #                      label = "Select Undersampling Method", 
             #                      choices = list("Random Undersampling" = "Random", 
             #                                     "Tomek Links" = "Tomek"),
             #                      selected = "Random")
             #   ),
             #   column(6,
             #          actionButton("undersampleButton", 
             #                       label = "Perform Undersampling")
             #   )
             # ),
             # 
             # br(), br(),
             
             # Output for null values percentage (no change here)
             DTOutput('NullPercentageOut')
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
                            sliderInput("fracTrain", label = h4("Train Split %"), min=10, max=100, value=50, step=10),
                            br(),
                            radioButtons('mltype', "Choose the type of the task:",
                                         choices = c( "Classification"="clf","Regression"="reg"), 
                                         selected = "clf"),
                            uiOutput('machAlgorithm'),
                            verbatimTextOutput("grid_params") ,
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
               ),
               
                # In the '6. Rapport de Projet' tabPanel
                tabPanel('6. Analyse exploratoire',
                         fluidPage(
                           titlePanel(p("Analyse exploratoire des données", style = "color:#3474A7")),
                           hr(),  # 
                           # Display the section title
                           # h4(strong("1. Analyse exploratoire des données pour ce TP")),
                           
                           # Display the data attributes information
                           h3(strong("1. Dimensions, valeurs manquantes et attributs constants", style = "font-weight: bold; color:#3474A7")),
                           
                           tags$div(
                             HTML("<strong>Credit Fraud:</strong><br>
                           Dimensions: 284807<br>
                           Valeurs manquantes: 0<br>
                           Attributs constants: Aucun")
                           ),
                           
                           hr(),  # Horizontal line for separation
                           
                           tags$div(
                             HTML("<strong>Bank Marketing:</strong><br>
                           Dimensions: 411882<br>
                           Valeurs manquantes: 0<br>
                           Attributs constants: Aucun")
                           ),
                           
                           hr(),  # Horizontal line for separation
                           
                           tags$div(
                             HTML("<strong>Employee Attrition:</strong><br>
                           Dimensions: 441029<br>
                           Valeurs manquantes: 111<br>
                           Attributs constants: 'EmployeeCount', 'Over18', 'StandardHours' (à supprimer quand vous lancez dans l'app)")
                           ),
                           
                           hr(),  # Horizontal line for separation
                           
                           # Adding the graphs for churn proportions
                           h3("2. Proportion d'individus ayant churné", style = "font-weight: bold; color:#3474A7"),
                           
                           fluidRow(
                             # Deux premières images côte à côte
                             column(3, 
                                    h5(strong("1. Credit Fraud")),
                                    img(src="credit_fraud_churn.png", height = '100%', width = '100%')
                             ),
                             column(3, 
                                    h5(strong("2. Bank Marketing")),
                                    img(src="bank_marketing_churn.png", height = '100%', width = '100%')
                             ),
                             column(3, align = "center",
                                    h5(strong("3. Employee Attrition")),
                                    img(src="employee_attrition_churn.png", height = '100%', width = '100%')
                             )
                             
                           ),
                           
                          
                         ),
                         
                         h3("3. Analyse des variables catégorielles", style = "font-weight: bold; color:#3474A7"),
                         
                         # Ajouter une explication ou un sous-titre ici si nécessaire
                         br(),
                         h5(strong("Bank-Marketing Dataset")),
                         
                         
                         fluidRow(
                           # Deux premières images côte à côte
                           column(3, 
                                  img(src="bank_categorical_default.png", height = '100%', width = '100%')
                           ),
                           column(3, 
                                  img(src="bank_categorical_education.png", height = '100%', width = '100%')
                           ),
                           column(3, 
                                  img(src="bank_categorical_job.png", height = '100%', width = '100%')
                           ),
                           column(3, 
                                  img(src="bank_categorical_poutcome.png", height = '100%', width = '100%')
                           )
                         ),
                         
                         br(),
                         
                         
                         h5(strong("Employee attrition Dataset")),
                         
                         
                         fluidRow(
                           # Deux premières images côte à côte
                           column(3, 
                                  img(src="employee_business.png", height = '100%', width = '100%')
                           ),
                           column(3, 
                                  img(src="employee_departement.png", height = '100%', width = '100%')
                           ),
                           column(3, 
                                  img(src="employee_education.png", height = '100%', width = '100%')
                           ),
                           column(3, 
                                  img(src="employee_jobe.png", height = '100%', width = '100%')
                           )
                         ),
                         
                         br(),
                         
                         
                         
                         h3("4. Analyse des variables numériques", style = "font-weight: bold; color:#3474A7"),
                         
                         br(),
                         h5(strong("Bank-Marketing Dataset")),
                         
                         
                         fluidRow(
                           # Deux premières images côte à côte
                           column(3, 
                                  img(src="marketing_histo1.png", height = '100%', width = '100%')
                           ),
                           column(3, 
                                  img(src="marketing_histo2.png", height = '100%', width = '100%')
                           ),
                           column(3, 
                                  img(src="marketing_histo3.png", height = '100%', width = '100%')
                           ),
                           column(3, 
                                  img(src="marketing_histo4.png", height = '100%', width = '100%')
                           )
                         ),
                         
                         br(),
                         
                         h5(strong("Credit Fraud Dataset")),
                         
                         
                         fluidRow(
                           # Deux premières images côte à côte
                           column(3, 
                                  img(src="fraud4.png", height = '100%', width = '100%')
                           ),
                           column(3, 
                                  img(src="fraud_1.png", height = '100%', width = '100%')
                           ),
                           column(3, 
                                  img(src="fraud2.png", height = '100%', width = '100%')
                           ),
                           column(3, 
                                  img(src="fraud3.png", height = '100%', width = '100%')
                           )
                         ),
                         
                         br(),
                         
                         
                         h5(strong("Employee attrition Dataset")),
                         
                         
                         fluidRow(
                           # Deux premières images côte à côte
                           column(3, 
                                  img(src="employee_histo1.png", height = '100%', width = '100%')
                           ),
                           column(3, 
                                  img(src="employee_histo2.png", height = '100%', width = '100%')
                           ),
                           column(3, 
                                  img(src="employee_histo3.png", height = '100%', width = '100%')
                           ),
                           column(3, 
                                  img(src="employee_histo4.png", height = '100%', width = '100%')
                           )
                         ),
                         
                         br(),
                         
                         
                         
                         h3("5. Matrice de corrélation des attributs", style = "font-weight: bold; color:#3474A7"),
                         
                         # Ajouter une explication ou un sous-titre ici si nécessaire
                         br(),
                         
                         fluidRow(
                           # Deux premières images côte à côte
                           column(6, align = "center",
                                  h5(strong("1. Credit Fraud")),
                                  img(src="4_credit_fraud.png", height = '100%', width = '100%')
                           ),
                           column(5, align = "center",
                                  h5(strong("2. Bank Marketing")),
                                  img(src="4_bank_mark.png", height = '100%', width = '100%')
                           ),
                           column(6, align = "center",
                                  h5(strong("3. Employee Attrition")),
                                  img(src="4_employee_attrition.png", height = '100%', width = '100%')
                           )
                           
                         ),
                         
                         br(),
                     
                         

                         h3("5. Conclusions", style = "font-weight: bold; color:#3474A7"),
                        
                         h4(strong("5.1. Employee attrition Dataset"), style = "color:#3474A7"),
                         

                         tags$div(
                           HTML("
  <p><strong>1. Valeurs manquantes :</strong> Le jeu de données contient très peu de valeurs manquantes, qui peuvent être supprimées sans affecter l'analyse globale.</p>
  
  <p><strong>2. Informations sur l'attrition :</strong></p>
  <ul>
    <li><strong>Rôle ayant le taux d'attrition le plus élevé :</strong> Les commerciaux (Sales Executive) ont le taux d'attrition le plus élevé.</li>
    <li><strong>Département avec le taux d'attrition le plus élevé :</strong> Le département Recherche et Développement a le taux d'attrition le plus élevé.</li>
  </ul>
  
  <p><strong>3. Analyse statistique :</strong> Nous avons réalisé des tests du Chi² pour évaluer les associations entre les variables et l'attrition :</p>
  <ul>
    <li><strong>Aucune association significative :</strong> Sexe, Niveau d'éducation.</li>
    <li><strong>Association significative :</strong> Statut matrimonial, Revenu mensuel, Satisfaction professionnelle, Satisfaction de l'environnement de travail, Équilibre vie privée-vie professionnelle, et Âge.</li>
  </ul>
  
  <p><strong>4. Déséquilibre des données :</strong> Le jeu de données est déséquilibré, ce qui signifie que la précision peut ne pas être la meilleure mesure pour évaluer les performances. À la place, les scores AUC et F1 fournissent une meilleure compréhension de l'efficacité du modèle.</p>

  <p><strong>5. Performance du modèle :</strong></p>
  <ul>
    <li><strong>Score F1 initial :</strong> 0.214.</li>
    <li><strong>Score F1 après sous-échantillonnage :</strong> 0.192. Le sous-échantillonnage a réduit le score F1 et le rappel, ce qui le rend moins efficace.</li>
    <li><strong>Score F1 après sur-échantillonnage avec SMOTE :</strong> 0.4, montrant une amélioration notable des performances.</li>
  </ul>

  <p><strong>6. Importance des caractéristiques :</strong> Les caractéristiques clés à prendre en compte pour réduire l'attrition incluent :</p>
  <ul>
    <li>Satisfaction de l'environnement de travail</li>
    <li>Revenu mensuel</li>
    <li>Nombre total d'années de travail</li>
    <li>Années dans l'entreprise</li>
    <li>Distance du domicile</li>
    <li>Années depuis la dernière promotion</li>
  </ul>
  
  <p>Ces caractéristiques sont essentielles pour cibler des stratégies de rétention des employés et réduire les taux d'attrition dans l'entreprise.</p>
  ")
                         ),
                         
                         
          h4(strong("5.2. Credit Fraud Dataset"), style = "color:#3474A7"),
          # ajouter 
          
          h4(strong("5.3. Bank Marketing Dataset"), style = "color:#3474A7"),
          
                         
          
                         
                       
                       

                )









    )
    
  )
)