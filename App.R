# 06-observeEvent
#
#
# Show Load Data button
# Show label for Data Loaded
# Show radio buttons for selecting variables to Plot
# Show graph based on the radio buttons selections
# Show radio buttons for formula building
# Show Formula based on the Radio buttons
# Show options for selecting from various Models : Rpart, RandomForest, glm
# Run model fit based on the model selection and using the formula created
# Show summary of the fit in panel
# Show button to run prediction on validation data using the selected fit
# Show slider to change cutoff for confusion matrix default : 0.5
# Show confusion matrix as the slider changes
#
# show result panel for final summary of the model fit and cutoff used.
#

library(shiny)
library(ggplot2)
library(rpart)
library(randomForest)
library(caret)
library(e1071)

ui <- fluidPage(

#   sliderInput(inputId = "num",
#               label = "Choose a number",
#               min = 1, max = 100, value = 25),
#   textOutput(outputId = "title"),
#   actionButton(inputId = "go",
#                label = "Print Value"),
#   plotOutput(outputId = "hist")
#


   h2("Analysis of Titanic Survivors"),
   hr(style="border:2px solid black"),
    inputPanel(
        verticalLayout(
        h4("Description : "),
        div(style="width:800px","Here we will try to analyse and develop a model to predict survivors from the Titanic Accident. This is a popular datascience competition at", HTML("<a href='https://www.kaggle.com/c/titanic' target='_blank'>https://www.kaggle.com/c/titanic</a>"), ". We have downloaded the dataset (train.csv) to our application. For the purpose of this project we'll be using the training dataset."),
        div(style="width:800px", "Step 1 : Lets look at the structure of the data. This is obtained using the function str()."),
        div(style="width:800px", "Step 2 : Now lets see relation between each of the variables. We'll use ggplot2 to visualize the data and relationship.")
    )),
   # actionButton(inputId = "btnLoad", label = "Load Training Data"),
   h4(textOutput(outputId = "lblLoaded"), style="color:green"),
   verbatimTextOutput(outputId = "txtSummary"),
   radioButtons(inputId = "rdohistx", choices = c("Age", "Sex", "Parch", "SibSp", "Fare", "Pclass", "Embarked"), label = "Options to Plot - X", selected = "Pclass", inline = TRUE),
   radioButtons(inputId = "rdohisty", choices = c("Age", "Sex", "Parch", "SibSp", "Fare", "Pclass", "Embarked"), label = "Options to Plot - Fill", selected = "Sex", inline = TRUE),
   plotOutput(outputId = "pltHist", width = 900, height = 400),

#    actionButton(inputId = "btnSplitData", label = "Create Train/Validation Data"),
#    h4(textOutput(outputId = "lblSplitStatus"), style="color:green"),

   #radioButtons(inputId = "rdoModels", choices = c("Rpart", "RandomForest"), label = "Models to run", selected = "Rpart", inline = TRUE),
  br(),
  h3("Model Creation and Evaluation"),
    inputPanel(
        verticalLayout(
        h4("Description : "),
        div(style="width:800px", "Here we'll build and analyse models using 2 classification algorithms."),
        div(style="width:800px", "1. Rpart : This is a decision tree algorithm. Our formula consists of outcome 'Survived' as factor variable and the 7 predictor variables as shown below. In order to fine tune the model, we'll utilize the control function and provide 2 params CP and MinSplit"),

        div(style="width:800px","2. RandomForest : We'll use exact same formula as Rpart. However here we have 3 control parameters, Mtry, NodeSize, Ntrees to fine tune and improve accuracy of the model."),
        div(style="width:800px","In order to evaluate the accuracy of the Model. We'll run the model over the train set to extract predictions and calculate the confusionMatrix.")
    )),
    tabsetPanel(style="border:1px",

     tabPanel(title = "RPart", border=1,

              h5("Formula :"),
              h4(verbatimTextOutput(outputId = "txtFormulaRP")),

              textInput(inputId = "txtCP", label = "ComplexityParam ", value = 0.05),
              textInput(inputId = "txtMinSplit", label = "Min Split ", value = 10),

              actionButton(inputId = "btnRunFitRP", label = "Run Model Fit"),
              verbatimTextOutput(outputId = "txtFitSummaryRP"),
              h4("Confusion Matrix"),
              verbatimTextOutput(outputId = "txtVarImpRP")
     ),
     tabPanel(title = "RandomForest",
              h5("Formula :"),
              verbatimTextOutput(outputId = "txtFormulaRF"),
              # textInput(inputId = "txtMtry", label = "x Variables Sampled at each Split ", value = 5),
              sliderInput(inputId = "sldMtry", label = "Variables Sampled at each Split (mtry)",min = 1,max = 7,value = 3),
              textInput(inputId = "txtNodeSize", label = "Node Size ", value = 10),
              textInput(inputId = "txtNtrees", label = "nTrees ", value = 500),
              actionButton(inputId = "btnRunFitRF", label = "Run Model Fit"),
              verbatimTextOutput(outputId = "txtFitSummaryRF"),
              h4("Confusion Matrix"),
              verbatimTextOutput(outputId = "txtVarImpRF")
     )
   ),
    inputPanel(
        verticalLayout(
        h4("Summary :"),
        div(style="width:800px", "The Confusion matrix provides insight into the model performance by showing the True Positives, False Positive, True Negatives, False Negatives. By fine Tuning the control parameters we can increase the accuracy. To avoid Overfitting, we can go a step further and split the train dataset into Train and Validation set.")
    )),
    hr(style="border:2px solid black")

)

server <- function(input, output) {


  # observe responds to the print button
  # but not the slider
  #
  dtrain <- read.csv("train.csv", header = TRUE, na.strings = c(" ", ""))
  output$lblLoaded <- renderText("Training data loaded")
  output$txtSummary <- renderPrint({paste(str(dtrain))})

  output$txtFormulaRP <-  renderText({paste( "rpart(formula = as.factor(Survived)~Age+Sex+Parch+SibSp+Pclass+Fare+Embarked,data = dtrain, control = rpart.control(minsplit = ",input$txtMinSplit ,", cp =  ", input$txtCP,"))" ,sep = "")})

  output$txtFormulaRF <- renderText({paste("randomForest(formula = as.factor(Survived)~Age+Sex+Parch+SibSp+Pclass+Fare+Embarked,data = dtrain, mtry = ",input$sldMtry , ", nodesize = ", input$txtNodeSize, ", ntree=", input$txtNtrees ,")", sep = "")})

   data <- eventReactive(input$btnLoad, {
    ##    print(as.numeric(input$num))
     read.csv("train.csv", header = TRUE, na.strings = c(" ", ""))
  })




output$pltHist <- renderPlot({
  #     hist(data()[,input$rdohistx])
 # ggplot(data = dtrain, aes(input$rdohistx), fill = factor(input$rdohisty)) + geom_bar()
  x <- input$rdohistx
  y <- input$rdohisty
  if (input$rdohistx=="Age")
  {x <- "round(Age/10)*10"}
  else if (input$rdohistx=="Fare")
  {x <- "round(Fare/100)"}
  if (input$rdohisty=="Age")
  {y <- "round(Age/10)"}
  else if (input$rdohisty=="Fare")
  {y <- "round(Fare/100)"}


  ggplot(data = dtrain, aes_string(x, fill = paste("factor(", y, ")"))) + geom_bar()
})

  fitRP <- reactiveValues(data = "This is Summary for The RPart Model")
  fitRF <- reactiveValues(data = "This is Summary for The RandomForect Model")


  observeEvent(input$btnRunFitRP, {
    fitRP$data <- rpart(formula = as.factor(Survived)~Age+Sex+Parch+SibSp+Pclass+Fare+Embarked,data = dtrain, control = rpart.control(minsplit = as.numeric(input$txtMinSplit) , cp =  as.numeric(input$txtCP)))

    output$txtFitSummaryRP <- renderPrint({print(fitRP$data, digits = 2, cp = as.numeric(input$txtCP))})
    output$txtVarImpRP <- renderPrint({confusionMatrix(predict(fitRP$data, dtrain, "class"), dtrain$Survived)})
    #print(fitRP$data, digits = 2, cp = as.integer(input$txtCP))
  })


  observeEvent(input$btnRunFitRF, {
    fitRF$data <- randomForest(as.factor(Survived)~Age+Sex+Parch+SibSp+Pclass+Fare+Embarked, data = dtrain, mtry = as.integer(input$sldMtry) , nodesize = as.integer(input$txtNodeSize), ntree=as.integer(input$txtNtrees), importance = TRUE, na.action = na.omit )

   output$txtFitSummaryRF <- renderPrint({fitRF$data})
   output$txtVarImpRF <- renderPrint({confusionMatrix(predict(fitRF$data, dtrain, "class"), dtrain$Survived)})

  })

}

shinyApp(ui = ui, server = server)

