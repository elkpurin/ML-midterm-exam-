library(shiny)
library(shinythemes)
library(devtools)
devtools::install_github("hadley/emo")
library(naivebayes)
library(scales)

load("dat_model2.RData")

ui<-fluidPage(
  
  theme = shinytheme("superhero"),
  
  titlePanel("Predict your own grade!"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("mid25", "fill your midterm score:", value = 10, 
                  min = 0, max = 25, step = 1),
      submitButton("Update", icon("refresh")),
      h4("you can estimate yourself by sliding the question below to improve the prediction!"),
      sliderInput("att5", "rate how often did you attend to the class ? (0 out of 5)", 
                  min = 0, max = 5, value = 3),
      sliderInput("prac5", "rate score that you should get from your home work (0 out of 5)",
                  min = 0, max = 5, value = 3),
      sliderInput("crit10", "do you think how many score do you get on critical assignment?",
                  min = 0, max = 10, value = 5), 
      sliderInput("proposal10", "do you think how many score do you get on proposal assignment?",
                  min = 0, max = 10, value = 5),
      sliderInput("proj20", "do you think how many score do you get on project assignment?",
                  min = 0, max = 20, value = 5),
      submitButton("Update", icon("refresh"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Suggestion",
                 h6(paste0("for this app: F,D,D+ grade are not recommend to keep going on with your class (you are under the risk) but other grade are acceptable", 
                           emo::ji("+1"))),
                 h2("your suggestion"), 
                 verbatimTextOutput("pred"),
                 plotOutput("midplot")),
        tabPanel("Predict final score",
                 h6(paste0("for predict your final score, this app is estimated by your midterm score")),
                 verbatimTextOutput("pred_final")),
        tabPanel("Predict your grade",
                 h6(paste0("this is your grade propabilities (%) based on your final score")),
                 tableOutput("grade"))
      )
    )
  )
)



server<-function(input, output) {
  
  df<-reactive({
    data.frame(
      mid25 = input$mid25,
      att5 = input$att5,
      prac5 = input$prac5, 
      crit10 = input$crit10,
      proposal10 = input$proposal10,
      proj20 = input$proj20,
      stringsAsFactors = FALSE)
  })

    pred.reac<-reactive({
      as.character(predict(fit.rf, newdata = df(), type = "raw"))
    })
    
    pred_lm.reac<-reactive({
      as.character(round(predict(fit.lm, newdata = df())))
    })
    
output$midplot<-renderPlot(
  ggplot() + geom_histogram(data = score_train, aes(x=mid25), bins = 20) + 
    geom_vline(data = score_train, xintercept = mean(score_train$mid25), linetype = 2, size = 1, col = "blue") +
    geom_text(aes(x=mean(score_train$mid25), label="\naverage", y=8), colour="blue", angle=0, text=element_text(size=11)) +
    geom_vline(data = df(), xintercept = mean(df()$mid25), linetype = 2 ,size = 1) +
    geom_text(aes(x=mean(df()$mid25), label="\nyour score", y=8), colour="black", angle=0, text=element_text(size=11)) +
    labs(title = "your midterm score compare to the avarage", subtitle = "midterm score came from the past few year database") +
    xlab("midterm score") + ylab("frequency")
)

output$pred<-renderText(
  ifelse(pred.reac()==0, "THE SHOW MUST GO ON!! keep going! you do the great job","You should go to the register center and get the W. ASAP")
)

output$pred_final<-renderText(
  print(paste0("your final grade is about ", pred_lm.reac() ," out of 25", " point"," (with 69% accuracy)"))
)

 grade<-reactive({
  predict(fit.nb, newdata = df(), type = "prob")
  })

output$grade<-renderTable(as.data.frame(round(grade(), digits = 3)*100))

}

shinyApp(ui = ui, server = server)
