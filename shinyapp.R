library(shiny)
library(shinythemes)

load("dat_model2.RData")

ui<-fluidPage(
  
  titlePanel("Predict your own grade!"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("mid25", "fill your midterm score:", 
                  min = 0, max = 25, value = 15),
      sliderInput("att5", "rate how often did you attend to the class ? (0 out of 5)", 
                  min = 0, max = 5, value = 3),
      sliderInput("prac5", "rate score that you should get from your home work (0 out of 5)",
                  min = 0, max = 5, value = 3),
      sliderInput("crit10", "do you think how many score do you get on critical assignment?",
                  min = 0, max = 10, value = 5), 
      sliderInput("proposal10", "do you think how many score do you get on proposal assignment?",
                  min = 0, max = 10, value = 5),
      sliderInput("proj20", "do you think how many score do you get on project assignment?",
                  min = 0, max = 10, value = 5)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("my recomendation",
                 h2("my recomendation"), 
                 verbatimTextOutput("pred"),
                 plotOutput("midplot"),
                 h4("your score compare to avarage midterm score")),
        tabPanel("predict final score",
                 verbatimTextOutput("pred_final"))
      )
    )
  )
)

server<-function(input, output) {
  
  df<-reactive({
    
    data.frame(
      mid25 = as.numeric(as.character(input$mid25)),
      att5 = as.numeric(as.character(input$att5)),
      prac5 = as.numeric(as.character(input$prac5)), 
      crit10 = as.numeric(as.character(input$crit10)),
      proposal10 = as.numeric(as.character(input$proposal10)),
      proj20 = as.numeric(as.character(input$proj20)),
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
    geom_vline(data = df(), xintercept = mean(df()$mid25), linetype = 2 ,size = 1)
)

output$pred<-renderText(
  
  ifelse(pred.reac()==0, "THE SHOW MUST GO ON!!!!","You should go to the register center")
  
       )

output$pred_final<-renderText(
  print(paste0("your final grade is about ", pred_lm.reac() ," point", "(with 69% accuracy)"))
)

}

shinyApp(ui = ui, server = server)
