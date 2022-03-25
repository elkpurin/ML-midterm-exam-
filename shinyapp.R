library(shiny)
library(shinythemes)
library(devtools)
devtools::install_github("hadley/emo")
library(naivebayes)
library(scales)

load("dat_model2.RData")

ui<-fluidPage(
  
  theme = shinytheme("superhero"),
  
  titlePanel("Suggestion & Prediction for Research Subjects"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("mid25", "fill your midterm score:", value = NA, 
                  min = 0, max = 25, step = 1),
      submitButton("Update", icon("refresh")),
      h4("Improving the suggested prediction! \nsliding the question below to evaluate yourself and then click Update"),
      sliderInput("att5", "How often did you attend to the class?", 
                  min = 0, max = 5, value = 0),
      sliderInput("prac5", "Do you think how many score you should get from your homeworks (0 out of 5)",
                  min = 0, max = 5, value = 0),
      sliderInput("crit10", "Do you think how many score do you get on critical assignment?",
                  min = 0, max = 10, value = 0), 
      sliderInput("proposal10", "Do you think how many score do you get on proposal assignment?",
                  min = 0, max = 10, value = 0),
      sliderInput("proj20", "Do you think how many score do you get on project assignment?",
                  min = 0, max = 20, value = 0),
      submitButton("Update", icon("refresh"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Suggestion",
                 h6(paste0("Noted: F,D,D+ grade are not recommend to keep going on with your class (you are under the risk) but other grade are acceptable", 
                           emo::ji("+1"))),
                 h2("your suggestion"), 
                 verbatimTextOutput("pred"),
                 plotOutput("midplot")),
        tabPanel("Predict your final score",
                 h6(paste0("Noted: your final score is predited by your midterm score. Sliding other score does not effect the prediction")),
                 verbatimTextOutput("pred_final"),
                 plotOutput("final.plot")),
        tabPanel("Predict your grade",
                 h6(paste0("Noted: this is your grade propabilities (%) based on your midterm score (accuracy = 63.7%)")),
                 tableOutput("grade"),
                 plotOutput("nb.plot"))
      )
    )
  )
)



server<-function(input, output) {
  
  df<-reactive({
    
    validate(
      need(input$mid25, "Please fill your midterm score"),
    )
    
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
    
output$midplot<-renderPlot({
  req(input$mid25)
  ggplot() + geom_histogram(data = score_train, aes(x=mid25), bins = 20) + 
    geom_vline(data = score_train, xintercept = mean(score_train$mid25), linetype = 2, size = 1, col = "blue") +
    geom_text(aes(x=mean(score_train$mid25), label="\naverage", y=8), colour="blue", angle=0, text=element_text(size=11)) +
    geom_vline(data = df(), xintercept = mean(df()$mid25), linetype = 2 ,size = 1) +
    geom_text(aes(x=mean(df()$mid25), label="\nyour score", y=8), colour="black", angle=0, text=element_text(size=11)) +
    labs(title = "your midterm score compare to the avarage", subtitle = "average midterm score from the past few year") +
    xlab("midterm score") + ylab("frequency")
})

output$pred<-renderText(
  ifelse(pred.reac()==0, "THE SHOW MUST GO ON!! keep going! you do the great job","You should go to the register center and get the W. ASAP")
)

output$pred_final<-renderText(
  print(paste0("your final grade is about ", pred_lm.reac() ," out of 25", " point"," (with 69% accuracy)"))
)

df2<-reactive({
  data.frame(
    final25 = as.numeric(pred.reac()),
    stringsAsFactors = FALSE)
})

output$final.plot<-renderPlot(
  ggplot() + geom_histogram(data = final_train, aes(x=final25), bins = 20) + 
    geom_vline(data = final_train, xintercept = mean(final_train$final25), linetype = 2, size = 1, col = "blue") +
    geom_text(aes(x=mean(final_train$final25), label="\naverage", y=8), colour="blue", angle=0, text=element_text(size=11)) +
    geom_vline(xintercept = as.numeric(pred_lm.reac()), linetype = 2 ,size = 1) +
    geom_text(aes(x=as.numeric(pred_lm.reac()), label="\nyour score", y=8), colour="black", angle=0, text=element_text(size=11)) +
    labs(title = "your predicted final score compare to the avarage", subtitle = "average final score from the past few year") +
    xlab("final score") + ylab("frequency")
)

 grade<-reactive({
  predict(fit.nb, newdata = df(), type = "prob")
  })

output$grade<-renderTable(as.data.frame(round(grade(), digits = 3)*100))

  t.grade<-reactive({
    as.data.frame(t(grade()))
  })
  
  nb.df<-reactive({
    data.frame(
      grade = c("A", "B", "B+", "C", "C+", "D", "D+"),
      value = t.grade()[,1]*100
    )
  })
  
  nb.df.plot<-reactive({
    nb.df()%>%mutate(ypos = cumsum(value)-0.5*value)
  })

  nb.plot<-reactive({
    ggplot(nb.df.plot(), aes(x="", y=value, fill=grade)) +
      geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0) +
      theme_bw() +
      geom_text(aes(label = paste0(round(value), "%")),
                position = position_stack(vjust = 0.5), size=5, 
                color = "white", size=6) +
      scale_fill_brewer(palette="Set2") +
      labs(title = "Probabilities of expected grade") +
      xlab("") + ylab("")
  })
  
output$nb.plot<-renderPlot(
    print(nb.plot())
)

}

shinyApp(ui = ui, server = server)
