library(shiny)
library(shinythemes)
library(tidyverse)

ui<-fluidPage(
  
  theme = shinytheme("superhero"),
  
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
                  min = 0, max = 20, value = 5)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("my recomendation",
                 h5("F,D,D+ grade are not recommend to go on with your class & other grade are acceptable"),
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

