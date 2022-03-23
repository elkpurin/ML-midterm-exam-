server<-function(input, output) {
  
  load("dat_model2.RData")
  
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
