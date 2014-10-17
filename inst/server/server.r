library(shiny)
library(shinyAce)



shinyServer(function(input, output, session) {
  output$output <- renderPrint({
    
    observe({
      updateAceEditor(session, "code", theme=input$theme, mode="r")
    })
    
    changed <- buttonfix(session, input$eval)
    
    if (changed$eval)
    {
      code <- input$code
      bcast(code, rank.source=0L)
      
      ret <- eval(parse(text=code), envir=.GlobalEnv)
      
      return(isolate(ret))
    }
  })
})



