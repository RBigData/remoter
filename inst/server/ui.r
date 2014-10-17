library(shiny)
library(shinyAce)


themes <- getAceThemes()

shinyUI(
  bootstrapPage(
    div(class="container-fluid",
      h2("Theme: "),
      selectInput("theme", "", choices=themes, selected="textmate")
    ),
    div(class="container-fluid",
      div(class="row-fluid",
        div(class="span6",
          h2("Source Code"),  
          aceEditor("code", mode="r", value="x <- allreduce(1)
x"),
            actionButton("eval", "Evaluate")
        ),
        div(class="span6",
          h2("Output"),
          verbatimTextOutput("output")
        )
      )
    )
  )
)
