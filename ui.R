require(shiny)

reactiveSvg <- function (outputId) 
{
  HTML(paste("<div id=\"", outputId, "\" class=\"shiny-network-output\"><svg /></div>", sep=""))
}

shinyUI(
  pageWithSidebar(
    headerPanel("JZS t test demonstration"),
    sidebarPanel(
      HTML("Prior is blue line; posterior is red line<hr/>"),
      sliderInput("t", "t statistic:", 0, min=-5, max=5,step=.05),
      sliderInput("N", "Sample size:", 20, min=2, max=100,step=1),
      HTML("<hr/>Prior:"),
      
      sliderInput("r", "prior scale:", 0, min=log2(1/4), max=log2(4),step=.05),
      textOutput("rText"),
      HTML("<hr/>Output:<br/>"),
       
      textOutput("nullbfText"),
      HTML("or (equivalently),"),
      textOutput("altbfText")
      
    ),
    mainPanel(
      includeHTML("svgfiller.js"),
      reactiveSvg(outputId = "svg.grid")
    )
  )
)
