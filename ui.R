if (!require(shiny)){
      install.packages("shiny")
      library(shiny)
}

# Define UI for application 
shinyUI(fluidPage(
      
      # Application title
      titlePanel("Population Structure Evoulution!"),
      
      # Sidebar with a slider input for base year and comparison year
      sidebarLayout(
            
            sidebarPanel(
                  h5("Evolution of population structures"),
                  p("Compares current year with selected base year and comparison year"),
                  hr(),
                  em("Please be patient when you change country and we have to read data from the web, which might take SEVERAL minutes -- and I really mean SEVERAL minutes"),
                  br(),
                  em("... but after that it works pretty well"),
                  hr(),
                  selectInput("country", 
                              label = "Select country",
                              choices = list("## setting up list",
                                             "Benin",
                                             "Burkina Faso",
                                             "Burundi",
                                             "Zimbabwe"
                              ), 
                              selected = 5),
                  
                  sliderInput("years",
                              "Which years?",
                              min = 1950,
                              max = 2100,
                              value = c(1950,2100),
                              step=1,
                              format="####")
                  
            ),
            
            # Show the plot and summary table
            mainPanel(
                  plotOutput("popPyramid"),
                  plotOutput("popAges"),
                  hr(),
                  hr(),
                  h4("Population structure key facts:"),
                  tableOutput("popSummary")
            )
      )
))
