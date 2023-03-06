
library(shiny)
library(tidyverse)

admis_scores <- read_delim("df.csv") %>% 
  mutate(newRegion = factor(Region)) %>% 
  rename(Admission_Rate = "Admission Rate")
head(admis_scores)
region <- unique(admis_scores$Region)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("US Colleges/University Admissions SAT/ACT scores 2020-2021"),

    # Sidebar with a slider input for number of bins 
    mainPanel(
        tabsetPanel(
            tabPanel("About",
                     mainPanel(
                       hr("This app displays information about the 75th percentile SAT & ACT scores, admissions rate, and the location of 1,386 institutions in the U.S from 2020-2021"),
                       hr("The data contains 1386 institutions and 9 variables."),
                       br("Here is a (random) sample of data:"),
                       tableOutput("sampleTable"))),
            tabPanel("SAT Plot",
                     sidebarPanel(
                       sliderInput("SAT_Range",
                                   "What range of SAT plot:",
                                   min = 0,
                                   max = 800,
                                   value = c(0, 800)
                      ),
                      checkboxGroupInput("Region",
                                         "Choose which regions to plot:",
                                         choices = region,
                                         selected = region
                       )
                     ),
                     mainPanel(
                       plotOutput("satploteng"),
                       plotOutput("satplotmath")
                     )
              
            ),
            tabPanel("ACT Plot",
                     sidebarPanel(
                       sliderInput("ACT_Range",
                                   "What range of ACT plot: ",
                                   min = 0,
                                   max = 36,
                                   value = c(0, 36)
                      ), checkboxGroupInput("Region",
                                            "Choose which regions to plot:",
                                            choiceNames = region,
                                            choiceValues = region
                      )
                      ), 
                     mainPanel(
                       plotOutput("actploteng"),
                       plotOutput("actplotmath")
                     )
            ),
            tabPanel("Table",
                     sidebarPanel(
                       hr("This panel displays the average and range of each region or state for each ACT and SAT subject."),
                       hr(p(strong("SATVR75:"), "75th percentile SAT English")),
                       hr(p(strong("SATMT75:"), "75th percentile SAT Math")),
                       hr(p(strong("ACTEN75:"), "75th percentile ACT English")),
                       hr(p(strong("ACTMT75:"), "75th percentile ACT Math")),
                       hr("\n"),
                       selectInput("Subject", "Select test type and subject:",
                                   choices = c("ACTEN75", "ACTMT75", "SATVR75", "SATMT75")
                     )
                     )),
        )))
                    

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$sampleTable <- renderTable({
    admis_scores %>% 
    sample_n(5)
  })

    output$satploteng <- renderPlot({
      admis_scores %>% 
        filter(newRegion %in% input$Region) %>% 
        filter(SATVR75 >= input$SAT_Range[1],
               SATVR75 <= input$SAT_Range[2]) %>% 
        ggplot(aes(SATVR75, Admission_Rate, col = newRegion)) +
        labs(title = "75th Percentile SAT English scores and Admission Rates",
             x = "75th Percentile SAT English score", y = "Admission Rate") +
        geom_point()
    })
    
    output$satplotmath <- renderPlot({
      admis_scores %>% 
        filter(newRegion %in% input$Region) %>% 
        filter(SATMT75 >= input$SAT_Range[1],
               SATMT75 <= input$SAT_Range[2]) %>% 
        ggplot(aes(SATVR75, Admission_Rate, col = newRegion)) +
        labs(title = "75th Percentile SAT Math Score and Admissions Rate",
             x = "75th Percentile SAT Math Score", y = "Admission Rate") +
        geom_point()
    })
    
    output$actploteng <- renderPlot({
      admis_scores %>% 
        filter(newRegion %in% input$Region) %>% 
        filter(ACTEN75 >= input$ACT_Range[1],
               ACTEN75 <= input$ACT_Range[2]) %>% 
        ggplot(aes(ACTEN75, Admission_Rate, col = newRegion)) +
        labs(title = "75th Percentile ACT English Score and Admission Rates",
             x = "75th Percentile ACT English Score", y = "Admission Rate") +
        geom_point()
    })
    
    output$actplotmath <- renderPlot({
      admis_scores %>% 
        filter(newRegion %in% input$Region) %>% 
        filter(ACTMT75 >= input$ACT_Range[1],
               ACTMT75 <= input$ACT_Range[2]) %>% 
        ggplot(aes(ACTMT75, Admission_Rate, col = newRegion)) +
        labs(title = "75th Percentile ACT Math Score and Admission Rates",
             x = "75th Percentile ACT Math Score", y = "Admission Rate") +
        geom_point()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
