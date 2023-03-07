
library(shiny)
library(tidyverse)

score <- read_delim("df.csv") %>%
  mutate(newRegion = factor(Region)) %>%
  rename(Admission_Rate = "Admission Rate") 
head(score)
region <- unique(score$Region)
states <- unique(score$State)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("US University & College Admissions and SAT & ACT Scores"),
  
  mainPanel(
    tabsetPanel(
      # Introduction tab with brief information about the purpose of the app and dataset 
      tabPanel("Introduction",
               mainPanel(
                 # Add image to intro page
                 imageOutput("Image"),
                 h2("Project Overview"),
                 p("Has COVID-19 impacted the importance of SAT scores within
                      college admissions in the U.S? Due to quarantine lockdown,
                      many students across the country were forced to halt their
                      in person classes and exams. This created a lot of stress 
                      and anxiety within students who were trying to apply for 
                      colleges/universities. Although the SAT used to be a huge 
                      part of the admission process, many universities went 
                      test-optional in order to “reduce the demand on students”,
                      according to collegeboard. This report provides the 
                      relationship between math/verbal SAT and ACT scores and the
                      admission rate in colleges and universities in the U.S.
                      Specifically, we are trying to show how some schools are 
                      moving away from SAT scores and how it has less prevalence
                      than it used to."),
                 h3("Audience"),
                 p("Our main target audience would be students who are
                      planning to apply for universities/colleges. This
                      information allows them to gain more knowledge on the
                      correlation between SAT/ACT scores and admission rate,
                      in order to help them understand how important these
                      standardized tests may be. Another target audience are
                      colleges and universities. They may want to learn more
                      about the number of applicants received, enrollment, and
                      the SAT/ACT scores of their potential students, which can
                      influence their admissions decisions."),
                 h3("Data Source"),
                 p("The data that the report refers to is the",
                   a("Kaggle US College & University Admissions 2020-2021", href='https://www.kaggle.com/datasets/jfschultz/us-college-admisions-2021-rates-and-test-scores'),
                   "database. This
                   dataset provides the broad overview of U.S admissions for
                   each college/university per state."),
                 h3("Plots & Table"),
                 p("The four plots that we have created intend to show the
                      relationship of the math/verbal SAT and ACT scores and
                      admissions rate by region. The table intends to compare
                      the two plots."))
      ),
      # Scatter plots showing SAT verbal and math score for each region
      tabPanel("SAT Plot",
               h3("Admissions Rate with SAT Math and English Scores"),
               sidebarPanel(
                 # Can change the score range users want to view on the scatter plot
                 sliderInput("SAT_Range",
                             "What range of SAT plot: ",
                             min = 0,
                             max = 800,
                             value = c(0,800)
                 ),
                 # Can choose the region users want to plot on scatter plot
                 checkboxGroupInput("Regionsat",
                                    "Choose which region(s) to plot:",
                                    choices = region, 
                                    selected = region
                                    
                 )
               ),
               mainPanel(
                 #scatter plot 
                 plotOutput("satploteng"),
                 plotOutput("satplotmath")
               )
               
      ),
      tabPanel("ACT Plot",
               h3("Admissions Rate with ACT Math and English Scores"),
               sidebarPanel(
                 # Can change the score range users want to view on the scatter plot
                 sliderInput("ACT_Range",
                             "What range of ACT plot: ",
                             min = 0,
                             max = 36,
                             value = c(0,36)
                 )
                 # Can choose the region users want to plot on scatter plot
                 ,checkboxGroupInput("Regionact",
                                     "Choose which region(s) to plot:",
                                     choices = region, 
                                     selected = region
                 )
               ),
               mainPanel(
                 #scatter plot
                 plotOutput("actploteng"),
                 plotOutput("actplotmath")
               )
      ),
      tabPanel("Table",
               h3("Average SAT and ACT Scores in Each U.S. State"),
               sidebarPanel(
                 # choose which state to view for average score for each standardized test and subject
                 selectInput("State", "Select State:",
                             choices = states
                 ),
               ),
               mainPanel(dataTableOutput("table"),
                         textOutput("sentence"))
      ),
      
      tabPanel("Conclusion",
               h3("Patterns and Discoveries"),
               p("[put text here]"),
               
               h4("Example"),
               p("[put text here]"),
               
               h3("Implications of Insight"),
               p("[put text here"),
               
               h3("Data Quality"),
               p("The dataset included a number of variables and observations that allowed 
                           us to deliver more features to feed into the data. We were able to access and 
                           use more data assets that led us to make versatile plots and tables, which 
                           helped to show and provide detailed results."),
               p("The dataset was gathered from a national center for education statistics, 
                             who are the primary federal entity responsible for collecting and analyzing 
                             data related to education in the United States. They also go through the 
                             process of conducting, publishing and reviewing data, however, there could be 
                             some limitations to the data that is presented as it comes from people who created 
                             and manipulated these datasets."),
               p("The dataset does include content that was produced by humans, which could 
                           potentially contain biased information against groups of people. However, the 
                           source that we gathered our data from uses “restricted-data” licenses as a way to 
                           make more detailed information available, which could eliminate a few aspects of 
                          data bias."),
               
               h3("Future Ideas"),
               p("As the controversial debate around whether standardized test scores should be used 
                           to determine students’ college admission increases, some universities are making SAT/ACT 
                           scores optional (especially after the COVID pandemic) and shifting their focus on other 
                           attributes of the student. That being said, a future idea to advance this project is to 
                           record data of universities that have SAT/ACT scores optional and see how the admissions 
                           rate compares against schools that require SAT/ACT scores. This could possibly generate 
                           interesting outcomes as to how college admissions requirements are changing over the years."),
               p("Another idea could be to collect data of the main categories (grades, personal statements, 
                           SAT/ACT scores, demographics, income, etc.) universities look for in prospective students, 
                           and rank them from “most considered during admission” to “least considered during admission” 
                           for each of the universities. With this information, applicants can take into consideration 
                           what college admissions are seeking the most and better prepare for their application.")
      )
      
    )))


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #image for first page
  output$Image <- renderImage({
    list(src = "picture.jpg", width = "600", height = "300")
  })
  
  # SAT verbal scatter plot
  output$satploteng <- renderPlot({
    score %>%
      filter(newRegion %in% input$Regionsat) %>%
      filter(SATVR75 >= input$SAT_Range[1],
             SATVR75 <= input$SAT_Range[2]) %>%
      ggplot(aes(SATVR75, Admission_Rate, col = newRegion)) +
      labs(title = "75th Percentile SAT English Score and Admissions Rate",
           x = "75th Percentile SAT English Score", y= "Admission Rate") +
      geom_point()
  })
  # SAT math scatter plot
  output$satplotmath <- renderPlot({
    score %>%
      filter(newRegion %in% input$Regionsat) %>%
      filter(SATMT75 >= input$SAT_Range[1],
             SATMT75 <= input$SAT_Range[2]) %>%
      ggplot(aes(SATVR75, Admission_Rate, col = newRegion)) +
      labs(title = "75th Percentile SAT Math Score and Admissions Rate",
           x = "75th Percentile SAT Math Score", y= "Admission Rate") +
      geom_point()
  })
  
  #ACT english scatter plot
  output$actploteng <- renderPlot({
    score %>%
      filter(newRegion %in% input$Regionact) %>%
      filter(ACTEN75 >= input$ACT_Range[1],
             ACTEN75 <= input$ACT_Range[2]) %>%
      ggplot(aes(ACTEN75, Admission_Rate, col = newRegion)) +
      labs(title = "75th Percentile ACT English Score and Admissions Rate",
           x = "75th Percentile ACT English Score", y= "Admission Rate") +
      geom_point() 
  })
  
  #ACT math scatter plot
  output$actplotmath <- renderPlot({
    score %>%
      filter(newRegion %in% input$Regionact) %>%
      filter(ACTMT75 >= input$ACT_Range[1],
             ACTMT75 <= input$ACT_Range[2]) %>%
      ggplot(aes(ACTMT75, Admission_Rate, col = newRegion)) +
      labs(title = "75th Percentile ACT Math Score and Admissions Rate",
           x = "75th Percentile ACT Math Score", y= "Admission Rate") +
      geom_point()
  })
  
  #Average score for each subject and standardized test for each state
  output$table <- renderDataTable({
    score %>%
      group_by(input$State) %>%
      filter(!is.na(SATVR75), !is.na(SATMT75), !is.na(ACTEN75), !is.na(ACTMT75), State == input$State) %>%
      summarize(avg_SAT_MT = mean(SATMT75), avg_SAT_VR = mean(SATVR75), avg_ACT_MT = mean(ACTMT75), avg_ACT_EN = mean(ACTEN75)) 
  })
  
  #sentence that summarizes the score for chosen state
  output$sentence <- renderText({
    scoreavg <- score %>%
      group_by(input$State) %>%
      filter(State == input$State, !is.na(SATVR75), !is.na(SATMT75), !is.na(ACTEN75), !is.na(ACTMT75)) %>%
      summarize(avg_SAT_MT = mean(SATMT75), avg_SAT_VR = mean(SATVR75), avg_ACT_MT = mean(ACTMT75), avg_ACT_EN = mean(ACTEN75))
    paste("The SAT verbal & math and ACT math and english score for", scoreavg[1], "are", scoreavg[2],",", scoreavg[3],",", scoreavg[4],"and",scoreavg[5], "respectively." )
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)