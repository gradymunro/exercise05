library(shiny)
library(tidyverse)
library(lubridate)
library(rsconnect)

#--------------------Setting up Covid19 Data--------------------
covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

census_pop_est_2018 <- read_csv("https://www.dropbox.com/s/6txwv3b4ng7pepe/us_census_2018_state_pop_est.csv?dl=1") %>% 
  separate(state, into = c("dot","state"), extra = "merge") %>% 
  select(-dot) %>% 
  mutate(state = str_to_lower(state))

covid19_recent <- covid19 %>% 
  mutate(state = str_to_lower(state)) %>%
  left_join(census_pop_est_2018,
            by = c("state")) %>% 
  mutate(cases_per_100000 = (cases/est_pop_2018)*100000)
#--------------------------------------------------------------

#--------------------Setting up UI--------------------
ui <- fluidPage("Comparing Covid19 Cases per 100,000 Across States",
                selectInput(inputId = "state",
                            label = "Choose states to compare:",
                            choices = covid19_recent %>% 
                              arrange(state) %>% 
                              distinct(state) %>% 
                              pull(state),
                            multiple = TRUE),
                sliderInput(inputId = "dates",
                            label = "Choose a date range:",
                            min = as.Date("2020-01-21"),
                            max = as.Date("2022-03-30"),
                            value = c(as.Date("2020-01-21"),
                                      as.Date("2022-03-30"))),
                submitButton(text = "Create my plot!"),
                plotOutput(outputId = "covid19_plot"))

server <- function(input, output) {
  output$covid19_plot <- renderPlot(
    covid19_recent %>% 
      filter(state == input$state) %>% 
      ggplot(aes(x = date,
                 y = cases_per_100000,
                 color = state)) +
      geom_line() +
      scale_x_date(limits = input$dates)
  )
}

shinyApp(ui = ui, server = server)