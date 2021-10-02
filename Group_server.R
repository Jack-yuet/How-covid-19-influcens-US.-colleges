library(tidyverse)
library(stringr)
library(lintr)
library(styler)
library(shiny)
library(ggplot2)
library(plotly)

state_cases <- read.csv("us-states.csv")
us_state_cases <- state_cases %>%
  filter(date == "2020-12-11") %>%
  select(date, state, cases)

us_college_cases_df <- read.csv("university cases.csv")
us_college_cases_df[is.na(us_college_cases_df)] <- 0
state_college_cases <- us_college_cases_df %>%
  group_by(state) %>%
  summarise(state_college_total_cases = sum(cases))

us_state_cases[us_state_cases == "District of Columbia"] <- "Washington, D.C."

state_comparision <- left_join(us_state_cases, state_college_cases, by="state")
state_comparision <- state_comparision %>% 
  mutate(rate = 100 * state_college_total_cases / cases)

us_cases_info <- read.csv("us-states.csv") 
universities_cases <- read.csv("university cases.csv")
us_college_cases <- universities_cases %>% 
  select(date, state, county, college, cases)
colnames(us_college_cases) <- c("Date", "State", "County", "College", "Cases_in_Campus")

us_cases_before_date_in_college_data <- us_cases_info %>%
  select(date, state, cases) %>%
  filter(date == "2020-12-11") %>%
  group_by(state) %>%
  select(state, cases)

state_college_cases <- us_college_cases %>%
  group_by(State) %>%
  summarise(College_Cases = sum(Cases_in_Campus))
colnames(us_cases_before_date_in_college_data) <- c("State", "State_Cases")
college_cases_in_state <- left_join(state_college_cases, us_cases_before_date_in_college_data, 
                                    by = "State") %>%
  mutate(College_Cases_Ratio = round(College_Cases / State_Cases , 4) * 100)
college_cases_in_state <- na.omit(college_cases_in_state)

us_college_with_factors <- cwurData_df %>% 
  filter(country == "USA") %>% 
  filter(year == max(year)) %>% 
  select(institution, country, national_rank,quality_of_education,quality_of_faculty,
         influence,citations,patents) %>% 
  mutate(overall_performace_value = (quality_of_education+quality_of_faculty+influence+citations+patents)/5)

Group_server <- function(input, output) {
  
  output$alert1 <- renderText({
    alert <- c("(It might be slow. If you cannot see the plot, please wait for a moment.) ")
    return(alert)
  })
  
  output$intro <- renderText({
    
    intro_info <- c("Northern Mariana Islands has the lowest proportion of students infected in 
    the total population in U.S. states of 0, and Wyoming has the highest ratio of 0.056. The 
    first state you chose for comparision is ", input$state_1, ". The second state 
              you chose in the United States is", input$state_2, ". The third state you chose in the
              United States is", input$state_3)
    return(intro_info)
  })
  
  output$plot1 <- renderPlot({
    title <- paste0("Comparision of College Students' COVID Cases in Different States")
    
    plot_data <- state_comparision %>% 
      filter(state == input$state_1 | state == input$state_2 | state == input$state_3)
    
    my_plot <- ggplot(plot_data, mapping = aes_string(x = "state", y = "rate", fill = "state")) +
      geom_bar(stat = "identity") +
      labs(
        title = 'The proportion of students infected in the total population in U.S. states',
        x = 'States',
        y = 'Proportion of College Students Infected (%)'
      )
    my_plot
  })
  
  output$alert <- renderText({
    alert <- c("(If it does not show the plot, please wait for a moment.) ")
    return(alert)
  })
  
  output$info <- renderText({
    info <- c("The range of universities' expense you chose is from ", input$expense[1], "$ to",
              input$expense[2], "$. And the range of universities' acceptance rate you chose is from",
              input$acceptrate[1], "% to", input$acceptrate[2], "%.")
    return(info)
  })
  
  output$plot2 <- renderPlot({
    title <- paste0("The distribution of the acceptance rate and expend in the U.S.")
    
    plotdata <- college_acceptance_rate %>% 
      filter(Expend >= input$expense[1], Expend <= input$expense[2]) %>%
      filter(accept_rate >= input$acceptrate[1], accept_rate <= input$acceptrate[2])
    
    my_plot <- ggplot(plotdata) +
      geom_point(mapping = aes_string(x = "accept_rate", y = "Expend", color = "Private")) +
      labs(
        title = 'The distribution Acceptance Rate and Expend of public and private colleges in the U.S.',
        x = 'Acceptance Rate(%)', y = 'Expend($)'
      ) +
      scale_color_discrete(name="College Type", labels=c("Public", "Private"))
    my_plot
  })
  
  output$plot3 <- renderPlot({
    title <- paste0("College Students' Cases Ratio for US States")
    plotdata <- college_cases_in_state %>%
      filter(College_Cases_Ratio >= input$ratio[1], College_Cases_Ratio <= input$ratio[2])
    ratio_plot <- 
      ggplot(plotdata, aes(x = State, y = College_Cases_Ratio)) +
      geom_bar(stat='identity') +
      labs(title = title, 
           x = "State", 
           y = "Case Ratio for College Students") +
      theme(axis.text.x = element_text(angle=50, size=7, hjust = 1))
    ratio_plot
  })
  
  output$info <- renderText({
    info <- c("The range of universities' expense you chose is from ", input$expense[1], "$ to",
              input$expense[2], "$. And the range of universities' acceptance rate you chose is from",
              input$acceptrate[1], "% to", input$acceptrate[2], "%.")
    return(info)
  })
  
  output$alert2 <- renderText({
    alert <- c("(If it does not show the plot, please wait for a moment.) ")
    return(alert)
  })
  
  output$intro2 <- renderText({
    intro <- c("The ratio range you choosed is from ", input$ratio[1], " to ", input$ratio[2], ". The chart 
               below shows the ratio range you choosedf or college students diagnosed with COVID-19
               in each state.") 
    return(intro)
    
    output$alert3 <- renderText({
      alert <- c("If the chart information is not displayed immediately, 
               please wait for a moment or try to refresh the web page.")
      return(alert)
    })
  }) 
  
  output$plot4 <- renderPlot({
    
    title <- paste0("The influences of different factors on rank of the colleges in the U.S.")
    
    college_plot <- ggplot(data = us_college_with_factors) +
      geom_point(mapping = aes_string(x = "national_rank", y = input$feature_choice,
                                      color = "overall_performace_value"))
    return(college_plot)
  })
  
}