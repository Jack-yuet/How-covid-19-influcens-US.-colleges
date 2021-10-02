library(shiny)
library(dplyr)
library(tidyverse)

college_df <- read.csv("College.csv")
us_college_information <- college_df %>% 
  select(College, Private, Apps, Accept, Enroll, Outstate, Room.Board, Grad.Rate, Expend)

college_acceptance_rate <- us_college_information %>% 
  mutate(accept_rate = round(Accept / Apps, 2) * 100)

range_accept_rate <- college_acceptance_rate %>% 
  select(accept_rate) %>% 
  range()

range_expense <- college_acceptance_rate %>% 
  select(Expend) %>% 
  range()

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

min_ratio <- min(college_cases_in_state$College_Cases_Ratio)
max_ratio <- max(college_cases_in_state$College_Cases_Ratio)

cwurData_df <- read.csv("cwurData.csv")
us_college_with_factors <- cwurData_df %>% 
  filter(country == "USA") %>% 
  filter(year == max(year)) %>% 
  select(institution, country, national_rank,quality_of_education,quality_of_faculty,
         influence,citations,patents) %>% 
  mutate(overall_performace_value = (quality_of_education + quality_of_faculty + influence+citations+patents) / 5)

max_num_college <- nrow(us_college_with_factors)
features_vectors <- colnames(us_college_with_factors)

data_intro_panel <- tabPanel(
  h2("Home Page"),
  h2("Group Members"),
  h3("Jack Yue, Jennifer Zhang, Jinrui Fang, Dean Dai"),
  h2("Problem Domain"),
  p("COVID-19, as a global pandemic, has caused a significant impact on modern society
    in all aspects. Due to the COVID-19, almost every university in the United States 
    has canceled the in-person courses, and the admission policies for first-year students 
    have also changed a lot. For students in the university application season, the application
    for this year can be much more difficult. As an international student, it is not easy 
    to collect information to choose and apply for a U.S. university. Under the pandemic, 
    this situation can be even more challenging. Our purpose is to collect information on 
    U.S. universities, including school rankings, school admission information, and policy 
    changes response to COVID-19. While providing information to students, we also want to help 
    students choose the university that suits them to apply."),
  h2("Data Introduction"),
  a("The first dataset", href = "https://www.kaggle.com/gpreda/world-university-rankings-advanced-analysis"),
  p("The Center for World University Rankings (CWUR) is a prominent organization and publisher
    of the one of the largest academic ranking of global universities and colleges. This data set
    covers three rankings' systems across the world, the Times Higher Education World University 
    Ranking, the Academic Ranking of World Universities, also known as the Shanghai Ranking, to The
    Center for World University Rankings. Myles O'Neill collected and made this data set available 
    for us to access. At the top of the website, we could see he made this data set to make students 
    investigate the best universities globally. This sample data set filters the ranking data for USA 
    in the most recent year. It contains world ranking, national rank and score for institutions."), 
  a("The second dataset", href = "https://www.kaggle.com/flyingwombat/us-news-and-world-reports-college-data"), 
  p("In this data set, we had 777 observations with 18 variables. Jason Nguyen collects this data set, 
    and I download it from Kaggle. She collected these data from U.S. News, a media company dedicated to 
    helping citizens, consumers, business leaders, and policy officials make crucial decisions. This data
    set enables us to filter the university which we want to be based on the data. This sample data set
    shows enrollment information of each college in the USA. It contains the colleges' name, the number
    of the applications, the number fo the acceptations, the number of outstate's student, tuition and 
    graduate rate. "),
  a("The third dataset", href = "https://github.com/nytimes/covid-19-data/blob/master/colleges/colleges.csv"),
  p("This dataset shows the counts of Covid-19 cases reported on college and university campuses in the
    United States.The author who created this dataset is Albert Sun. At this particular time, knowing the
    confirmed cases of COVID-19 at each school can help students make a better college choice because the
    number of confirmed cases is closely related to the students' health. COVID-19 College Cases Dataset
    was created by New York Times, and they frequently updated this dataset, and the last version was 
    updated on December 11th. In addition, this dataset attractBesidestion because it has a close relationship
    to all questions related to how COVID-19 affects colleges' action in the U.S.The special term in this 
    dataset: ipeds_id: The ID number of the college in the Integrated Postsecondary Education Data System 
    (IPEDS). This sample data set shows number of covid cases in each college in the USA on latest day of 
    data set."),
  a("The fourth dataset", href = "https://github.com/nytimes/covid-19-data/blob/master/us-states.csv"),
  p("In this data set, we had 19869 observations with 5 variables. Albert Sun from New York Times collects this data set, 
    and I download it from Github. The most interesting part of this dataset is that it is a daily-updating dataset, which 
    means the author of this dataset will update the total COVID-19 cases of each states daily. This data set enables us 
    to have information about the COVID-19 confirmed cases in each states. I include 10 observations in This sample data set 
    to show the total COVID-19 confirmed cases of 10 states in the United States on December 11th. It contains the date, the 
    name of the state, and the COVID-19 confirmed cases in the state we choose.")
)

Jack_panel <- tabPanel(
  titlePanel("COVID-19 in College"),
  sidebarLayout(
    sidebarPanel(
      c("Tips: Please input three states in United States. The first letter entered below needs to be capitalized. If you want to input 
        Washington DC, please input Washington, D.C."),
      textInput(inputId = "state_1", label = "Input the first state:", value = "Washington"),
      textInput(inputId = "state_2", label = "Input the second state:", value = "California"),
      textInput(inputId = "state_3", label = "Input the third state:", value = "Iowa")
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("COVID-19 Plot", 
                           h3("Does state with higher total cases also have higher ratio of college students 
                              infected to the total number of infections in the state?"),
                           br(),
                           h5("According to the data we have, Northern Mariana Islands is the place with lowest 
                           total COVID-19 cases, and it is also the place that the ratio of college students infected 
                           to the total number of infections is the lowest in U.S. since it has the 
                           college students diagnosed case equals to 0. However, Wyoming has the greatest 
                           percentage of college students diagnosed with a number of 5.59%, but it only has 2171 college student diagnosed cases.
                           In comparision, California has 9065 student cases with a rate of only 0.59%
                           In conclusion, there is no clear relationship between the number of total cases and the ratio of college students 
                           infected to the total number of infections in the state."),
                           textOutput(outputId = "intro"),
                           br(),       
                           plotOutput(outputId = "plot1"), 
                           br(),
                           textOutput(outputId = "alert1")
                  )
      )
    )
  )
)

Jennifer_panel <- tabPanel(
  titlePanel("What percentage of the number of college students diagnosed in each state accounted 
             for the total number of diagnoses in the state?"), 
  sidebarLayout(
    sidebarPanel(
      c("Clik on the slider to pick the ratio range and the state to see the ratio 
        for college students diagnosed with COVID-19 in each state in US."), 
      sliderInput(inputId = "ratio", label = "Pick the ratio range you want to check",  #修改label
                  min = min_ratio, max = max_ratio, value = c(min_ratio, max_ratio)), 
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", 
                 textOutput(outputId = "intro2"), 
                 br(),
                 plotOutput("plot3"),
                 br(),
                 textOutput(outputId = "alert3"))
      )
    )
  )
)

Fjr_panel <- tabPanel(
  titlePanel("Accept/Expense Rate"),
  sidebarLayout(
    
    sidebarPanel(
      sliderInput(inputId = "acceptrate", label = "Pick the range of the acceptance rate", 
                  min = range_accept_rate[1], max = range_accept_rate[2], value = range_accept_rate),
      
      sliderInput(inputId = "expense", label = "Pick the range of the expense", 
                  min = range_expense[1], max = range_expense[2], value = range_expense)
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Plot",
                           h3("How would the expense in the university affect the accept rate?"),
                           br(),
                           h5("Based on our data, the expense of private universities is usually
                              higher than the public universities. What is more,
                              the expense of US public universities is below 20,000$, 
                              and the accept rate is almost between 25% to 100%. However, there is no
                              one could deny that the expense in the universities does not have dierect
                              and explicit with the accept rate. For private universities, the higher expense,
                              in some degree, is with lower accept rate."),
                           textOutput(outputId = "info"),
                           br(),                        
                           plotOutput(outputId = "plot2"), 
                           br(),
                           textOutput(outputId = "alert")
                  )
                  
      )
    )
  )
)

Dean_panel <- tabPanel(
  titlePanel("How are the factors on ranking US colleges?"),
  
  p("These are six factors that you could choose to compare different colleges,
    note: there was one factor called overall performance value which is calculated 
    by the the previous five vectors divided by 5, which could show the tendency of the
    combined influence on the performance of the colleges"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "feature_choice", label = "Performace factors to consider",
        choices = features_vectors, selected = "quality_of_education")
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput(outputId = "plot4"))
      )
    ),
  )
)

Group_ui <- navbarPage(
    title = "Information about U.S. Colleges and its special condition under COVID-19",
    data_intro_panel,
    Jack_panel,
    Jennifer_panel,
    Fjr_panel,
    Dean_panel
)