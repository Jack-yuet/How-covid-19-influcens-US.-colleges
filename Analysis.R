# Part 2 Data Description 
library("dplyr")
library("ggplot2")

cwurData_sample_df <- read.csv("cwurData.csv") %>%
  filter(country == "USA") %>% 
  filter(year == max(year)) %>% 
  slice_head(n = 10) %>% 
  select(world_rank, institution, country, national_rank, score)

college_sample_df <- read.csv("College.csv") %>% 
  slice_head(n = 10) %>% 
  select(College, Private, Apps, Accept, Enroll, Outstate, Room.Board, Grad.Rate, Expend)

universities_sample_cases <- read.csv("university cases.csv") %>%
  slice_head(n = 10) %>% 
  select(date, state, county, college, cases)

state_sample_df <- read.csv("us-states.csv") %>%
  slice_head(n = 10) %>% 
  select(date, state, cases)

# Part 2.2 Summary Analysis
## Summary descriptive statistics - Dean & Jennifer 
college_df <- read.csv("College.csv")
summary_statistics_for_us_college <- summary(college_df)
college_df <-na.omit(college_df)
mean(college_df$Accept)
#2018.804
median(college_df$Accept)
#1110
max(college_df$Accept)
#26330
mean(college_df$Grad.Rate)
#65.463
median(college_df$Grad.Rate)
#65
max(college_df$Grad.Rate)
#118
us_college_cases <- read.csv("university cases.csv")
summary_statistics_for_college_cases <- summary(us_college_cases)
us_college_cases <-na.omit(us_college_cases)
mean(us_college_cases$cases)
#213.352
max(us_college_cases$cases)
#5806
min(us_college_cases$cases)
#0
median(us_college_cases$cases)
#54
sd(us_college_cases$cases)
##525.3174

## Graph 1 - Jinrui Fang
cwurData_df <- read.csv("cwurData.csv")
us_ranking_data <- cwurData_df %>% 
  filter(country == "USA") %>% 
  filter(year == max(year)) %>% 
  select(world_rank, institution, country, national_rank, score)

universities_cases <- read.csv("university cases.csv")
us_college_cases <- universities_cases %>% 
  select(date, state, county, college, cases)

college_df <- read.csv("College.csv")
us_college_information <- college_df %>% 
  select(College, Private, Apps, Accept, Enroll, Outstate, Room.Board, Grad.Rate, Expend)

college_acceptance_rate <- us_college_information %>% 
  mutate(accept_rate = round(Accept / Apps, 2) * 100)

us_college_acceptance_rate_plot <- ggplot(college_acceptance_rate) +
  geom_point(mapping = aes(x = accept_rate, y = Grad.Rate)) +
scale_x_continuous(labels = function(x) paste0(x, '%')) +
  scale_y_continuous(labels = function(y) paste0(y, '%')) +
  labs(
    title = 'Graduation and Acceptance Rate of Colleges in the U.S.',
    x = 'Acceptance Rate(%)', y = 'Graduation Rate(%)'
  ) +
  theme(
    legend.title = element_blank()
  )

outlier_1 <- college_acceptance_rate %>% 
  filter(Grad.Rate == max(Grad.Rate)) %>% 
  select(Grad.Rate, accept_rate, College)

outlier_2 <- college_acceptance_rate %>% 
  filter(accept_rate == min(accept_rate)) %>% 
  select(Grad.Rate, accept_rate, College)
  
## Graph 2 - Jack
state_cases <- read.csv("us-states.csv")

us_state_cases <- state_cases %>%
  filter(date == "2020-12-11") %>%
  select(date, state, cases)

highest_4_state_cases <- top_n(us_state_cases, 4, wt = cases)
lowest_4_state_cases <- top_n(us_state_cases, -4, wt = cases)
top_states <- rbind(highest_4_state_cases, lowest_4_state_cases) 
top_states <- arrange(top_states, desc(cases))

States_with_most_and_least_cases <-
  ggplot(top_states) +
  aes(x = reorder(state, cases), y = cases) +
  geom_col(mapping = aes(x=state, y=cases, fill = state)) +
  labs(
    title = "COVID-19 Cases in Some States of US",
    x = "State",
    y = "COVID-19 Cases"
  ) + 
  scale_color_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(size = 5))

outlier_graph2 <- top_states %>% 
  filter(cases == min(top_states$cases)) %>% 
  select(state, cases)

############
## Graph 3 - Dean & Jennifer
college_df <- read.csv("College.csv")

college_spending_outstate_df <- college_df %>% 
  mutate(spending_fees_of_colleges = Outstate + Room.Board + Books + Personal + Expend) %>% 
  select(College,Apps,spending_fees_of_colleges)

#View(college_spending_outstate_df)
college_spending_outstate_plot <- ggplot(data = college_spending_outstate_df) +
  geom_point(mapping = aes(x = Apps, y = spending_fees_of_colleges),color = "blue" 
             ) +
  labs(
    title = "Cost and Spending for different colleges depending on numbers of applications",
    x = "Applications", 
    y = "Total Spendging Fees For Each Student Per Year"
  ) +
  theme(legend.title = element_blank())

outlier_spending_college <- college_spending_outstate_df %>% 
  filter(spending_fees_of_colleges > 80000) %>% 
  select(College)
# Johns Hopkins University
Outlier_specific_spending <- college_spending_outstate_df %>% 
  filter(College == "Johns Hopkins University") %>% 
  select(spending_fees_of_colleges)
# 83313

Outlier_Specific_college <- college_spending_outstate_df %>% 
  filter(Apps > 45000) %>% 
  select(College)
#Rutgers at New Brunswick

Outlier_Specific_applications <- college_spending_outstate_df %>% 
  filter(College == "Rutgers at New Brunswick") %>% 
  select(Apps)
#48094

mean(college_spending_outstate_df$spending_fees_of_colleges)
# 26348.39

median(college_spending_outstate_df$spending_fees_of_colleges)
# 24493

median_Spending_college <- college_spending_outstate_df %>% 
  filter(spending_fees_of_colleges == 24493) %>% 
  select(College)
# Salem-Teikyo University

#########
# Section 3 Specific Question Analyses
### Q1 -- By: Jinrui Fang 
college_private_acceptance_rate <- college_acceptance_rate %>% 
  filter(Private == "Yes") %>%
  select(College, accept_rate, Expend)
college_avg_private_acceptance_rate <- mean(college_private_acceptance_rate$accept_rate)
college_avg_private_expend <- mean(college_private_acceptance_rate$Expend)

college_public_acceptance_rate <- college_acceptance_rate %>% 
  filter(Private == "No") %>% 
  select(College, accept_rate, Expend)
college_avg_public_acceptance_rate <- mean(college_public_acceptance_rate$accept_rate)
college_avg_public_expend <- mean(college_public_acceptance_rate$Expend)

college_expend_plot <- ggplot(college_acceptance_rate) +
  geom_point(mapping = aes(x = accept_rate, y = Expend, color = Private)) +
  labs(
    title = 'The distribution Acceptance Rate and Expend of public and private colleges in the U.S.',
    x = 'Acceptance Rate(%)', y = 'Expend($)'
  ) +
  scale_color_discrete(name="College Type", labels=c("Private", "Public"))

### Q2 -- Dean 
cwurData_df <- read.csv("cwurData.csv")

us_college_with_factors <- cwurData_df %>% 
  filter(country == "USA") %>% 
  filter(year == max(year)) %>% 
  mutate(overall_performace_value = (quality_of_education+quality_of_faculty+influence+citations+patents)/5) %>% 
  rename( "quality of education" = quality_of_education,
          "quality of faculty" = quality_of_faculty )%>% 
  select(institution,national_rank,'quality of education','quality of faculty',
         influence,citations,patents,overall_performace_value)

# We can call overall_performace_value as OPV.

overall_performace_value_plot <- ggplot(data = us_college_with_factors) +
  geom_point(mapping = aes(x = national_rank, y = overall_performace_value),
             color = "lightblue")+
  labs(
    title = 'The Performance of US colleges with National Rankings based on OPV',
    x = 'National Rankings', y = 'Overall Performace Values'
  ) +
  theme(
    legend.title = element_blank()
  )
# overall_performace_value_plot

The_most_excellent_college <- us_college_with_factors %>% 
  filter(overall_performace_value == min(overall_performace_value)) %>% 
  select(institution,national_rank,'quality of education','quality of faculty',
         influence,citations,patents, overall_performace_value)

# Harvard University
The_most_poor_college <- us_college_with_factors %>% 
  filter(overall_performace_value == max(overall_performace_value)) %>% 
  select(institution,national_rank,'quality of education','quality of faculty',
         influence,citations,patents, overall_performace_value)
# Oakland University

college__outlier_of_performance <- us_college_with_factors %>% 
  filter(overall_performace_value>400, national_rank<75) %>% 
  select(institution,national_rank,'quality of education','quality of faculty',
         influence,citations,patents, overall_performace_value)
# Southern Methodist University

college_collection <- c(The_most_excellent_college[1],The_most_poor_college[1],
                        college__outlier_of_performance[1])

### Q3 -- Jennifer 
us_cases_info <- read.csv("us-states.csv") 
universities_cases <- read.csv("university cases.csv")
us_college_cases <- universities_cases %>% 
  select(date, state, county, college, cases)
#wrangling data
colnames(us_college_cases) <- c("Date", "State", "County", "College", "Cases_in_Campus")

#calculate the cases number for each state before 2020-12-11，since the cases datd from
#"us_college_cases" is on 2020-12-11.
us_cases_with_date <- us_cases_info %>%
  mutate(date = as.character(date))

college_cases_with_date <- us_college_cases %>%
  mutate(date = as.character(Date))

most_recent_date <- max(us_cases_info$date)
#"2021-02-20"

us_cases_before_date_in_college_data <- us_cases_with_date %>%
  select(date, state, cases) %>%
  filter(date == "2020-12-11") %>%
  group_by(state) %>%
  select(state, cases)
#View(us_cases_before_date_in_college_data)

#calculate the total cases number and ratio for college students in each state
state_college_cases <- us_college_cases %>%
  group_by(State) %>%
  summarise(College_Cases = sum(Cases_in_Campus))
#View(state_college_cases)

colnames(us_cases_before_date_in_college_data) <- c("State", "State_Cases")
college_cases_in_state <- left_join(state_college_cases, us_cases_before_date_in_college_data, 
                                    by = "State") %>%
  mutate(College_Cases_Ratio = round(College_Cases / State_Cases , 6) * 100)
college_cases_in_state <- na.omit(college_cases_in_state)
#View(college_cases_in_state)

#plots
college_student_cases_ratio_plot <- 
  ggplot(data = college_cases_in_state, aes(x = State, y = College_Cases_Ratio)) +
  geom_bar(stat='identity') +
  labs(
    title = "The performance of us colleges with rankings",
    x = "National", 
    y = "The performance of us colleges with rankings"
  ) +
  theme(axis.text.x = element_text(angle=50, size=7, hjust = 1))

outlier_1 <- college_cases_in_state %>%
  filter(College_Cases_Ratio == max(College_Cases_Ratio))
#Wyoming with 5.60 college cases ratio
outlier_2 <- college_cases_in_state %>%
  filter(College_Cases_Ratio == min(College_Cases_Ratio))
#Northern Mariana Islands with 0 college cases ratio
max_state_cases <- college_cases_in_state %>%
  filter(State_Cases == max(State_Cases)) 
max_state_cases
#New York has the highest cases number in its state, 759765
min_state_cases <- college_cases_in_state %>%
  filter(State_Cases == min(State_Cases))
#Northern Mariana Islands has the lowest cases number in its state, 113
#evaluation
college_cases_ratio_mean <- mean(college_cases_in_state$College_Cases_Ratio, na.rm = T)
#2.671063
college_cases_ratio_median <- median(college_cases_in_state$College_Cases_Ratio, na.rm = T)
#2.78365
college_cases_ratio_standard<- sd(college_cases_in_state$College_Cases_Ratio, na.rm = T)
#2.78365

### Q4 -- Jack 
us_college_cases_df <- read.csv("university cases.csv")
state_college_cases <- us_college_cases_df %>%
  group_by(state) %>%
  summarise(state_college_total_cases = sum(cases))

us_state_cases[us_state_cases == "District of Columbia"] <- "Washington, D.C."

state_comparision <- left_join(us_state_cases, state_college_cases, by="state")

state_comparision <- mutate(
  state_comparision,
  college_student_proportion = state_college_total_cases * 100 / cases
)

highest_state_percentage <- top_n(state_comparision, 1, wt = college_student_proportion)
lowest_state_percentage <- top_n(state_comparision, -1, wt = college_student_proportion)

highest_state_percentage_info <- highest_state_percentage %>%
  select(state, cases, state_college_total_cases)
lowest_state_percentage_info <- lowest_state_percentage %>%
  select(state, cases, state_college_total_cases)

state_comparision <- state_comparision %>%
  filter(state_college_total_cases != 0)

COVID_cases_college_proportion_plot <- ggplot(state_comparision) +
  geom_point(mapping = aes(x=cases, y=college_student_proportion, color = state)) +
  labs(
    title = "Ratio of College COVID-19",
    x = "Total Cases",
    y = "The Percentage of College Students Diagnosed (%)"
  )
