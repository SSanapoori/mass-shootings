
#Packages loaded
library(dplyr)
library(lubridate)
library(tidyr)
library(knitr)
library(kableExtra)
library(plotly)
library(leaflet)
library(ggplot2)

#Opening up the dataset and viewing it
shootings_original <- read.csv("data/shootings-2018.csv")
View(shootings_original)

#Creating a new dataframe with number of casualites which is the sum of dead and injured 
shootings <- read.csv("data/shootings-2018.csv") %>% 
  mutate(num_casualties = num_killed + num_injured)
View(shootings)
write.csv(shootings, "shootings.csv", row.names = FALSE)

##Summary 

#Date of first shooting of the year" 

#What were the total number of shootings in 2018? 

#To get the total number of shootings I can find the number of rows - 1 (for the headers)
num_shootings <- nrow(shootings_original) - 1
num_shootings
#Answer: 339

#How many lives were lost due to shootings?

#To get the number of lives lost I can find the sum of people dead. 
num_dead <- sum(shootings_original$num_killed)
num_dead

#Answer:373


#Which city was impacted the most? 

#I am defining most impacted by the highest number of deaths. 
city_most_impacted <- shootings_original %>%
                        group_by(city) %>%
                        summarise(most_dead = max(num_killed), na.rm = TRUE) %>%
                        filter(most_dead == max(most_dead)) %>%
                        select(city)
city_most_impacted
#Answer: Pompano Beach(Parkland)


#Which state lost the most people due to shootings? 

#I am looking at the most number of deaths when I say lost the most people
state_with_most_deaths <- shootings_original %>%
                            group_by(state) %>%
                            summarise(most_deaths = max(num_killed), na.rm = TRUE) %>%
                            filter(most_deaths == max(most_deaths)) %>%
                            select(state)
state_with_most_deaths

# To get the number of deaths
num_most_deaths <- shootings_original %>% 
  group_by(state) %>% 
  summarise(most_deaths = max(num_killed), na.rm = TRUE) %>% 
  filter(most_deaths == max(most_deaths)) %>% 
  select(most_deaths)
num_most_deaths
#Answer: Florida and 17

#Which city had the highest number of casualities (dead and injured)

#I will find the sum of number of people dead and injured for each incident then group it by city. 
city_most_hurt <- shootings %>%
                    group_by(city) %>%
                    summarise(most_casualties  = max(num_casualties)) %>%
                    filter(most_casualties == max(most_casualties)) %>% 
                    select(city)
city_most_hurt

#number of casualties for the city most hurt

numb_casualties <- shootings %>% 
  group_by(city) %>% 
  summarise(most_casualties = max(num_casualties)) %>% 
  filter(most_casualties == max(most_casualties)) %>% 
  select(most_casualties)
numb_casualties
#Answer: Pompano Beach (Parkland)

#Where (city) did the incident with the second highest number of people hurt occur? 

city_second_most_hurt <- shootings %>%
                           mutate(rank = rank(desc(num_casualties))) %>% 
                           group_by(city) %>% 
                           arrange(rank) %>% 
                           filter(rank == 2) %>% 
                           select(city)
city_second_most_hurt
#Answer: Santa Fe  


##Summary Table 

# I want to create a table that is grouped by state and will list out the total number
# of casualties, number killed and number injured

state_summary <- shootings %>%
                    group_by(state) %>% 
                    summarise(total_dead = sum(num_killed),
                              total_injured = sum(num_injured),
                              total_hurt = sum(num_casualties))
View(state_summary)

#Renaming column names in state_summary dataframe

colnames(state_summary)[1] <- "State"
colnames(state_summary)[2] <- "Dead"
colnames(state_summary)[3] <- "Injured"
colnames(state_summary)[4] <- "Casualties" 
View(state_summary)

#Convering the dataframe into a clean table form using kable

state_table <- state_summary %>% kable() %>% kable_styling()
state_table

#I want to find the top 5 states with the most number of shootings

top_states <- state_summary %>% 
                arrange(desc(`Casualties`)) %>% 
                top_n(5)
View(top_states)

#Convert top_5_states into a clean table form using kable

top_states_table <- top_states %>% kable() %>% kable_styling()


##Particular Incident

#For this part, I am using the tables I will find the state with the most number of 
#casualties. From there, I'll select the incident that had the most number of casualties and provide information
#about that incident. 
#I am defining casualty as the total number of people that died and were injured due to the shooting

state_with_most_people_hurt <- shootings %>% 
                                group_by(state) %>%
                                summarise(most_casualties = sum(num_casualties), na.rm = TRUE) %>% 
                                filter(most_casualties == max(most_casualties)) %>% 
                                select(state)
state_with_most_people_hurt

#getting the number of casualties in California 
number_casualities_california <- top_states %>% filter(State == "California") %>% select(Casualties)

#Answer was California with 186 casualties 

incident_with_most_casualites <- shootings %>% 
                    filter(state == "California") %>% 
                    filter(num_casualties == max(num_casualties))
View(incident_with_most_casualites)  

#Getting name of incident 
incident_name <- incident_with_most_casualites %>% 
  select(city,state)

#This incident that occured in Thousand Oaks had the highest number of casualites with 15 people hurt out
#of which 13 were dead 

#Finding date of incident
incident_date <- incident_with_most_casualites %>% 
                  select(date)

#Finding location of incident 
address <- incident_with_most_casualites %>% 
            select(address,city)
View(address)


#Finding number of casualites 
casualties <- incident_with_most_casualites %>% 
                select(num_casualties)

#Finding number of people that died 
dead <- incident_with_most_casualites %>% 
          select(num_killed)

#Finding number of people that are injured 
injured <- incident_with_most_casualites %>% 
            select(num_injured)


## Interactive Visual 

# This visual will show the location of each shooting that occured in 2018 and the size of the marker will be 
# based on the number of casualites (number injured and dead). When you hover over each point the extra information 
# provided will be the number of injuries, number of deaths, and the city and state

#Creating the dataframe for the visualization 
shootings_visualization <- shootings %>%
                            group_by(state,city,long,lat) %>%
                            summarise(total_casualities = sum(num_casualties),
                                      total_killed = sum(num_killed),
                                      total_injured = sum(num_injured)) %>%
                            arrange(-total_casualities) %>% 
                            ungroup()
View(shootings_visualization)


highest_casualties <- shootings_visualization %>% 
  filter(total_casualities == max(total_casualities)) %>% 
  select(city,state)
highest_casualties

#Hovering information
shootings_visualization$hover <- with(shootings_visualization,
                                    paste("Location:",shootings_visualization$city, ",", 
                                          shootings_visualization$state, "<br>",
                                          "Injured:", shootings_visualization$total_injured, "<br>",
                                          "Dead:", shootings_visualization$total_killed, "<br>",
                                          "Total number of people shot:", shootings_visualization$total_casualities))


visual <- leaflet(data = shootings_visualization) %>% # specify the data you want to add as a layer
  addTiles() %>%
  addCircleMarkers(
    lat = ~lat,   # a formula specifying the column to use for latitude
    lng = ~long, # a formula specifying the column to use for longitude
    popup = ~hover,    # a formula specifying the information to pop up
    radius = ~total_casualities * 2,
    stroke = FALSE
  )
visual

##Plot

# Question I want to answer is death rates in each state. 

# Creating a new dataframe that only includes, state, num_hurt and num_killed and a new column that includes death rates 

shootings_plot <- shootings %>% group_by(state) %>% 
  summarise(total_shootings = sum(num_casualties), total_killed = sum(num_killed)) %>% 
  ungroup() %>% 
  mutate(death_percentage = ((total_killed)/(total_shootings))*100)
View (shootings_plot)

         


shootings_plot$hover <- with(shootings_plot,
                                      paste("State:",shootings_plot$state, ",", "<br>",
                                            "Dead:", shootings_plot$total_killed, "<br>",
                                            "Total number of people shot:", shootings_plot$total_shootings, "<br>",
                                           "Death Percentage", shootings_plot$death_percentage))
plot <- plot_ly(
  data = shootings_plot,      
  x = ~shootings_plot$death_percentage, 
  y = ~shootings_plot$total_shootings, 
  text = ~shootings_plot$hover, 
  type = "scatter", 
  mode = "markers"  
) %>%
  layout(
    title = "Death Percentage in 2018 due to Shootings", 
    xaxis = list(title = "Death Percentage"), 
    yaxis = list(title = "Total Shootings") 
  )
plot





