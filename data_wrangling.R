#library statements 
library(dplyr)
library(stringr)


# Loading in data sets
df_1 <- read.csv("Unemployment in America Per Us State.csv")
crime_df <- read.csv("crime_and_incarceration_by_state.csv")


#data cleaning
#cleaning unemployment dataset so we only have data from 2001 to 2016
df_1 <- filter(df_1, Year >= 2001 & Year <= 2016)

#cleaning unemployment dataset so all numbers are numeric
df_1$Total.Civilian.Non.Institutional.Population.in.State.Area <- as.numeric(gsub(",", "", df_1$Total.Civilian.Non.Institutional.Population.in.State.Area))
df_1$Total.Civilian.Labor.Force.in.State.Area <- as.numeric(gsub(",", "", df_1$Total.Civilian.Labor.Force.in.State.Area))
df_1$Total.Employment.in.State.Area <- as.numeric(gsub(",", "", df_1$Total.Employment.in.State.Area))
df_1$Total.Unemployment.in.State.Area <- as.numeric(gsub(",", "", df_1$Total.Unemployment.in.State.Area))

#creating a new df with only average values of each year for each state
unemployment_df <- df_1 %>%
  group_by(State.Area, Year) %>%
  summarize(Total.Civilian.Non.Institutional.Population.in.State.Area = round(mean(Total.Civilian.Non.Institutional.Population.in.State.Area),0 ),
            Total.Civilian.Labor.Force.in.State.Area = round(mean(Total.Civilian.Labor.Force.in.State.Area), 0),
            Percent.....of.State.Area.s.Population = round(mean(Percent.....of.State.Area.s.Population), 1),
            Total.Employment.in.State.Area = round(mean(Total.Employment.in.State.Area), 0), 
            Percent.....of.Labor.Force.Employed.in.State.Area = round(mean(Percent.....of.Labor.Force.Employed.in.State.Area), 1),
            Total.Unemployment.in.State.Area = round(mean(Total.Unemployment.in.State.Area), 0),
            Percent.....of.Labor.Force.Unemployed.in.State.Area = round(mean(Percent.....of.Labor.Force.Unemployed.in.State.Area), 1),
            .groups = 'drop')

#renaming column titles 
unemployment_df <- rename(unemployment_df, State = State.Area)
unemployment_df <- rename(unemployment_df, Avg.Civilian.Non.Institutional.Pop = Total.Civilian.Non.Institutional.Population.in.State.Area)
unemployment_df <- rename(unemployment_df, Avg.Civilian.Labor.Force = Total.Civilian.Labor.Force.in.State.Area)
unemployment_df <- rename(unemployment_df, Avg.Perc.of.State.Pop = Percent.....of.State.Area.s.Population)
unemployment_df <- rename(unemployment_df, Avg.Employment = Total.Employment.in.State.Area)
unemployment_df <- rename(unemployment_df, Avg.Perc.of.Labor.Force.Employed = Percent.....of.Labor.Force.Employed.in.State.Area)
unemployment_df <- rename(unemployment_df, Avg.Unemployment = Total.Unemployment.in.State.Area)
unemployment_df <- rename(unemployment_df, Avg.Perc.of.Labor.Force.Unemployed = Percent.....of.Labor.Force.Unemployed.in.State.Area) 
crime_df <- rename(crime_df, State = jurisdiction)
crime_df <- rename(crime_df, Year = year)

#changing state values from all caps to lower in crime_df
crime_df$State <- str_to_title(tolower(crime_df$State))

#merging the data sets 
df <- merge(unemployment_df, crime_df, by = c("State", "Year"))

#creating a new categorical variable - regions
regions <- list("Connecticut" = "Northeast", "Maine" = "Northeast","Massachusetts" = "Northeast",
  "New Hampshire" = "Northeast","Rhode Island" = "Northeast","Vermont" = "Northeast",
  "New Jersey" = "Northeast","New York" = "Northeast","Pennsylvania" = "Northeast",
  "Illinois" = "Midwest","Indiana" = "Midwest","Michigan" = "Midwest","Ohio" = "Midwest",
  "Wisconsin" = "Midwest","Iowa" = "Midwest","Kansas" = "Midwest","Minnesota" = "Midwest",
  "Missouri" = "Midwest","Nebraska" = "Midwest","North Dakota" = "Midwest",
  "South Dakota" = "Midwest","Delaware" = "South","Florida" = "South",
  "Georgia" = "South","Maryland" = "South","North Carolina" = "South",
  "South Carolina" = "South","Virginia" = "South","District of Columbia" = "South",
  "West Virginia" = "South","Alabama" = "South","Kentucky" = "South","Mississippi" = "South",
  "Tennessee" = "South","Arkansas" = "South","Louisiana" = "South",
  "Oklahoma" = "South","Texas" = "South","Arizona" = "West","Colorado" = "West",
  "Idaho" = "West","Montana" = "West","Nevada" = "West","New Mexico" = "West",
  "Utah" = "West","Wyoming" = "West","Alaska" = "West","California" = "West",
  "Hawaii" = "West","Oregon" = "West","Washington" = "West"
)
df <- df %>% left_join(data.frame(State = names(regions), Region = unlist(regions)),by = "State")

#adding a new total crime count column
df$total_crime <- df$property_crime_total + df$violent_crime_total

#making a df with only state, region, and unemployment data
drill_data_df <- df %>%
  group_by(State, Region) %>%
  summarise(Avg.Unemployment = mean(Avg.Unemployment), total_crime = mean(total_crime, na.rm = TRUE))

#making the region vs unemployment df
regions_df <- df %>% 
  group_by(Region) %>%
  summarise(total_crime = mean(total_crime, na.rm = TRUE), Avg.Unemployment = mean(Avg.Unemployment))

state_df <- filter(drill_data_df, Region == "South")


