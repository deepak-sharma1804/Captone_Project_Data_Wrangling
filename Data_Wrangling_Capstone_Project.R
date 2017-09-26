# Loading Library

library("dplyr")
library("tidyr")
library("stringr")

# Reading Initial Dataset

initial_data <- read.csv(file = "D:\\Data_Wrangling\\Traffic_Violations.csv",header = T)

# converting data frame into tibble

initial_data <- as.tbl(initial_data)

# changing column names having dot(.) and Hazmat to HazardousMaterials

names(initial_data) <- str_replace_all(names(initial_data),pattern = "\\.",replacement = "")

names(initial_data) <- str_replace(names(initial_data),pattern = "HAZMAT",replacement = "HazardousMaterials")


# dividing the data to two parts one without NA and one with NA

initial_data <- na.omit(initial_data)


# It has been observed that if person has violated mutliple laws then each row represent each charge that the person has charged with,
# we are grouping  such row so that each row repsent each person
name <- names(initial_data)

initial_data <- initial_data %>% group_by_at(names(initial_data)[-grep("Description|Charge", names(initial_data))])  %>% summarise(Description =paste(Description,collapse ="," ))


#Handling missing values

# a) if there are missing values in State or DriverState or DL State then set it to default state "MD"
initial_data$State <- as.character(initial_data$State)
initial_data$DriverState <- as.character(initial_data$DriverState)
initial_data$DLState <- as.character(initial_data$DLState)



temp_dataset <- initial_data %>% filter((State=="XX"|State=="") | (DriverState=="XX"|DriverState=="") | (DLState=="XX"|DLState==""))

dim(temp_dataset)

initial_data$State <- ifelse(initial_data$State=="XX"|initial_data$State=="","XX",initial_data$State)

initial_data$DriverState <- ifelse(initial_data$DriverState=="XX"|initial_data$DriverState=="","XX",initial_data$DriverState)

initial_data$DLState <- ifelse(initial_data$DLState=="XX"|initial_data$DLState=="","XX",initial_data$DLState)

# Ordering the dataset by Date and Time 

initial_data <- initial_data %>% arrange(DateOfStop,TimeOfStop)

#NEED TO BE EXECUTED AFTER TALKING TO BRANKO

# replacing abbrevaited form of state to full form for better understand.
value <- FALSE

#value <- TRUE

if(value)
{

states_full <- c("AA" = "Armed Forces America" , "AE" = "Armed Forces" , "AP" = "Armed Forces Pacific" , "AK" = "Alaska" , "AL" = "Alabama" , "AR" = "Arkansas" ,
            "AZ" = "Arizona" , "CA" = "California" , "CO" = "Colorado" , "CT" = "Connecticut" , "DC" = "Washington DC (District of Columbia)" , "DE" = "Delaware" ,
            "FL" = "Florida" , "GA" = "Georgia" , "GU" = "Guam" , "HI" = "Hawaii" , "IA" = "Iowa" , "ID" = "Idaho" , "IL" = "Illinois" , "IN" = "Indiana" , "KS" = "Kansas" ,
            "KY" = "Kentucky" , "LA" = "Louisiana" , "MA" = "Massachusetts" , "MD" = "Maryland" , "ME" = "Maine" , "MI" = "Michigan" , "MN" = "Minnesota" , "MO" = "Missouri" ,
            "MS" = "Mississippi" , "MT" = "Montana" , "NC" = "North Carolina" , "ND" = "North Dakota" , "NE" = "Nebraska" , "NH" = "New Hampshire" , "NJ" = "New Jersey" ,
            "NM" = "New Mexico" , "NV" = "Nevada" , "NY" = "New York" , "OH" = "Ohio" , "OK" = "Oklahoma" , "OR" = "Oregon" , "PA" = "Pennsylvania" , "PR"="Puerto Rico" ,
            "RI" = "Rhode Island" , "SC" = "South Carolina" , "SD" = "South Dakota" , "TN" = "Tennessee" , "TX" = "Texas" , "UT" = "Utah" , "VA" = "Virginia" ,
            "VI" = "Virgin Islands" , "VT" = "Vermont" , "WA" = "Washington" , "WI" = "Wisconsin" , "WV" = "West Virginia" , "WY" = "Wyoming")

#test3 <- state.name[grep(pattern = as.character(initial_data$DriverState),x = state.abb,ignore.case = T)]

test3 <- function(state){
  print(state)
  state.name[grep(pattern = state,x = state.abb)]
}

initial_data$DriState <- lapply(initial_data$DriverState,test3)

initial_data %>% select(DriverState,DriState)

View(head(initial_data,60))

initial_data$DLState <- states_full[initial_data$DLState]


initial_data %>% select(DriverState,DLState)

states_full["VA"]


initial_data 

View(initial_data)

distinct(initial_data$Charge)

}
