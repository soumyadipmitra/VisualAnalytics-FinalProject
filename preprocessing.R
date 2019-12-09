library(tidyverse)
library(tidyr)
library(readxl)

kids_data <- function() {
  #kids dataset
  kids = read_excel('data/Child population by age group.xlsx')
  return(kids)
}

nation_data <- function() {
  #national dataset
  nation_df<-read_excel("data/national_afcars_trends_2009_through_2018.xlsx",sheet="Data")
  return(nation_df)
}

state_data <- function() {

#State dataset
#Numbers of Children Served in Foster Care, by State
state_served <- read_excel("data/afcars_state_data_tables_09thru18.xlsx",range="Served!A8:K60") %>%
  gather(year,Served,'FY 2009':'FY 2018')

#Numbers of Children in Foster Care on September 30th, by State
state_inCare <- read_excel("data/afcars_state_data_tables_09thru18.xlsx",range="In Care on September 30th!A8:K60") %>%
  gather(year,InCare_Sep30,'FY 2009':'FY 2018')

#Numbers of Children Entering Foster Care, by State
state_entered <- read_excel("data/afcars_state_data_tables_09thru18.xlsx",range="Entered!A8:K60") %>%
  gather(year,Entered,'FY 2009':'FY 2018')

#Numbers of Children Exiting Foster Care, by State
state_exited <- read_excel("data/afcars_state_data_tables_09thru18.xlsx",range="Exited!A8:K60") %>%
  gather(year,Exited,'FY 2009':'FY 2018')

#Numbers of Children Waiting for Adoption, by State
state_waitingAdoption <- read_excel("data/afcars_state_data_tables_09thru18.xlsx",range="Waiting for Adoption!A8:K60") %>%
  gather(year,Waiting_Adoption,'FY 2009':'FY 2018')

#Numbers of Children Waiting for Adoption Whose Parental Rights Have Been Terminated, by State
state_parentalRightsTerminated <- read_excel("data/afcars_state_data_tables_09thru18.xlsx",range="Parental Rights Terminated!A8:K60") %>%
  gather(year,parental_rights_terminated,'FY 2009':'FY 2018')

#Numbers of Children Adopted, by State
state_adopted <- read_excel("data/afcars_state_data_tables_09thru18.xlsx",range="Adopted!A8:K60") %>%
  gather(year,adopted,'FY 2009':'FY 2018')

merge_cols<-c("State","year")
#The merge argument only takes two values as input, so you have to do them separately:
#state_df<- merge(state_served,state_inCare,state_entered,state_exited,state_waitingAdoption,state_parentalRightsTerminated,state_adopted,by=c("State","year"))

state_df<- merge(state_served,state_inCare,by=merge_cols)
state_df<- merge(state_df,state_entered,by=merge_cols)
state_df<- merge(state_df,state_exited,by=merge_cols)
state_df<- merge(state_df,state_waitingAdoption,by=merge_cols)
state_df<- merge(state_df,state_parentalRightsTerminated,by=merge_cols)
state_df<- merge(state_df,state_adopted,by=merge_cols)

## Transfor the year to number in state data
state_df <- state_df %>% mutate(year = as.numeric(substr(year,3,7)))

return(state_df)
}

choiceNames <- c("Served" = "Served",
                 "In Care as of Sep 30" = "InCare_Sep30",
                 "Entered" = "Entered",
                 "Exited" = "Exited",
                 "Waiting for Adoption" = "Waiting_Adoption",
                 "Parental Rights Terminated" = "parental_rights_terminated",
                 "Adopted" = "adopted"
)

state_nation_data <- function() {
  #national dataset
  nation_data<-read_excel("data/national_afcars_trends_2009_through_2018.xlsx",sheet="Data")
  
  #State dataset
  #Numbers of Children Served in Foster Care, by State
  state_served <- read_excel("data/afcars_state_data_tables_09thru18.xlsx",range="Served!A8:K60") %>%
    gather(year,Served,'FY 2009':'FY 2018')
  
  #Numbers of Children in Foster Care on September 30th, by State
  state_inCare <- read_excel("data/afcars_state_data_tables_09thru18.xlsx",range="In Care on September 30th!A8:K60") %>%
    gather(year,InCare_Sep30,'FY 2009':'FY 2018')
  
  #Numbers of Children Entering Foster Care, by State
  state_entered <- read_excel("data/afcars_state_data_tables_09thru18.xlsx",range="Entered!A8:K60") %>%
    gather(year,Entered,'FY 2009':'FY 2018')
  
  #Numbers of Children Exiting Foster Care, by State
  state_exited <- read_excel("data/afcars_state_data_tables_09thru18.xlsx",range="Exited!A8:K60") %>%
    gather(year,Exited,'FY 2009':'FY 2018')
  
  #Numbers of Children Waiting for Adoption, by State
  state_waitingAdoption <- read_excel("data/afcars_state_data_tables_09thru18.xlsx",range="Waiting for Adoption!A8:K60") %>%
    gather(year,Waiting_Adoption,'FY 2009':'FY 2018')
  
  #Numbers of Children Waiting for Adoption Whose Parental Rights Have Been Terminated, by State
  state_parentalRightsTerminated <- read_excel("data/afcars_state_data_tables_09thru18.xlsx",range="Parental Rights Terminated!A8:K60") %>%
    gather(year,parental_rights_terminated,'FY 2009':'FY 2018')
  
  #Numbers of Children Adopted, by State
  state_adopted <- read_excel("data/afcars_state_data_tables_09thru18.xlsx",range="Adopted!A8:K60") %>%
    gather(year,adopted,'FY 2009':'FY 2018')
  
  ## Merge the data for all categories for states
  merge_cols<-c("State","year")
  state_data<- merge(state_served,state_inCare,by=merge_cols)
  state_data<- merge(state_data,state_entered,by=merge_cols)
  state_data<- merge(state_data,state_exited,by=merge_cols)
  state_data<- merge(state_data,state_waitingAdoption,by=merge_cols)
  state_data<- merge(state_data,state_parentalRightsTerminated,by=merge_cols)
  state_data<- merge(state_data,state_adopted,by=merge_cols)
  
  ## Transfor the year to number in state data
  state_data <- state_data %>% mutate(year = as.numeric(substr(year,3,7)))
  
  ## Spread the national data
  nation_data <- spread(nation_data,Population,Counts)
  
  ## Add a State column and rename the columns to match with state_data
  nation_data['State'] = 'Nation'
  nation_data <- nation_data %>% 
    rename(year = FY) %>% 
    rename(InCare_Sep30 = "In Care On Sept 30th") %>%
    rename(Waiting_Adoption = "Waiting For Adoption") %>%
    rename(parental_rights_terminated = "Termination Of Parental Rights") %>%
    rename(adopted = Adopted)

  ## Bind Nation and State Data
  state_nation_data <- rbind(state_data,nation_data)
  
  return(state_nation_data)
}

