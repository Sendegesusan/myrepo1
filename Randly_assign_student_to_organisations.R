rm(list = ls())


library(tidyverse)
library(readxl)



###**assign students to treatment***

data<-data

placements<-placements

program<-data$program

skill<-placements$Skills

slots<-placements$`NUMBER PLACEMENTS`

#seed<-2003245
# assignments should mimic identical assignment

#** The the function randomly assigns students to organisation if the number of application is greater than**
#**the number of available slots***
#**it also randomly gives organisations students if there were more slots that the available applications**


#** It only requires that the degree programs are exactly the same as skills needed**

############################################################################################
# It takes in five augments that is;          
# two data frames (data-students data, placements - organisations data), 
# program - a vector of all programs for each of the students that applied
# slots - a vector indicating all the slots available at a given organisation
# seed - a random integer to ensure reproducibility
############################################################################################


stratiffied_assignment<-function(data, placements, program, slot,  seed = NULL){

  set.seed(seed)
  # Get the unique values of the  program column in the data frame
  prog_strata <- unique(program)
  skill_strat<-  unique(skill)

  #getting number of programs whose skills are needed at the job
  prog_in_skills<-ifelse(prog_strata%in%skill_strat, prog_strata, NA)

  prog_in_skills<-sort(na.omit(prog_in_skills))

  #getting skills where students applied

  skills_applied<- ifelse(skill_strat%in%prog_strata, skill_strat,NA)
  skills_applied<-sort(na.omit(skills_applied))

  # Get the number of programs
  n_strata <- length(prog_in_skills)

  #defining a data frame to
  assigned_dat<- data.frame(matrix(ncol = (ncol(data)+2)+ncol(placements), nrow = 0))
 # i = 1
  #Loop through each stratum
  for (i in  1:n_strata) {

    # Get the current degree Program
    prog <- prog_in_skills[i]
    #get current skill needed
    skill <- skills_applied[i]
    # Subset the data frame to only the observations in the current program
    prog_data <- data[data$program == prog,]

    #subset skills placement data to observations in the current needed job
    skill_data<- placements[placements$Skills==skill,]
    skill_data <- skill_data |>
                  filter(!is.na(`NUMBER PLACEMENTS`))
    # Expanding skills data to the total number of slots available
    skill_data<- as.data.frame(lapply(skill_data, rep, skill_data$`NUMBER PLACEMENTS`))

    # randomly assigning students to treatment in the case where number of application is grater than available slots
    if(nrow(prog_data)>= nrow(skill_data)){
      # generating a random sample of size equal to the available slots
        prog_sample<-sample(1:nrow(prog_data), nrow(skill_data))
        # using the pipe function to generate two variables in program data that are used for assigning applicants to treatment
        prog_data<-prog_data%>%
        mutate(index = 1:nrow(prog_data), treatment = ifelse(index %in%prog_sample,1,0))%>% # the two variables are index and treatment
        filter(treatment == 1)# extracting out only those assigned to treatment

        assined_data_prog_slots<- cbind(prog_data, skill_data) #combining the data where students are assigned to treatment with that with company information
    }
    # Randomly giving companies students in the case where the available number of slots exceeds applications
    if(nrow(prog_data)<nrow(skill_data)){
      #**assign more to treatment if the number of applicants is odd and half number if the applicants are even**
      
      # assigning students to treatment (all receive)
      prog_data<-prog_data%>%
        mutate(index = 1:nrow(prog_data), treatment = 1)
      # generating a random sample of size equal to the available applications
      skill_sample<-sample(1:nrow(skill_data), nrow(prog_data))
      
      # using the pipe function to generate two variables in placement/skills data that are used for students to organisations 
      skill_data<-skill_data%>%
        mutate(index = 1:nrow(skill_data), given = ifelse(index%in%skill_sample,1,0))%>%# index and given are the two variables used
        filter(given == 1)%>%# filter only information for organisation that have received students
        select(-c(index, given))# drop index and given for easy merging
      assined_data_prog_slots<-cbind(prog_data, skill_data)#combining the data where students are assigned to treatment with that with company information

    }
    
    #Changing variable names in the combined data set to much with the names of the empty data set that was created before the loop
    colnames(assined_data_prog_slots) <- names(assigned_dat )
    # Combining the data sets for which students have been allocated to organisations at program level into an overall data set 
    
    assigned_dat <- rbind(assigned_dat, assined_data_prog_slots)# this is the resulting data frame from the for loop

  }
  #Renaming variables in the resulting data frame to the variable names in the original data set
  
  names(assigned_dat)<-c(names(prog_data), names(skill_data))
  
  # Returning the overall data frame
  return(assigned_dat)
}


#**Demo use of the generated function **
ass_data<-stratiffied_assignment(data, program, placements = placements, skill, seed = 729167)
