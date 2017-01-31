##title: "Data Science Graduate Programs"

library(ggplot2)
library(plotly)
library(tidyr)
library(dplyr)
library(magrittr)
library(ggmap)
library(stringr)
library(DT)
library(splitstackshape)
library(ggthemes)

#Load dataset
DS <- read.csv("C:/Users/komal.ingale/Downloads/dataScienceData.csv", stringsAsFactors = FALSE)
DS$STATE %<>% as.factor()

#Code for Graph Representing Number of Graduate Programs of Students By State
NBS <- DS
NBS %<>% distinct(STATE, SCHOOL, CITY, PROGRAM)
Num_By_State <- ggplot(NBS, aes(x=STATE)) +
  geom_bar(stat="count", aes(fill=..count..)) +
  scale_fill_gradient("Count", low="blue", high = "orange") +
  theme(axis.ticks = element_line(size = 0.8), 
        axis.text.x = element_text(vjust = 0.5, 
                                   angle = 70)) +labs(y = "Number of DS Programs Per State", 
                                                      colour = "+ scale_fill_gradient(\"Count\", low=\"blue\", high = \"orange\")") +
  theme(axis.text.x = element_text(angle = 90))

# Graph Representing Number of Graduate Programs of Students By State
ggplotly(Num_By_State)



#Code for Graph Representing Numbers Available for Each Year

Num_For_Each_Year <- ggplot(DS, aes(x=YEAR)) +
  geom_bar(stat="count", aes(fill=..count..)) +
  scale_fill_gradient("Count", low="blue", high = "orange") +
  theme(plot.title = element_text(face = "bold")) +
  labs(title = "Programs Added By Year", x = "Year Added", y = "Count")


# Graph Representing Numbers Available for Each Year
ggplotly(Num_For_Each_Year)


## Plotted Each School Location on the Map
DS$INTERNATIONAL_STUDENTS %<>% stringr::str_replace("%", replacement="")
DS$INTERNATIONAL_STUDENTS_BY_PERCENT = DS$INTERNATIONAL_STUDENTS
DS %<>% select(-c(INTERNATIONAL_STUDENTS))

DS_RM_NA <- DS %>% remove_missing()
DS_RM_NA %<>% distinct(STATE, SCHOOL, CITY, PROGRAM, .keep_all = TRUE)
DS_RM_NA$INTERNATIONAL_STUDENTS_BY_PERCENT %<>% as.numeric()
for(i in 1:nrow(DS_RM_NA)){
  DS_RM_NA[i, "SCHOOL_PROGRAM"] <- paste(DS_RM_NA[i, "SCHOOL"], DS_RM_NA[i, "PROGRAM"], sep="__") 
}

#Code for School & Program vs. Percentage of International Students: 1st Method
NUM_INTERNATIONAL_STUDENT <- ggplot(DS_RM_NA, aes(x=SCHOOL_PROGRAM, y=INTERNATIONAL_STUDENTS_BY_PERCENT)) +
  geom_bar(stat="identity", aes(fill=DS_RM_NA$INTERNATIONAL_STUDENTS_BY_PERCENT)) +
  scale_fill_gradient("Count", low="blue", high = "orange") + theme(axis.text.x = element_text(angle = 90)) + theme(axis.text.x = element_blank()) + theme(plot.title = element_text(face = "bold")) +labs(title = "School & Program Vs. Percentage of International Students", 
                                                                                                                                                                                                           x = "School & Program", y = "International Students (in %)")
# Graph for School & Program vs. Percentage of International Students: 1st Method
ggplotly(NUM_INTERNATIONAL_STUDENT)


#Code for School & Program vs. Percentage of International Students: 2nd Method
DS_RM_NA$INTERNATIONAL_STUDENTS_BY_PERCENT %<>% as.factor()
NUM_INTERNATIONAL_STUDENT2 <- ggplot(DS_RM_NA, aes(x=INTERNATIONAL_STUDENTS_BY_PERCENT)) +
  geom_bar(stat="count", aes(fill=..count..)) +
  scale_fill_gradient("Count", low="yellow", high = "purple") +
  theme(plot.title = element_text(face = "bold")) +
  labs(title = "Prevalence of International Students (By Percent)", 
       x = "International Students (By Percent)", 
       y = "Count")

# Graph for School & Program vs. Percentage of International Students: 2nd Method
ggplotly(NUM_INTERNATIONAL_STUDENT2)


#Sorted the Data and took a mean of the percentage of international students
DS_RM_NA$INTERNATIONAL_STUDENTS_BY_PERCENT %<>% as.integer()
AVG_SCHOOL_INTERNATIONAL_STUDENT <- DS_RM_NA %>%
  group_by(SCHOOL) %>%
  summarise(MEAN_PERCENT_INTERNATIONAL_STUDENT = mean(INTERNATIONAL_STUDENTS_BY_PERCENT)) %>% 
  arrange(desc(MEAN_PERCENT_INTERNATIONAL_STUDENT))

#Code bar graph of School vs. Mean Percentage of International Students
PLOT_AVG_INTERNATIONAL_STUDENT <- ggplot(AVG_SCHOOL_INTERNATIONAL_STUDENT, aes(x=SCHOOL, y= MEAN_PERCENT_INTERNATIONAL_STUDENT)) +
  geom_bar(stat="identity", aes(fill=AVG_SCHOOL_INTERNATIONAL_STUDENT$MEAN_PERCENT_INTERNATIONAL_STUDENT)) +
  scale_fill_gradient("Count", low="green", high = "red") + theme(axis.text.x = element_text(size = 6, 
                                                                                             angle = 90), plot.title = element_text(face = "bold")) +labs(title = "School Vs. Mean Percent of International Students", 
                                                                                                                                                          y = "Mean Percent of International Students", 
                                                                                                                                                          fill = NULL)
# Bar Graph of School vs. Mean Percentage of International Students
ggplotly(PLOT_AVG_INTERNATIONAL_STUDENT)


#Looking at the Data, there seemed to be a lot of Business Based programs, so I filtered out the ones that had the mentions of Business in the Department,
#Program name, or that had the mention of MBA. I just wanted to see which programs stuck more to Statistics, Analytics and Data Science
DS_PLAY <- DS
DS_PLAY %<>% arrange(desc(row_number()))
DS_PLAY <- cbind(DS_PLAY,(DETECT_BUSINESS_1 = str_detect(DS_PLAY$PROGRAM, "usiness")))
DS_PLAY <- cbind(DS_PLAY,(DETECT_MBA = str_detect(DS_PLAY$PROGRAM, "MBA")))
DS_PLAY <- cbind(DS_PLAY,(DETECT_BUSINESS_2 = str_detect(DS$DEPARTMENT, "usiness")))
#colnames(DS_PLAY) %>% dput()
colnames(DS_PLAY) <- c("SCHOOL", "STATE", "CITY", "NOC", "PROGRAM", "TYPE", "DEPARTMENT", 
                       "DELIVERY", "DURATION", "PREREQ", "LINK", "LOC_LAT", "LOC_LONG", 
                       "WORLD_RANK", "COUNTRY", "TEACHING", "INTERNATIONAL", "RESEARCH", 
                       "CITATIONS", "INCOME", "TOTAL_SCORE", "NUM_STUDENTS", "STUDENT_STAFF_RATIO", 
                       "F_M_RATIO", "YEAR", "timesData", "INTERNATIONAL_STUDENTS_BY_PERCENT", 
                       "DETECT_BUSINESS_1", "DETECT_MBA", "DETECT_BUSINESS_2")
DS_PLAY$DETECT_BUSINESS_1 %<>% as.character()
DS_PLAY$DETECT_BUSINESS_2 %<>% as.character()
DS_PLAY$DETECT_MBA %<>% as.character()

for(i in 1:nrow(DS_PLAY)){
  if(DS_PLAY[i, "DETECT_BUSINESS_1"] == TRUE | DS_PLAY[i, "DETECT_BUSINESS_2"] ==TRUE | DS_PLAY[i, "DETECT_MBA"] == TRUE){
    DS_PLAY[i, "BUSINESS_PROGRAM"] <- "TRUE"
  }else{
    DS_PLAY[i, "BUSINESS_PROGRAM"] <- "FALSE"
  }
}


NO_BUSINESS <- filter(DS_PLAY, BUSINESS_PROGRAM == "FALSE")
NO_BUSINESS %<>% select(-c(DETECT_BUSINESS_1, DETECT_BUSINESS_2, DETECT_MBA, BUSINESS_PROGRAM))

#Then "NO_BUSINESS" ones and saw that there were instances of CS and IT based degrees, so I subsetting them down further in an attempt to make a group of data that stuck closely to Stats, Data Science and Analytics
#sorted it down even further by using distinct to get rid of extra years and turned it into a data table
NO_BUSINESS %<>% arrange(desc(row_number()))
DETECT_STATS = str_detect(NO_BUSINESS$PROGRAM, "tatistics")
DETECT_DATA = str_detect(NO_BUSINESS$PROGRAM, "ata")
DETECT_ANALYTICS = str_detect(NO_BUSINESS$PROGRAM, "nalytics")
NO_BUSINESS <- cbind(NO_BUSINESS, DETECT_STATS, DETECT_DATA, DETECT_ANALYTICS)

for(i in 1:nrow(NO_BUSINESS)){
  if(NO_BUSINESS[i, "DETECT_STATS"] == TRUE | NO_BUSINESS[i, "DETECT_DATA"] == TRUE | NO_BUSINESS[i, "DETECT_ANALYTICS"] == TRUE){
    NO_BUSINESS[i, "STATS_DS_ANALYTICS"] <- "TRUE"
  }else{
    NO_BUSINESS[i, "STATS_DS_ANALYTICS"] <- "FALSE"
  }
}

STAT_DS_ANALYTICS <- filter(NO_BUSINESS, STATS_DS_ANALYTICS == "TRUE")
STAT_DS_ANALYTICS %<>% select(-c(DETECT_STATS,DETECT_DATA, DETECT_ANALYTICS))
STAT_DS_ANALYTICS %<>% 
  group_by(SCHOOL, PROGRAM) %>%
  arrange(desc(SCHOOL, YEAR))
SDA_NO_DUPES <- distinct(STAT_DS_ANALYTICS, SCHOOL, PROGRAM, .keep_all = TRUE)

DT::datatable(SDA_NO_DUPES, options = list(searching=TRUE), filter= 'top')