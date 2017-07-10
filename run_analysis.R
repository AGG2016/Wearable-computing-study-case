################ FIRST PART ################

#Common to both
activities<-read.table("./activity_labels.txt")#This are row labels
activityNames<-activities$V2
variables<-read.table("./features.txt")#Columns' names

#Let's put the train set together, first

######## TRAIN ########
subjects<-read.table("./train/subject_train.txt")#Row's IDs
features<-read.table("./train/X_train.txt")#Observations
labels<-read.table("./train/y_train.txt")#Activities (codes)

#Assign variables' names
colnames(features)<-variables$V2

#Replace activities codes with labels
library(plyr)
labels$V1<-as.factor(labels$V1)
labels$V1 <- revalue(labels$V1, c("1"="Walking",
                                  "2"="Walking Upstairs",
                                  "3"="Walking Downstairs",
                                  "4"="Sitting",
                                  "5"="Standing",
                                  "6"="Laying"))

#Rename Activities and IDs, and adds them to the main dataset
library(dplyr)
labels <- rename(labels,"Activity"=V1)
features <- cbind(features,labels)
subjects <- rename(subjects,"Subject ID"=V1)
features <- cbind(features,subjects)

######## TEST ########
subjectst<-read.table("./test/subject_test.txt")#Row's IDs
featurest<-read.table("./test/X_test.txt")#Observations
labelst<-read.table("./test/y_test.txt")#Activities (codes)

#Assign variables' names
colnames(featurest)<-variables$V2

#Replace activities codes with labels
labelst$V1<-as.factor(labelst$V1)
labelst$V1 <- revalue(labelst$V1, c("1"="Walking",
                                  "2"="Walking Upstairs",
                                  "3"="Walking Downstairs",
                                  "4"="Sitting",
                                  "5"="Standing",
                                  "6"="Laying"))

#Rename Activities and IDs, and adds them to the main dataset
labelst <- rename(labelst,"Activity"=V1)
featurest <- cbind(featurest,labelst)
subjectst <- rename(subjectst,"Subject ID"=V1)
featurest <- cbind(featurest,subjectst)


####### MERGE #########

complete <- rbind(features,featurest)

#Modifying variables' names with minimum standards
validNames <- make.names(names=names(complete), unique=TRUE, allow_ = TRUE)
colnames(complete) <- validNames

##Caveat: I am well aware that the last lesson of the course
##recommended to eliminate dots and speces from variables'
##names, but I think it is not the best option with such long
#descriptive names.

####### Retaining variables of interest #######

#Method 1
##Caveat: the following was not the method actually used!
selection<-c(1:6,41:46,81:86,121:126,161:166,201,202,214,215,227,228,
             240,241,253,254,266,271,345:350,424:429,503,504,516,517,
             529,530,542,543,562,563)
#In order to double check the variables selected, I read the
#list of features selecting manually the columns to be retained.
#This was not very practical

#Method 2: I did not trust my manual counting of 64 variables
#related to mean a standard deviation of measurements
requested_vars <- grep("mean|std", 
                   names(complete), value = T)
meanFreq_vars <- grep("meanFreq", requested_vars, value = T)
requested_vars <- requested_vars[!requested_vars %in% 
                                         meanFreq_vars]
#Should we drop the variables related to "meanFreq"? I did, but,
#the assignment is not clear enough regarding this. The same with
#variables related to angles, which also include "mean"
#measurements.

#We the selection by means of the 'grep' function, we end up
#with 66 variables, even after dropping the "meanFreq" ones.

#Retaining variables related to mean & standard deviation
#Method 1:
complete1 <- select(complete,selection)
#Method 2:
complete2 <- complete[,c(requested_vars,'Activity','Subject.ID')]

#We extract the variables names to a text file, in order to include them
#in the codebook
varNames<-file("varNames.txt")
writeLines(names(complete2), varNames)
close(varNames)

################ SECOND PART ################

######## Preparing the report requested ########
library(reshape2)
df2 <- melt(complete2,id.vars = c("Subject.ID","Activity"))
df2 <- rename(df2,Variable=variable)
report <- df2 %>%
        group_by(Subject.ID, Activity,Variable) %>%
        summarise(Mean = mean(value))
#Output: mean of each of the variables for every subject, for
#every activity

write.csv(report,"FinalReport.csv")
