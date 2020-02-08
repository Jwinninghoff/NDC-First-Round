## First task: Show the change over time in remedial course-taking among deaf 
## undergraduate students from 1996-2016.

## Analysis tool: PowerStats:
## https://nces.ed.gov/datalab/index.aspx
## Dataset: National Postsecondary Student Aid Study (NPSAS), Undergraduates

## Using the NPSAS dataset compare the percentage of deaf undergraduate 
## students who took any remedial course in 2016 to those who took any 
## remedial course in 1996.

## Develop a way to visually represent this data that might be used by our 
## social media team to spotlight this information. This can simply be a 
## sketch or it can be a graph or visualization that you create.

## Obtain raw datasets
C <- c("NCES-1996.csv","NCES-2000.csv","NCES-2004.csv","NCES-2008.csv","NCES-2012.csv",
       "NCES-2016.csv")
for(i in C){paste(getwd(),"/Raw datasets/",C,sep="")->files}

read.csv(files[1])->doc1;read.csv(files[2])->doc2;read.csv(files[3])->doc3;
read.csv(files[4])->doc4;read.csv(files[5])->doc5;read.csv(files[6])->doc6

## Clean datasets
library(dplyr)

slice(doc1,15)$X.1%>%as.character()%>%as.numeric()*.01->value96;
slice(doc2,15)$X.1%>%as.character()%>%as.numeric()*.01->value00;
slice(doc3,14)$X.1%>%as.character()%>%as.numeric()*.01->value04;
slice(doc4,15)$X.1%>%as.character()%>%as.numeric()*.01->value08;
slice(doc5,15)$X.1%>%as.character()%>%as.numeric()*.01->value12;
slice(doc6,15)$X.1%>%as.character()%>%as.numeric()*.01->value16

Year <- c(1996,2000,2004,2008,2012,2016)
`Percentage of Remedial Course Taking` <- c(value96,value00,value04,value08,value12,
                                            value16)

round(`Percentage of Remedial Course Taking`,digits=2)->`Percentage of Remedial Course Taking`
cbind(Year,`Percentage of Remedial Course Taking`)%>%as.data.frame->dat

## Create visualized plot
library(ggplot2)
library(plotly)

ggplotly(ggplot(dat,aes(x=Year,y=`Percentage of Remedial Course Taking`))+
  geom_line(col=alpha("#EC5937",0.6),size=2)+geom_point(col="#930606",size=3.2)+
  labs(title="Remedial Course-Taking Among Deaf Undergraduate Students")+
  theme_minimal(base_family = "courier"))

## Second Task Provide a descriptive data report related to employment and educational outcomes, 
## and disparities thereof between deaf and hearing people. 

## Analysis tool: U.S. Census data:
## https://data.census.gov/mdat/#/
## Dataset: 2017 American Community Survey (ACS) 5-year Estimates- Public Use Microdata Sample

## Using the ACS 5-year dataset, research a few education or employment-related data points of your choice and provide a descriptive data report about the differences
## between hearing and deaf people. Please limit the scope of your inquiry to ages 18-64 or a more narrow age range as you see fit. 

## Helpful Hints:
## The variable for deaf people is DEAR. 
## You can create a custom group by clicking on the variable. For age, customize the AGEP variable.
## OCCP allows you to disaggregate
## by industry code.

## Obtain raw dataset
paste(getwd(),"/Raw datasets/Census.csv",sep="")->read
read.csv(read)->doc7

## Clean dataset and then separate and categorize datasets
library(dplyr)
slice(doc7,c(6:7,9:12,14:16,22,24:27,29:32,34:36))->doc7
slice(doc7,3:5)->nodiploma
slice(doc7,7:9)->highdiploma
slice(doc7,11:13)->BA
slice(doc7,15:17)->MA
slice(doc7,19:21)->PHD

## Convert numeric vectors into percentage based on employment-population ratio
sapply(nodiploma[,2:3],as.character)->nodiploma[,2:3]
sapply(nodiploma[,2:3],as.numeric)->nodiploma[,2:3]
for(i in 2:3){
  nodiploma[,i]/sum(nodiploma[,i])->nodiploma[,i]
}
sapply(highdiploma[,2:3],as.character)->highdiploma[,2:3]
sapply(highdiploma[,2:3],as.numeric)->highdiploma[,2:3]
for(i in 2:3){
  highdiploma[,i]/sum(highdiploma[,i])->highdiploma[,i]
}
sapply(BA[,2:3],as.character)->BA[,2:3]
sapply(BA[,2:3],as.numeric)->BA[,2:3]
for(i in 2:3){
  BA[,i]/sum(BA[,i])->BA[,i]
}
sapply(MA[,2:3],as.character)->MA[,2:3]
sapply(MA[,2:3],as.numeric)->MA[,2:3]
for(i in 2:3){
  MA[,i]/sum(MA[,i])->MA[,i]
}
sapply(PHD[,2:3],as.character)->PHD[,2:3]
sapply(PHD[,2:3],as.numeric)->PHD[,2:3]
for(i in 2:3){
  PHD[,i]/sum(PHD[,i])->PHD[,i]
}

## From now on, set up the barplots
library(plotly)

Animals <- c("giraffes", "orangutans", "monkeys")
SF_Zoo <- c(20, 14, 23)
LA_Zoo <- c(12, 18, 29)
data <- data.frame(Animals, SF_Zoo, LA_Zoo)

f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
)

`Education Level`<-c("No Diploma","High School Diploma","Bachelor's Degree","Master's Degree","Doctorate Degree")
Hearing <- c(round(unlist(nodiploma)[7],digits=2),round(unlist(highdiploma)[7],digits=2),
             round(unlist(BA)[7],digits=2),round(unlist(MA)[7],digits=2),round(unlist(PHD)[7],digits=2))
Deaf <- c(round(unlist(nodiploma)[4],digits=2),round(unlist(highdiploma)[4],digits=2),
          round(unlist(BA)[4],digits=2),round(unlist(MA)[4],digits=2),round(unlist(PHD)[4],digits=2))

dat <- data.frame(`Education Level`,Deaf,Hearing)

plot_ly(dat, x = ~`Education Level`, y = ~Deaf, type = 'bar', name = 'Deaf') %>%
  add_trace(y = ~Hearing, name = 'Hearing') %>%
  layout(yaxis = list(title = 'Percentage'),xaxis=list(title='',categoryorder = "array",
        categoryarray = ~`Education Level`),title='Employment Group Ratio: Compare Deaf 
        or Hard of Hearing to Hearing',barmode = 'group')




