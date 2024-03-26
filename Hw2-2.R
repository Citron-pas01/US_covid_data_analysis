ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, library, character.only = TRUE)
}

packages <- c("ggplot2", "dplyr", "tidyverse", "nycflights13", "gapminder","data.table","reshape2","openintro","readxl","stringr")
ipak(packages)

uspop <- read.csv(file ="http://personal.psu.edu/mzm6664/uspop_assgn.csv")
metapop <- read.csv(file ="http://personal.psu.edu/mzm6664/meta_assgn.csv")

library(dplyr)
oldGRP = filter(select(uspop,AGEGRP, AGE_GROUP),AGE_GROUP == "Age 65 to 69 years")[1,1]
Oldpop <- filter(uspop,AGEGRP >= oldGRP )
# this is to get a old age population data.

Oldpop =  select(Oldpop, FIPS, STNAME, CTYNAME, AGEGRP, TOT_POP)
# select specific columns with FIPS, STNAME, CTYNAME, 
# AGEGRP and TOT_POP

d <- transform(Oldpop, StateID = FIPS %/% 1000)
# Add a "StateID" column which represent the state name 
# and is derived from FIPS

StateSum <- aggregate(cbind(TOT_POP) ~ StateID, data=d, FUN=sum)
# Sum up the TOT_POP when the rows are in the "StateID"

NewStateSum <- StateSum[order(-StateSum[,2]), ]
# get the desending sequence of the TOT_POP 

dd <- select(d, STNAME, StateID )
# bridge the state name and StateID as one to one relationship

ddd <- distinct(dd, STNAME, StateID, .keep_all = TRUE)
# remove those repeated rows and leave only one "Alabama, 1"

library(tidyverse)
dddd <- filter_at(ddd, vars(starts_with("StateID")), 
                  all_vars(. %in% NewStateSum[1:10,1])) 
# from the sequence of TOT_POP to get the sequence of StateID 
# as well as the State name

TotalPOP = NewStateSum[1:10,2]
dddd <- cbind(dddd, TotalPOP)
# Add the desending TOT_POP sequence to the ranked StateID 
# and state name df.
head(dddd)
#write.csv(dddd,"C:\\Users\\Bo_Ni\\OneDrive\\PSU\\1-Class\\IE 582\\Hw2\\Q2-1.csv", row.names = FALSE)

#-------------------------get the county sequence-----------------------------#
CountySum <- aggregate(cbind(TOT_POP) ~ FIPS, data=Oldpop, FUN=sum)
# Sum up the TOT_POP when the rows are in the "FIPS"

dCd <- select(Oldpop, FIPS, STNAME, CTYNAME)
dCd <- distinct(dCd, FIPS, STNAME,CTYNAME, .keep_all = TRUE)
# bridge the state name and StateID as one to one relationship
# delete the repeated rows

dCCd <- filter_at(dCd, vars(starts_with("FIPS")), 
                  all_vars(. %in% CountySum[,1])) 
# from the sequence of TOT_POP to get the sequence of FIPS
# as well as the State name

TotalPOP = CountySum[,2]
dCounty <- cbind(dCCd, TotalPOP)
dCounty = dCounty[order(-dCounty[,4]), ]
# Add the desending TOT_POP sequence to the ranked StateID 
# and state name df.
head(dCounty[1:10,])
#write.csv(dCounty[1:10,],"C:\\Users\\Bo_Ni\\OneDrive\\PSU\\1-Class\\IE 582\\Hw2\\Q2-1-2.csv", row.names = FALSE)

#-----------------------------------bar graph #1------------------------------#
dbar <- transform(dddd, NEW = TotalPOP / 1000000)
# rescale the TotalPOP to million unit

StatesCounts <- select(dbar, STNAME, NEW)
# set two colums in the df

names(StatesCounts) <- c("x","y")
p1 <- ggplot(StatesCounts, aes(x,y)) +
  geom_bar(fill="green3",color='green3',stat="identity") +
  theme(axis.ticks=element_blank(),
        axis.text.x=element_text(angle=45,hjust=0.8,vjust=1,
                                 size=10,colour = 'black'))
# plot the bar graph
p1 + ggtitle("Top 10 States with Old Population") +
  xlab("Name of State") + ylab("Old Population (in millions)")
# Add the graph title

#-----------------------------------bar graph #2------------------------------#
dbar2 <- transform(dCounty[1:10,], NEW = TotalPOP / 100000)
# rescale the TotalPOP to million unit
CountyCounts <- select(dbar2,CTYNAME, STNAME, NEW)
# set 3 colums in the df

library(ggplot2)
CountyCounts$STNAME = with(CountyCounts, paste0("(", STNAME, ")"))
# add bracket to the state name

CountyCounts$State_county = paste(CountyCounts$CTYNAME,"\n",CountyCounts$STNAME)
# combine the county name and state name in different lines

p2 <- ggplot(CountyCounts, aes(x = reorder(State_county, -NEW),y = NEW)) +
  geom_bar(fill="green3",color='green3',stat="identity") +
  theme(axis.ticks=element_blank(),
        axis.text.x=element_text(angle=45,hjust=0.8,vjust=1,
                                 size=10,colour = 'black'))+
  theme(axis.title.x = element_text(angle=0,hjust=0.5,vjust=9,
                                      size=12,colour = 'black'))+
  theme(axis.title.y = element_text(size=12,colour = 'black'))
# plot the bar graph

p2 + ggtitle("Top 10 Counties with Old Population") +
  xlab("Name of County") + ylab("Old Population (in 100 Ks)")
# Add the graph title


