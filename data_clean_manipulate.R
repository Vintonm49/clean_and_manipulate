# Data Cleaning and Manipulation
# LTC Melanie Vinton
# This is a solution to an exercise presented during a R programming class hosted at the Center for Army Analysis.

data11<-read.csv("Medicare_Provider_Charge_Inpatient_DRG100_FY2011.csv")
data12<-read.csv("Medicare_Provider_Charge_Inpatient_DRG100_FY2012.csv")
data13<-read.csv("Medicare_Provider_Charge_Inpatient_DRG100_FY2013.csv")
data14<-read.csv("Medicare_Provider_Charge_Inpatient_DRGALL_FY2014.csv")

# Split DRG.Definition into 2 variables
library(tidyr)
data11 <- separate(data = data11,col = DRG.Definition,
                   into = c("DRG.Code","DRG.text"), sep="-", remove=FALSE)
data12 <- separate(data = data12,col = DRG.Definition,
                   into = c("DRG.Code","DRG.text"), sep="-", remove=FALSE)
data13 <- separate(data = data13,col = DRG.Definition,
                   into = c("DRG.Code","DRG.text"), sep="-", remove=FALSE)
data14 <- separate(data = data14,col = DRG.Definition,
                   into = c("DRG.Code","DRG.text"), sep="-", remove=FALSE)

head(names(data11),3)


# Top 100 Comparison
library(dplyr)
drgBydischarges_14<- summarise(group_by(data14, DRG.Code),
                               discharges = sum(Total.Discharges))
drgBydischarges_11<- summarise(group_by(data11, DRG.Code),
                               discharges = sum(Total.Discharges))
head(drgBydischarges_11,3)

drgBydischarges_14<-arrange(drgBydischarges_14,desc(discharges))
drgBydischarges_11<-arrange(drgBydischarges_11,desc(discharges))
top100_byDischarges14<-drgBydischarges_14[1:100,]
head(drgBydischarges_11,3)

top100DRG14<-top100_byDischarges14$DRG.Code  #vector of DRG codes
top100DRG11<-drgBydischarges_11$DRG.Code  #vector of DRG codes
topdiff_discharges <- as.data.frame(setdiff(top100DRG11,top100DRG14))

drg11 <- distinct(data11[,2:3])
names(topdiff_discharges)[1]<-"DRG.Code"
topdiff2 <- left_join(topdiff_discharges, drg11)
topdiff2


# DRG Diversity
provider<- summarise(group_by(data14, Provider.Name), distinctDRGs = n_distinct(DRG.Code))
provider<-arrange(provider, desc(distinctDRGs))
provider50<-provider[1:50,]
head(provider50, 3)

provider50 <- provider50[order(-provider50$distinctDRGs),]
provider50$Provider.Name=factor(provider50$Provider.Name,levels=provider50$Provider.Name)

library(ggplot2)
distro<-ggplot(data=provider50, aes(x=factor(Provider.Name),y=distinctDRGs)) +
  coord_flip()+
  geom_bar(colour="black", stat="identity", width = .5) +
  ggtitle("Number of Unique Procedures \nPerformed by Provider")+
  theme(axis.title.x = element_blank()) + #eliminate y axis label
  theme(axis.title.y = element_blank()) +
  #theme(plot.title = element_text(hjust = 0.5)) + #center the title
  theme(axis.text.y = element_text(size=5))
distro


# Cost Variability
co.var <- function(x) {
  100*sd(x)/mean(x)
}

by_DRG<-group_by(data14,DRG.Code)
numProvider<-summarise(by_DRG,providerAmt= n_distinct(Provider.Name))
numProvider2 <- filter(numProvider, providerAmt>=10)
head(numProvider2,2)

costVar <- summarise(by_DRG, coefVar=co.var(Average.Total.Payments))
cost_join <- left_join(numProvider2,costVar)

costVar100 <- arrange(cost_join, desc(coefVar))
costVar100$coefVar <- round(costVar100$coefVar, digits=2)
costVar100<-costVar100[1:100,]
head(costVar100,3)

# Map cost variability by state
library(maps)
statesMap<-map_data("state")
statesMap$state <- state.abb[match(statesMap$region, tolower(state.name))]

by_state<-group_by(data14,Provider.State)
stateVar <- summarise(by_state, coefVar=co.var(Average.Total.Payments))
stateVar$coefVar <- round(stateVar$coefVar, digits=2)
stateVar<-rename(stateVar,state=Provider.State)
choropleth <- merge(statesMap, stateVar, by = "state")

choropleth<-choropleth[order(choropleth$order), ]

state_coVar<-ggplot()+
  geom_polygon(data=choropleth, aes(long,lat, group=group, fill=coefVar),
               color="black",size=.2)+
  coord_map()+
  scale_fill_distiller(palette="Blues", direction = 1)+
  labs(x="", y="",fill="", title="Coeficient of Variation")+ #labels
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(),
        panel.background = element_blank())
state_coVar


# Expensive Providers in NY
percent_compare <- function(x,y) {
  round(100*(x/y),0)
}

nydata<-filter(data14,Provider.State=="NY")

by_DRGny<-group_by(nydata,DRG.Code)
numProviderny<-summarise(by_DRGny,providerAmt= n_distinct(Provider.Name))
ny_join<-left_join(nydata,numProviderny,by="DRG.Code")
ny_join10 <- filter(ny_join, providerAmt>=10)

by_DRGny10<-group_by(ny_join10,DRG.Code)
ny_avgDRG <- summarise(by_DRGny10,avgDRG=mean(Average.Total.Payments))
ny_join10<-left_join(ny_join10,ny_avgDRG,by="DRG.Code")

ny_join10$PerOverunder<-percent_compare(ny_join10$Average.Total.Payments,ny_join10$avgDRG)
summary(ny_join10$PerOverunder)

ny_join10 %>% arrange(desc(PerOverunder)) %>% 
  head(1)[c("DRG.text","Provider.Name","Provider.City","Total.Discharges","Average.Total.Payments","avgDRG")]

by_nyprovider<-group_by(ny_join10,Provider.Name)
by_nyprovider<-summarise(by_nyprovider, AvgOverunder=round(mean(PerOverunder),2))
by_nyprovider<-arrange(by_nyprovider,desc(AvgOverunder))
head(by_nyprovider,3)



