
##Filtering out charter schools

library(dplyr)
DSTAF <- read.csv("C:/Users/Justin/Downloads/DSTAF.csv")
assign_allroles_1419 <- read.csv("C:/Users/Justin/Downloads/assign_allroles_1419.csv")
teachers = assign_allroles_1419 %>% filter(grepl("TEACHER", role))
DSTAFret = DSTAF[,c(4,93)]
District.Names <- read.csv("C:/Users/Justin/Downloads/District Names - dref.csv")
merge = merge(DSTAFret, District.Names, by.x = "DISTNAME", by.y = "NAME.1")
merge = merge[,1:2]
merge[,2] = as.numeric(merge[,2])
table(merge$DISTNAME)
merge[duplicated(merge$DISTNAME),]


library(tidyverse)
unique <- distinct(teachers, Alt_ID, AY, .keep_all = TRUE)
unique = unique[unique$district.name %in% merge$DISTNAME, ]

##TOTAL POPULATION (UNUSED)

pivot_table <- pivot_wider(unique, id_cols = Alt_ID, names_from = AY, values_from = campus.id)
pivot_table_district <- pivot_wider(unique, id_cols = Alt_ID, names_from = AY, values_from = district.name)

pivot_table_1416 <- subset(pivot_table_district, select = -c(4:6))
pivot_table_1416 = pivot_table_1416[!is.na(pivot_table_1416$`2014-15`),]
pivot_table_1416[is.na(pivot_table_1416)] <- as.character(0)
comp1416 = ifelse(pivot_table_1416$`2014-15` == pivot_table_1416$`2015-16`, 1, 0)
table1416 = as.data.frame(table(comp1416))
Freq1416 = table1416[1,2]/(table1416[1,2] + table1416[2,2])

pivot_table_1517 <- subset(pivot_table_district, select = -c(2,5:6))
pivot_table_1517 = pivot_table_1517[!is.na(pivot_table_1517$`2015-16`),]
pivot_table_1517[is.na(pivot_table_1517)] <- as.character(0)
comp1517 = ifelse(pivot_table_1517$`2015-16` == pivot_table_1517$`2016-17`, 1, 0)
table1517 = as.data.frame(table(comp1517))
Freq1517 = table1517[1,2]/(table1517[1,2] + table1517[2,2])

pivot_table_1618 <- subset(pivot_table_district, select = -c(2:3,6))
pivot_table_1618 = pivot_table_1618[!is.na(pivot_table_1618$`2016-17`),]
pivot_table_1618[is.na(pivot_table_1618)] <- as.character(0)
comp1618 = ifelse(pivot_table_1618$`2016-17` == pivot_table_1618$`2017-18`, 1, 0)
table1618 = as.data.frame(table(comp1618))
Freq1618 = table1618[1,2]/(table1618[1,2] + table1618[2,2])

pivot_table_1719 <- subset(pivot_table_district, select = -c(2:4))
pivot_table_1719 = pivot_table_1719[!is.na(pivot_table_1719$`2017-18`),]
pivot_table_1719[is.na(pivot_table_1719)] <- as.character(0)
comp1719 = ifelse(pivot_table_1719$`2017-18` == pivot_table_1719$`2018-19`, 1, 0)
table1719 = as.data.frame(table(comp1719))
Freq1719 = table1719[1,2]/(table1719[1,2] + table1719[2,2])

##DISTRICT TURNOVER

pivot2 = pivot_wider(unique, id_cols = c(Alt_ID, district.name), names_from = AY, values_from = district.id)

##1416#####################################################
pivot2_1416 = subset(pivot2, select = -c(5:7))
pivot2_1416 = pivot2_1416[!is.na(pivot2_1416$`2014-15`),]
pivot2_1416[is.na(pivot2_1416)] = 0
pivot2_1416$comp1416 = ifelse(pivot2_1416$`2014-15` == pivot2_1416$`2015-16`, 1, 0)
DistrictPivot1416 = pivot2_1416 %>%
  group_by(district.name, comp1416) %>%
  summarize(total = n())

x = as.data.frame(table(DistrictPivot1416$district.name))
x[x$Freq == 1,]
y = rbind(x,x) %>% arrange(Var1)
y$Freq = 0
y = rename(y, c("comp1416" = "Freq", "district.name" = "Var1"))
y$comp1416 = c(rep(0,1), NROW(y)/2)/NROW(x)
z = merge(DistrictPivot1416, y, all = TRUE)
z[is.na(z$total),]
z[is.na(z)] = 0
DistrictPivot1416 = z

District1416 = lag(DistrictPivot1416$total)/(lag(DistrictPivot1416$total)+DistrictPivot1416$total)
District1416 = as.data.frame(District1416)
toDelete = seq(1, NROW(District1416),by = 2)
District1416 = District1416[-toDelete, ]
District1416 = as.data.frame(District1416)
DistrictPivot1416r = DistrictPivot1416[-toDelete, ]
District1416$District.Name = DistrictPivot1416r$district.name

##1517###################################################
pivot2_1517 = subset(pivot2, select = -c(3, 6:7))
pivot2_1517 = pivot2_1517[!is.na(pivot2_1517$`2015-16`),]
pivot2_1517[is.na(pivot2_1517)] = 0
pivot2_1517$comp1517 = ifelse(pivot2_1517$`2015-16` == pivot2_1517$`2016-17`, 1, 0)
DistrictPivot1517 = pivot2_1517 %>%
  group_by(district.name, comp1517) %>%
  summarize(total = n())

x = as.data.frame(table(DistrictPivot1517$district.name))
x[x$Freq == 1,]
y = rbind(x,x) %>% arrange(Var1)
y$Freq = 0
y = rename(y, c("comp1517" = "Freq", "district.name" = "Var1"))
y$comp1517 = c(rep(0,1), NROW(y)/2)/NROW(x)
z = merge(DistrictPivot1517, y, all = TRUE)
z[is.na(z$total),]
z[is.na(z)] = 0
DistrictPivot1517 = z

District1517 = lag(DistrictPivot1517$total)/(lag(DistrictPivot1517$total)+DistrictPivot1517$total)
District1517 = as.data.frame(District1517)
toDelete = seq(1, NROW(District1517),by = 2)
District1517 = District1517[-toDelete, ]
District1517 = as.data.frame(District1517)
DistrictPivot1517r = DistrictPivot1517[-toDelete, ]
District1517$District.Name = DistrictPivot1517r$district.name

##1618####################################################
pivot2_1618 = subset(pivot2, select = -c(3:4, 7))
pivot2_1618 = pivot2_1618[!is.na(pivot2_1618$`2016-17`),]
pivot2_1618[is.na(pivot2_1618)] = 0
pivot2_1618$comp1618 = ifelse(pivot2_1618$`2016-17` == pivot2_1618$`2017-18`, 1, 0)
DistrictPivot1618 = pivot2_1618 %>%
  group_by(district.name, comp1618) %>%
  summarize(total = n())

x = as.data.frame(table(DistrictPivot1618$district.name))
x[x$Freq == 1,]
y = rbind(x,x) %>% arrange(Var1)
y$Freq = 0
y = rename(y, c("comp1618" = "Freq", "district.name" = "Var1"))
y$comp1618 = c(rep(0,1), NROW(y)/2)/NROW(x)
z = merge(DistrictPivot1618, y, all = TRUE)
z[is.na(z$total),]
z[is.na(z)] = 0
DistrictPivot1618 = z

District1618 = lag(DistrictPivot1618$total)/(lag(DistrictPivot1618$total)+DistrictPivot1618$total)
District1618 = as.data.frame(District1618)
toDelete = seq(1, NROW(District1618),by = 2)
District1618 = District1618[-toDelete, ]
District1618 = as.data.frame(District1618)
DistrictPivot1618r = DistrictPivot1618[-toDelete, ]
District1618$District.Name = DistrictPivot1618r$district.name

##1719######################################################
pivot2_1719 = subset(pivot2, select = -c(3:5))
pivot2_1719 = pivot2_1719[!is.na(pivot2_1719$`2017-18`),]
pivot2_1719[is.na(pivot2_1719)] = 0
pivot2_1719$comp1719 = ifelse(pivot2_1719$`2017-18` == pivot2_1719$`2018-19`, 1, 0)
DistrictPivot1719 = pivot2_1719 %>%
  group_by(district.name, comp1719) %>%
  summarize(total = n())

x = as.data.frame(table(DistrictPivot1719$district.name))
x[x$Freq == 1,]
y = rbind(x,x) %>% arrange(Var1)
y$Freq = 0
y = rename(y, c("comp1719" = "Freq", "district.name" = "Var1"))
y$comp1719 = c(rep(0,1), NROW(y)/2)/NROW(x)
z = merge(DistrictPivot1719, y, all = TRUE)
z[is.na(z$total),]
z[is.na(z)] = 0
DistrictPivot1719 = z

District1719 = lag(DistrictPivot1719$total)/(lag(DistrictPivot1719$total)+DistrictPivot1719$total)
District1719 = as.data.frame(District1719)
toDelete = seq(1, NROW(District1719),by = 2)
District1719 = District1719[-toDelete, ]
District1719 = as.data.frame(District1719)
DistrictPivot1719r = DistrictPivot1719[-toDelete, ]
District1719$District.Name = DistrictPivot1719r$district.name

#################################################################

ifelse(DistrictPivot1416r$district.name == DistrictPivot1517r$district.name, "Yes", "No")

DistrictPivot1517 = insertRows(DistrictPivot1517, 1133, new = list("MANOR ISD", 1, 0))
DistrictPivot1517 = insertRows(DistrictPivot1517, 1133, new = list("MANOR ISD", 0, 0))

ifelse(DistrictPivot1416r$district.name == DistrictPivot1517r$district.name, "Yes", "No")

DistrictPivot1416 = insertRows(DistrictPivot1416, 1405, new = list("PETROLIA CISD", 1, 0))
DistrictPivot1416 = insertRows(DistrictPivot1416, 1405, new = list("PETROLIA CISD", 0, 0))

ifelse(DistrictPivot1416r$district.name == DistrictPivot1517r$district.name, "Yes", "No")
ifelse(DistrictPivot1517r$district.name == DistrictPivot1618r$district.name, "Yes", "No")
ifelse(DistrictPivot1416r$district.name == DistrictPivot1618r$district.name, "Yes", "No")
ifelse(DistrictPivot1416r$district.name == DistrictPivot1719r$district.name, "Yes", "No")
ifelse(DistrictPivot1517r$district.name == DistrictPivot1719r$district.name, "Yes", "No")
ifelse(DistrictPivot1618r$district.name == DistrictPivot1719r$district.name, "Yes", "No")

################################################################

DistrictAll = merge(District1416, District1517)
DistrictAll = merge(DistrictAll, District1618)
DistrictAll = merge(DistrictAll, District1719)

merge$DPSTURNR = merge$DPSTURNR/100
ifelse(DistrictAll$District.Name == merge$DISTNAME, "Yes", "No")
merge = merge %>% distinct(DISTNAME, .keep_all = TRUE)
ifelse(DistrictAll$District.Name == merge$DISTNAME, "Yes", "No")
merge = merge[-904,]
rownames(merge) = NULL
ifelse(DistrictAll$District.Name == merge$DISTNAME, "Yes", "No")

DistrictAll = merge(DistrictAll, merge, by.x = "District.Name", by.y = "DISTNAME")
DistrictAll = DistrictAll %>% rename("District2022" = "DPSTURNR")
#DistrictAll = DistrictAll[!duplicated(DistrictAll$District.Name),]
DistrictAll = DistrictAll %>% mutate(across(where(is.numeric), round, 3))
DistrictAll[is.na(DistrictAll)] = 0

write.csv(DistrictAll, file = "DistrictAll.csv")

##VISUALIZATIONS

library(reshape2)
library(factoextra)
library(ISLR)
library(tidyverse)

melt = melt(DistrictAll, id.vars = "District.Name", variable.name = "school_year")

any(melt$value == 1)

write.csv(melt, file = "melt.csv")

#KMEANS CLUSTERING

DistrictAllScale = DistrictAll
DistrictAllScale[2:6] = as.data.frame(scale(DistrictAllScale[2:6]))
DistrictAllScale = DistrictAllScale %>% remove_rownames %>% column_to_rownames(var = "District.Name")
distance = get_dist(DistrictAllScale)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k.max = 10
wss = numeric(k.max)
for(k in 1:k.max){
  wss[k] = eclust(DistrictAllScale,
                  FUNcluster = "kmeans",
                  k = k,
                  nstart = 50,
                  graph=0)$tot.withinss
}
plot(wss, type = "b")
km.out = eclust(DistrictAllScale[2:6], FUNcluster = "kmeans",
                nstart = 20,
                nboot = 50)
fviz_nbclust(DistrictAllScale, kmeans, nstart = 20, nboot = 50, method = "gap_stat")

km.obj = eclust(DistrictAllScale, FUNcluster = "kmeans", k = 3, nstart = 20)

km.obj
km.obj$clust_plot
fviz_cluster(km.obj, data = DistrictAllScale, labelsize = 0)

DistrictAllFake = DistrictAll
DistrictAllFake$cluster = km.obj$cluster
write.csv(DistrictAllFake, file = "DistrictAllFake.csv")
melt2 = melt(DistrictAllFake, id.vars = c("District.Name", "cluster"), variable.name = "school_year")
write.csv(melt2, file = "melt2.csv")

##df %>%
##  as_tibble() %>%
##  mutate(cluster = km.obj$cluster,
##         district = row.names(DistrictAllScale)) %>%
##  ggplot(aes(District1416, District1417, color = factor(cluster), label = district)) +
##  geom_text()

library(compareGroups)
DistrictAllCluster = cbind(DistrictAll, cluster = km.obj$cluster)
DistrictAllCluster = DistrictAllCluster %>% remove_rownames %>% column_to_rownames(var = "District.Name")
comparegroups.main = compareGroups(formula = cluster~., data = DistrictAllCluster, max.ylev = 10)
comparegroups.main
comparegroups.main.table = createTable(x = comparegroups.main, show.all = T, show.p.overall = F)
comparegroups.main.tableF = createTable(x = comparegroups.main, show.p.overall = F)
comparegroups.main.table
comparegroups.main.tableF
library(knitr)
comparegroups.html = suppressWarnings(
  export2md(
    x             = comparegroups.main.table,
    caption       = "",
    header.labels = c(
      "all"       = "All",
      "p.overall" = "p-value"
    )
  )
)
comparegroups.html
export2csv(comparegroups.main.tableF, file = "compareclusters.csv")

#Distributions

library(ggplot2)

g1 = ggplot(DistrictAll, aes(x = District1416)) + 
  geom_histogram(binwidth = 0.05, color = "blue", fill = "green") +
  labs(title = "2014-2016 Attrition Distribution", x = "Attrition rate", y = "Count") +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1))

g1 + facet_grid(rows = vars())

g2 = ggplot(DistrictAll, aes(x = District1517)) + 
  geom_histogram(binwidth = 0.05, color = "blue", fill = "green") +
  labs(title = "2015-2017 Attrition Distribution", x = "Attrition rate", y = "Count") + 
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  coord_cartesian(xlim = c(0, 1))

g3 = ggplot(DistrictAll, aes(x = District1618)) + 
  geom_histogram(binwidth = 0.05, color = "blue", fill = "green") +
  labs(title = "2016-2018 Attrition Distribution", x = "Attrition rate", y = "Count") +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  coord_cartesian(xlim = c(0, 1))

g4 = ggplot(DistrictAll, aes(x = District1719)) + 
  geom_histogram(binwidth = 0.05, color = "blue", fill = "green") +
  labs(title = "2017-2019 Attrition Distribution", x = "Attrition rate", y = "Count") + 
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  coord_cartesian(xlim = c(0, 1))

g5 = ggplot(DistrictAll, aes(x = District2022)) + 
  geom_histogram(binwidth = 0.05, color = "blue", fill = "green") +
  labs(title = "2021-2022 Attrition Distribution", x = "Attrition rate", y = "Count") + 
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  coord_cartesian(xlim = c(0, 1))

library("gridExtra")

grid.arrange(g1,g2,g3,g4,g5, nrow = 3, ncol = 2)
