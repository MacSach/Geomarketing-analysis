# Urban mining vs. direct marketing

###### Loading the libraries and WD setting  #####
install.packages("plyr")
install.packages("spdep")
install.packages("rgdal")
install.packages("maptools")
install.packages("sp")
install.packages("RColorBrewer")
install.packages("classInt")
install.packages("GISTools")
install.packages("maps")
install.packages("sf")
install.packages("ggplot2")
install.packages("tmap")
install.packages("osmdata")
install.packages("arules")
install.packages("arulesViz")
install.packages("plyr")
install.packages("igraph")
install.packages("tidygraph")
install.packages("nabor")
install.packages("dbscan")
install.packages("magrittr")
install.packages("showtext")
install.packages("readr")
install.packages("geosphere")

library(readr)
library(spdep)
library(rgdal)
library(maptools)
library(sp)
library(RColorBrewer)
library(classInt)
library(GISTools)
library(maps)
library(sf)
library(ggplot2)
library(tmap)
library(osmdata)
library(googleway)
library(RgoogleMaps)
library(PBSmapping)
library(ggmap)
library(plyr)
library(tidyverse)
library(dbscan)
library(nabor)
library(arules)
library(arulesViz)
library(igraph)
library(tidygraph)
library(magrittr) 	
library(geosphere)
library(showtext)


#Home
setwd("Path to your working directory")
wd = "Path to your working directory"



###### MAPS  ######

# loading the China shapefile on different levels
adm0 = readOGR(".", "adm1") #1
adm1 = readOGR(".", "adm1") #31
adm2 = readOGR(".", "adm2") #344
adm3 = readOGR(".", "adm3") #2408

# creating sf file
adm3.sf = st_read("adm3.shp")
class(adm3)


# map checking
plot(adm0)
plot(adm1)
plot(adm2)
plot(adm3)
# ok

###### Spatial point data  ######
#### Google Maps - mining the Pekin-based places ####
# Registering into the Google Maps Places and registering API key, to be able to use this system.
install.packages('googleway')
install.packages('RgoogleMaps')
install.packages('PBSmapping')
install.packages('ggmap')
install.packages("tidyverese")

# Here is the place to paste the individual API Key
register_google(key="___")

# Setting the borders of our study
Pekin_box = bbox(adm3.pekin)
Pekin_box

# 39.9042° N, 116.4074° E

# Plotting the map as a base for visualisations
MyMap2<-get_map(location=c(116.4074,39.9042), zoom=10, maptype="roadmap", source="google", crop=FALSE) # z pakietu ggmap
plot(MyMap2)

# The available object types in Google Maps Places 
types = c('accounting','
          airport','
          amusement_park','
          aquarium','
          art_gallery','
          atm','
          bakery','
          bank','
          bar','
          beauty_salon','
          bicycle_store','
          book_store','
          bowling_alley','
          bus_station','
          cafe','
          campground','
          car_dealer','
          car_rental','
          car_repair','
          car_wash','
          casino','
          cemetery','
          church','
          city_hall','
          clothing_store','
          convenience_store','
          courthouse','
          dentist','
          department_store','
          doctor','
          drugstore','
          electrician','
          electronics_store','
          embassy','
          fire_station','
          florist','
          funeral_home','
          furniture_store','
          gas_station','
          gym','
          hair_care','
          hardware_store','
          hindu_temple','
          home_goods_store','
          hospital','
          insurance_agency','
          jewelry_store','
          laundry','
          lawyer','
          library','
          light_rail_station','
          liquor_store','
          local_government_office','
          locksmith','
          lodging','
          meal_delivery','
          meal_takeaway','
          mosque','
          movie_rental','
          movie_theater','
          moving_company','
          museum','
          night_club','
          painter','
          park','
          parking','
          pet_store','
          pharmacy','
          physiotherapist','
          plumber','
          police','
          post_office','
          primary_school','
          real_estate_agency','
          restaurant','
          roofing_contractor','
          rv_park','
          school','
          secondary_school','
          shoe_store','
          shopping_mall','
          spa','
          stadium','
          storage','
          store','
          subway_station','
          supermarket','
          synagogue','
          taxi_stand','
          train_station','
          transit_station','
          travel_agency','
          university','
          veterinary_care','
          zoo')

# ~ removed "tourist attraction" because of the error. Continuing without this type, downloaded oonce again without this. ~

tour.df = data.frame()

# Places mining - Pekin
for (type in types) {
  for (i in 1:50) {
    tour = google_places(search_string = type,
                         location=c(39.9042, 116.4074),
                         radius=30000, key=APIKEY,
                         page_token = tour$next_page_token)
    
    tour = tour$results %>%
      cbind(., tour$results$geometry$location)
    
    tour_temp.df = tour %>%
      select(business_status, formatted_address, name, rating, types, user_ratings_total, lat, lng)
    
    tour.df = rbind(tour.df, tour_temp.df)
  }
  
  tour.type = tour.df[!duplicated(tour.df),]
  
  View(tour.type)
  nrow(tour.type)
  
  ggmap(MyMap2) +
    geom_point(data=tour.type,aes(x=lng, y=lat),alpha=0.5, color="darkred", size=2)
}


View(tour.df)

View(tour.df[!duplicated(tour.df),])

# Visualising
ggmap(MyMap2) +
  geom_point(data=tour.df,aes(x=lng, y=lat),alpha=0.5, color="darkred", size=1)


# Removing duplicates
tour.dist.df = distinct(tour.df)

# Creating the one-dimensional table
categories <- unique(unlist(tour.dist.df$types))

# Setting a new frama
tour.dist.df.cat <- tour.dist.df

# Creating a new base with dummy-columns -> 1 if the object is set to a specific object type
for (i in 1:nrow(tour.dist.df.cat)) {
  row_categories <- tour.dist.df.cat$types[[i]]
  for (category in categories) {
    tour.dist.df.cat[i, category] <- ifelse(category %in% row_categories, 1, 0)
  }
}
View(tour.dist.df.cat)

# Setting a new table - limited to only one main object type - tour.dist.df 
tour.dist.df$types_2 <- sapply(tour.dist.df$types, function(x) {
  if (x[1] == "none") {
    return(x[2])
  } else {
    return(x[1])
  }
})

tour.dist.df = distinct(tour.dist.df)
# Printing the results

View(tour.dist.df) 


#### Google Maps Places - exporting downloaded raw bases to CSVs ####
write_csv(tour.dist.df, "tourdistdf.csv")
write_csv(tour.dist.df.cat, "tourdistdfcat.csv")
write_csv(tour.df, "tourfull.csv")

#### Google Maps Places - limiting the base only to the Pekin-based places -> PEKIN_FILTERED ####
tour.dist.df_filtered = tour.dist.df %>%
  filter(between(lat,38,42),
         between(lng,114,117))


tour.dist.df.cat_filtered = tour.dist.df.cat %>%
  filter(between(lat,38,42),
         between(lng,114,117))

write_csv(tour.dist.df_filtered, "pekin_filtered.csv")
write_csv(tour.dist.df.cat_filtered, "pekin_with_cat_filtered.csv")

pekin_filtered = tour.dist.df_filtered



#### Geolife - Loading all the trajectories from the Geolife data base - Part 1 ####
# "landing" frame
all_data = NULL

# iterating by every folder
for (i in 0:181) {
  # setting the number format to 000
  person_id = sprintf("%03d", i)
  
  # loading the data list for a specific user
  trajectories_list = list.files(paste(wd, "Geolife Trajectories 1.3/Data", person_id, "Trajectory", sep = "/"))
  
  # iterating by every file in the specific folder
  for (j in 1:length(trajectories_list)) {
    
    # checking if the file is not "desktop.ini" - we don't need those
    if(trajectories_list[j] != "desktop.ini") {
      
      # loading the data from a specific trajectory file
      data = read.table(paste(wd, "Geolife Trajectories 1.3/Data", person_id, "Trajectory", trajectories_list[j], sep = "/"), header = FALSE, quote = "\"", skip = 6, sep = ",")
      
      # adding the column with the person ID and trajectory number
      data$person_id = i
      data$trajectory_id = paste(i, "_", j, sep = "")
      
      # binding all the data
      all_data = rbind(all_data, data)
    }
  }
}


#### Loading all the trajectories from the Geolife data base - Part 2 [starting from the ID 85] ####

# "landing" frame
all_data2 = NULL

# iterating by every folder
for (i in 85:181) {
  # setting the number format to 000
  person_id = sprintf("%03d", i)
  
  # loading the data list for a specific user
  trajectories_list = list.files(paste(wd, "Geolife Trajectories 1.3/Data", person_id, "Trajectory", sep = "/"))
  
  # iterating by every file in the specific folder
  for (j in 1:length(trajectories_list)) {
    
    # checking if the file is not "desktop.ini" - we don't need those
    if(trajectories_list[j] != "desktop.ini") {
      
      # loading the data from a specific trajectory file
      data = read.table(paste(wd, "Geolife Trajectories 1.3/Data", person_id, "Trajectory", trajectories_list[j], sep = "/"), header = FALSE, quote = "\"", skip = 6, sep = ",")
      
      # adding the column with the person ID and trajectory number
      data$person_id = i
      data$trajectory_id = paste(i, "_", j, sep = "")
      
      # binding all the data
      all_data2 = rbind(all_data2, data)
    }
  }
}

# Binding Part 1 with Part 2
all_data_bind = rbind(all_trajectories, all_trajectories2)
View(distinct(all_data_bind))

all_trajectories = distinct(all_data_bind)

#### Geolife - saving databases to CSVs ###
write_csv(all_trajectories, "all_trajectories.csv")
write_csv(all_data, "all_data_1.csv")
write_csv(all_data2, "all_data_2.csv")


#### Geolife&Google Maps Places - Visualising specific trajectories ####
ggmap(MyMap2) + geom_point(data=all_trajectories %>% 
                             filter(., person_id == "153"),
                           aes(x=V2, y=V1),
                           alpha=0.5, 
                           color="darkred", 
                           size=2)

ggmap(MyMap2) + geom_point(data=tour.dist.df,aes(x=lng, y=lat),alpha=0.5, size=1, colour="red")


#### Geolife&Google Maps Places - Visualising all the trajectories ####
# Warning - big data!
ggmap(MyMap2) + geom_point(data=all_trajectories %>%
                             aes(x=V2, y=V1),
                           alpha=0.5, 
                           color="darkred", 
                           size=2)

all_trajectories = read.csv("all_trajectories.csv", header = TRUE)
all_trajectories = read.csv("all_data_1.csv", header = TRUE)
all_trajectories2 = read.csv("all_data_2.csv", header = TRUE)

coordinates(all_trajectories) = c("V2", "V1")
View(all_trajectories)


#### Geolife trajectories vs objects Google Maps Places - matching similar locations (matching two bases together) ####

install.packages("dbscan")
library(dbscan)

install.packages("nabor")
library(nabor)
# 0.0003


# Find nearest neighbors within XX meters between the pekin_filtered database and all_trajectories database
nn <- knn(pekin_filtered[, c("lat", "lng")], all_trajectories[, c("V1", "V2")], k = 2, radius = 0.0009001)

View(nn$nn.dists)

View(pekin_filtered_df)
View(all_trajectories_df)


match = as.data.frame(nn$nn.idx)
class(match)
sum(match$V5 == 0)

nrow(all_trajectories) - sum(match$V1 == 0)

# For every point from the trajectories database, the two closest objects were found in a set range or none object was found)
# objects have individual IDs

# Adding the column with the closest object ID location 
all_trajectories_matched = all_trajectories
all_trajectories_matched["Location"] = match$V1
View(all_trajectories_matched)

# Filtering only the matched observations
visits = all_trajectories_matched %>%
  filter(., Location > 0)
View(visits)

# Calculating how often there is a specific location
counts <- table(visits$Location)
View(counts)


# Combining tables

# Creating ID for every location
pekin_filtered <- pekin_filtered %>%
  mutate(Location = row_number())
View(pekin_filtered)


visits_merged <- merge(visits, pekin_filtered, by = "Location", all.x = TRUE)


visits_merged <- subset(visits_merged, select = -c(business_status))

# The visits_merged set the object/place info to the matched trajectory-point

# Counting the number of occurrences of each location in each trajectory
visits_merged_counts <- visits_merged %>%
  group_by(trajectory_id, Location) %>%
  summarize(count = n())

View(visits_merged_counts)

# Selecting only lines in which the location appears at least 6 times
visits_merged_filtered <- visits_merged_counts %>%
  filter(count >= 6) %>%
  select(-count) %>%
  inner_join(visits_merged, by = c("trajectory_id", "Location"))

View(visits_merged_filtered)

# CSVs saving
write_csv(visits, "visits_all.csv")
write_csv(visits_merged_filtered, "visits_merged_filtered.csv")
visits_merged_filtered = read.csv("visits_merged_filtered.csv", encoding = "UTF-8")
pekin_filtered = read.csv("pekin_filtered.csv", encoding = "UTF-8")


#### Database for the basket analysis - V1 ####

# Leaving only the most important columns
v1 = visits_merged_filtered %>%
  select(person_id, trajectory_id, name, Location, types_2, V6, V7)

v1 = v1 %>%
  distinct(.,person_id, trajectory_id, name, Location, types_2)

# Reanaming types_2 to types
v1 <- v1 %>% rename(types = types_2)

# Renaming "establishment" to "point_of_interest" in the "types" column
v1 <- v1 %>% mutate(types = ifelse(types == "establishment", "point_of_interest", types))

# CSVs exporting
unique_types <- as.data.frame(unique(v1$types))
unique_types

write_csv(unique_types, "unique_types.csv")
write_csv(v1, "localizations_baskets_v1.csv")


sum = v1 %>%
  group_by(person_id) %>%
  summarize(num_trajectories = n_distinct(trajectory_id))

View(sum)

sum_posortowana <- sum[order(-sum$num_trajectories), ]

# Wybieranie pierwszych 30 osób z najwyższą liczbą tras
top_30 <- sum_posortowana[1:30, ]

#### 3.1. Total routes per person ####
barplot(top_30$num_trajectories, 
        names.arg = top_30$person_id,
        xlab = "Person ID",
        ylab = "Number of Trajectories",
        main = "Top 30 Persons by Number of Trajectories",
        col = "Grey",
        las = 2,
        cex.names = 0.7,
        ylim = c(0, max(top_30$num_trajectories) * 1.1),
        horiz = FALSE,
        xlim = c(0.5, 30.5))

# adding the labels
text(x = 1:2, y = top_30$num_trajectories[1:2], labels = top_30$num_trajectories[1:2], pos = 3, offset = 0.5)

#### Database for the basket analysis - V2 - without the person no. 153 ####
v2 = subset(v1, person_id != 153)

# Binding the V2 database and unique_types databes by "type"
v2_merged <- merge(v2, unique_types, by.x = "types", by.y = "type", all.x = TRUE)

# Cleaning and transforming the dabase
v2_comb <- v2_merged 
v2_comb$Location <- paste("[", v2_comb$Location, "]", sep = "")
v2_comb$Location <- paste(v2_comb$name, v2_comb$Location, sep = " ")

# Removing: "" 
v2_comb$Location = gsub("'", "", v2_comb$Location, fixed = TRUE)

# Creating the baskets bases
# The base for the basket with names
v2_names = v2_comb %>%
  select(., trajectory_id, Location)

# Removing: "" 
v2_names$Location = gsub("'", "", v2_comb$Location, fixed = TRUE)

View(v2_names)

# The base for the basket with types 
v2_types = v2_comb %>%
  select(., trajectory_id, types)
View(v2_types)

# The base for the basket with codes
v2_codes = v2_comb  %>%
  select(., trajectory_id, Location)
View(v2_codes)

# The base for the basket with categories 
v2_category = v2_comb %>%
  select(., trajectory_id, category)
View(v2_category)

#### 3.2. - 3.4. Analysis
#### The basket setting ####
# the baskets with types
write_csv(v2_types, "v2_types.csv")
tr_types <-read.transactions("v2_types.csv", format="single", cols=c("trajectory_id","types"), header=TRUE, sep=",")
View(tr_types)

# the baskets with codes
write_csv(v2_codes, "v2_codes.csv")
tr_codes <-read.transactions("v2_codes.csv", format="single", cols=c("trajectory_id","Location"), header=TRUE, sep=",")

# the baskets with names
write_csv(v2_names, "v2_names.csv")
tr_names <-read.transactions("v2_names.csv", format="single", cols=c("trajectory_id","Location"), header=TRUE, sep=",", encoding = "UTF-8")

# the baskets with categories
write_csv(v2_category, "v2_category.csv")
tr_category <-read.transactions("v2_category.csv", format="single", cols=c("trajectory_id","category"), header=TRUE, sep=",")

View(v2_comb)

#### Frequencies plots ####

# Types
itemFrequencyPlot(tr_types, topN = 20, type = 'relative', main = 'Type Frequency Plot', ylim = c(0, 0.3))
type_freq <- itemFrequency(tr_types, type = 'relative')
type_freq <- sort(type_freq, decreasing = TRUE)
type_freq = data.frame(type_freq)
class(type_freq)
View(type_freq)
head(type_freq, 20)

type_freq$freq <- as.numeric(as.character(type_freq$freq))

type_freq <- type_freq[order(-type_freq$freq), ]

# Names
itemFrequencyPlot(tr_names, topN = 20, type = 'relative', main = 'Name Frequency Plot', ylim = c(0, 0.2))
name_freq <- itemFrequency(tr_names, type = 'relative')
name_freq <- sort(name_freq, decreasing = TRUE)
name_freq = data.frame(name_freq)
class(name_freq)
head(name_freq, 20)

# Categories
itemFrequencyPlot(tr_category, topN = 20, type = 'relative', main = 'Name Frequency Plot', ylim = c(0, 0.2))
cat_freq <- itemFrequency(tr_category, type = 'relative')
cat_freq <- sort(cat_freq, decreasing = TRUE)
cat_freq = data.frame(cat_freq)
class(cat_freq)
head(cat_freq, 20)


#### Summaries for baskets ####
# SUMMARY NAMES
print(summary(tr_names))

# SUMMARY TYPES
print(summary(tr_types))

# SUMMARY CATEGORIES
print(summary(tr_category))

# SUMMARY CODES
print(summary(tr_codes))


#### Calculating the Jaccard index & dendrograms plotting ####
# DLA ITEMÓW - types
trans.sel<-tr_types[,itemFrequency(tr_types)>0.01] # selected transations
d.jac.i<-dissimilarity(trans.sel, which="items") # Jaccard as default
round(d.jac.i,2) 
plot(hclust(d.jac.i, method="ward.D2"), main="Dendrogram for items")

# DLA ITEMÓW - names
trans.sel<-tr_names[,itemFrequency(tr_names)>0.02] # selected transations
d.jac.i<-dissimilarity(trans.sel, which="items") # Jaccard as default
round(d.jac.i,2) 
plot(hclust(d.jac.i, method="ward.D2"), main="Dendrogram for items")

# DLA ITEMÓW - category
trans.sel<-tr_category[,itemFrequency(tr_category)>0.005] # selected transations
d.jac.i<-dissimilarity(trans.sel, which="items") # Jaccard as default
round(d.jac.i,2) 
plot(hclust(d.jac.i, method="ward.D2"), main="Dendrogram for categories")


#### Association rules & rules cleaning ####
# RULES TYPE - CLEAN ##############
rules.types<-apriori(tr_types, parameter=list(supp=0.0065, conf=0.5))
rules.types.clean<-rules.types[!is.redundant(rules.types)] # elimination of redundant rules
rules.types.clean<-rules.types.clean[is.significant(rules.types.clean, tr_types)] # no insignificant rules
rules.types.clean<-rules.types.clean[is.maximal(rules.types.clean)] # only maximal rules

inspectDT(rules.types.clean) # wyświetlenie reguł
rules.types %>% plot(interactive=TRUE)# scatter

rules.types.clean.supp <-sort(rules.types.clean, by="support", decreasing=TRUE)
View(inspect(head(rules.types.clean.supp, 10)))
rules.types.clean.supp

ruleExplorer(rules.types.clean)


# RULES NAME - CLEAN ##############
rules.names<-apriori(tr_names, parameter=list(supp=0.005, conf=0.7))
rules.names.clean<-rules.names[!is.redundant(rules.names)] # elimination of redundant rules
rules.names.clean<-rules.names.clean[is.significant(rules.names.clean, tr_names)] # no insignificant rules
rules.names.clean<-rules.names.clean[is.maximal(rules.names.clean)] # only maximal rules
inspectDT(rules.names.clean) # wyświetlenie reguł
rules.types %>% plot(interactive=TRUE)# scatter
ruleExplorer(rules.names.clean)
rules.by.supp<-sort(rules.names.clean, by="support", decreasing=TRUE)
inspect(head(rules.by.supp,10)) # wyświetlenie reguł
write(rules.names.clean, file = "3.3.RulesNames22.csv", sep = ",", fileEncoding = "UTF-8")
# RULES CATEGORIES - CLEAN ##############
rules.category<-apriori(tr_category, parameter=list(supp=0.01, conf=0.7))
rules.category.clean<-rules.category[!is.redundant(rules.category)] # elimination of redundant rules
rules.category.clean<-rules.category.clean[is.significant(rules.category.clean, tr_category)] # no insignificant rules
rules.category.clean<-rules.category.clean[is.maximal(rules.category.clean)] # only maximal rules
inspectDT(rules.category.clean) # wyświetlenie reguł
ruleExplorer(rules.category.clean)


rules.category %>% plot(interactive=TRUE)# scatter
plot(rules.category, method="grouped")
plot(rules.category, method="graph", control=list(type="items")) 
rules.category %>% plot(interactive=TRUE)



# 3.5. TYPE: CAFE RULES - CLEAN  ####
rules.types.cafe<-apriori(data=tr_types, parameter=list(supp=0.001, conf=0.7), 
                          appearance=list(default="lhs", rhs="cafe"), control=list(verbose=F))
rules.types.cafe.clean<-rules.types.cafe[!is.redundant(rules.types.cafe)] # elimination of redundant rules
rules.types.cafe.clean<-rules.types.cafe.clean[is.significant(rules.types.cafe.clean, tr_types)] # no insignificant rules
rules.types.cafe.clean<-rules.types.cafe.clean[is.maximal(rules.types.cafe.clean)] # only maximal rules
write(rules.types.cafe.clean, file = "3.4.CafeRules.csv", sep = ",", fileEncoding = "UTF-8")
inspectDT(rules.types.cafe.clean)
inspect(rules.types.cafe.clean)

#### 3.6. Cafe Range Analysis ####
cafe_check =  v2_comb %>%
  filter(types == "cafe")

View(cafe_check)
v2_comb
v2_merged
unique_cafes <- cafe_check %>%
  group_by(trajectory_id) %>%
  distinct(Location) %>%
  ungroup() %>%
  count(Location)
unique_cafes

unique_cafes$NewColumn <- sub(".*\\[(.*?)\\].*", "\\1",unique_cafes$Location)

unique_cafes <- unique_cafes %>% rename(name = Location)
unique_cafes <- unique_cafes %>% rename(Location = NewColumn)
unique_cafes <- unique_cafes %>% mutate(Location = as.integer(Location))
unique_cafes = left_join(unique_cafes, pekin_filtered[, c("Location", "lat", "lng")], by = "Location")
#
unique_cafes <- cafe_check %>%
  group_by(trajectory_id) %>%
  distinct(name, Location) %>%
  ungroup() %>%
  count(Location)
View(unique_cafes)

#
write.csv(unique_cafes, "kawiarnie2.csv")

# Filtering for transactions with "cafe"
cafe_transactions <- v2_merged$trajectory_id[v2_merged$types == "cafe"]


filtered_cafe <- v2_merged[v2_merged$trajectory_id %in% cafe_transactions, ]

View(filtered_cafe)

# Adding the lat & lng
filtered_cafe <- left_join(filtered_cafe, pekin_filtered[, c("Location", "lat", "lng")], by = "Location")

# Results
View(filtered_cafe)

# ROASTERS
roasters_tours = filtered_cafe$trajectory_id[filtered_cafe$Location == "412"]
roasters <- filtered_cafe[filtered_cafe$trajectory_id %in% roasters_tours, ]
View(roasters)
roasters_dist <- roasters %>%
  distinct(lat, lng, .keep_all = TRUE)
View(roasters_dist)

write.csv(roasters_dist, "roasters_dist.csv", fileEncoding = "UTF-8")
View(visits_merged_filtered)

# Map - ROASTERS 39.94848 116.4135
ggmap(MyMap6) + 
  geom_point(data=roasters_dist, aes(x=lng, y=lat), alpha=0.8, color="grey2", size=1.5) +
  geom_point(data=roasters_dist, aes(x=116.4135, y=39.94848), alpha=0.8, color="red", size=5)


# ARABICA CAFE
arabica_tours = filtered_cafe$trajectory_id[filtered_cafe$Location == "365"]
arabica <- filtered_cafe[filtered_cafe$trajectory_id %in% arabica_tours, ]
View(arabica)
arabica_dist <- arabica %>%
  distinct(lat, lng, .keep_all = TRUE)
View(arabica_dist)

# Map - ARABICA
ggmap(MyMap6) + 
  geom_point(data=arabica_dist, aes(x=lng, y=lat), alpha=0.8, color="grey2", size=1.5) +
  geom_point(data=arabica_dist, aes(x=116.4547, y=39.93734), alpha=0.8, color="red", size=5)

# Map - ARABICA vs ROASTERS 
ggmap(MyMap4) + 
  geom_point(data=arabica_dist, aes(x=lng, y=lat), alpha=0.6, color="gold2", size=3) +
  geom_point(data=roasters_dist, aes(x=lng, y=lat), alpha=0.6, color="dodgerblue3", size=3) +
  geom_point(data=arabica_dist, aes(x=116.4547, y=39.93734), alpha=1, color="red3", size=5) +
  geom_point(data=roasters_dist, aes(x=116.4135, y=39.94848), alpha=1, color="blue4", size=5)

#### The Bridge Cafe 420
bridge_tours = filtered_cafe$trajectory_id[filtered_cafe$Location == "420"]
bridge <- filtered_cafe[filtered_cafe$trajectory_id %in% bridge_tours, ]
View(bridge)
bridge_dist <- bridge %>%
  distinct(lat, lng, .keep_all = TRUE)
View(bridge_dist)

# Map - Bridge
ggmap(MyMap2) + 
  geom_point(data=bridge_dist, aes(x=lng, y=lat), alpha=0.5, color="darkblue", size=1.3) +
  geom_point(data=bridge_dist, aes(x=116.3369, y=39.99207), alpha=0.1, color="red", size=3)

#### The Never Bar Cafe 434
never_tours = filtered_cafe$trajectory_id[filtered_cafe$Location == "434"]
never <- filtered_cafe[filtered_cafe$trajectory_id %in% never_tours, ]
View(never)
never_dist <- never %>%
  distinct(lat, lng, .keep_all = TRUE)
View(never_dist)

# Map - Never
ggmap(MyMap2) + 
  geom_point(data=never_dist, aes(x=lng, y=lat), alpha=0.5, color="darkblue", size=1.3) +
  geom_point(data=never_dist, aes(x=116.3863, y=39.94034), alpha=0.1, color="red", size=3)

#### Bridge vs Never
ggmap(MyMap3) + 
  geom_point(data=bridge_dist, aes(x=lng, y=lat), alpha=0.2, color="green1", size=20) +
  geom_point(data=bridge_dist, aes(x=116.3369, y=39.99207), alpha=0.9, color="greenyellow", size=5) +
  geom_point(data=never_dist, aes(x=lng, y=lat), alpha=0.2, color="red4", size=20) +
  geom_point(data=never_dist, aes(x=116.3863, y=39.94034), alpha=0.9, color="red1", size=5)


### Distance calculations from ROASTERS
# ROASTERS
roasters_tours = filtered_cafe$trajectory_id[filtered_cafe$Location == "412"]
roasters <- filtered_cafe[filtered_cafe$trajectory_id %in% roasters_tours, ]
View(roasters)
roasters_dist <- roasters %>%
  distinct(lat, lng, .keep_all = TRUE)
View(roasters_dist)

roast_odl <- roasters_dist

# Setting the reference point [dla ID = 412]
ref_lat <- 39.94848
ref_lng <- 116.4135

#Distance calculation for every point 
roast_odl$distance_meters <- distGeo(cbind(roast_odl$lng, roast_odl$lat), c(ref_lng, ref_lat))

# Printing the results
View(roast_odl)



### Distance calculations from ARABICA CAFE
arabica_odl <- arabica_dist

# Wybrane współrzędne punktu referencyjnego
ref_lat <- 39.93734
ref_lng <- 116.4547

#Distance calculation for every point 
arabica_odl$distance_meters <- distGeo(cbind(arabica_odl$lng, arabica_odl$lat), c(ref_lng, ref_lat))

# Printing the results
View(arabica_odl)

###Distance calculations from NEVER CAFE 
# x=116.3863, y=39.94034

never_odl <- never_dist

# Wybrane współrzędne punktu referencyjnego
ref_lat <- 39.94034
ref_lng <- 116.3863

#Distance calculation for every point 
never_odl$distance_meters <- distGeo(cbind(never_odl$lng, never_odl$lat), c(ref_lng, ref_lat))

# Printing the results
View(never_odl)

###Distance calculations from BRIDGE CAFE 
# x=116.3369, y=39.99207
bridge_odl <- bridge_dist

# Wybrane współrzędne punktu referencyjnego
ref_lat <- 39.99207
ref_lng <- 116.3369

#Distance calculation for every point 
bridge_odl$distance_meters <- distGeo(cbind(bridge_odl$lng, bridge_odl$lat), c(ref_lng, ref_lat))

# Printing the results
View(bridge_odl)


### Calculating every distance
### ACE Cafe [358]
ace_tours <- filtered_cafe$trajectory_id[filtered_cafe$Location == "358"]
ace <- filtered_cafe[filtered_cafe$trajectory_id %in% ace_tours, ]
#View(ace)
ace_dist <- ace %>%
  distinct(lat, lng, .keep_all = TRUE)
#View(ace_dist)

ace_odl <- ace_dist

# Setting the reference point [dla ID = 358]
ref_lat <- 39.9839
ref_lng <- 116.4973

#Distance calculation for every point 
ace_odl$distance_meters <- distGeo(cbind(ace_odl$lng, ace_odl$lat), c(ref_lng, ref_lat))

# Printing the results
#View(ace_odl)

### ARABICA COFFEE [365]
arabica_tours <- filtered_cafe$trajectory_id[filtered_cafe$Location == "365"]
arabica <- filtered_cafe[filtered_cafe$trajectory_id %in% arabica_tours, ]
#View(arabica)
arabica_dist <- arabica %>%
  distinct(lat, lng, .keep_all = TRUE)
#View(arabica_dist)

arabica_odl <- arabica_dist

# Setting the reference point [dla ID = 365]
ref_lat <- 39.93734
ref_lng <- 116.4547

#Distance calculation for every point 
arabica_odl$distance_meters <- distGeo(cbind(arabica_odl$lng, arabica_odl$lat), c(ref_lng, ref_lat))

# Printing the results
#View(arabica_odl)

### Blenz Coffee [402]
blenz_tours <- filtered_cafe$trajectory_id[filtered_cafe$Location == "402"]
blenz <- filtered_cafe[filtered_cafe$trajectory_id %in% blenz_tours, ]
#View(blenz)
blenz_dist <- blenz %>%
  distinct(lat, lng, .keep_all = TRUE)
#View(blenz_dist)

blenz_odl <- blenz_dist

# Setting the reference point [dla ID = 402]
ref_lat <- 40.08003
ref_lng <- 116.5933

#Distance calculation for every point 
blenz_odl$distance_meters <- distGeo(cbind(blenz_odl$lng, blenz_odl$lat), c(ref_lng, ref_lat))

# Printing the results
#View(blenz_odl)


### Cafe Duet [395]
cafe_duet_tours <- filtered_cafe$trajectory_id[filtered_cafe$Location == "395"]
cafe_duet <- filtered_cafe[filtered_cafe$trajectory_id %in% cafe_duet_tours, ]
##View(cafe_duet)
cafe_duet_dist <- cafe_duet %>%
  distinct(lat, lng, .keep_all = TRUE)
##View(cafe_duet_dist)

cafe_duet_odl <- cafe_duet_dist

# Setting the reference point [dla ID = 395]
ref_lat <- 39.93878
ref_lng <- 116.4396

#Distance calculation for every point 
cafe_duet_odl$distance_meters <- distGeo(cbind(cafe_duet_odl$lng, cafe_duet_odl$lat), c(ref_lng, ref_lat))

# Printing the results
##View(cafe_duet_odl)

### Cafe Groove [387]
cafe_groove_tours <- filtered_cafe$trajectory_id[filtered_cafe$Location == "387"]
cafe_groove <- filtered_cafe[filtered_cafe$trajectory_id %in% cafe_groove_tours, ]
##View(cafe_groove)
cafe_groove_dist <- cafe_groove %>%
  distinct(lat, lng, .keep_all = TRUE)
##View(cafe_groove_dist)

cafe_groove_odl <- cafe_groove_dist

# Setting the reference point [dla ID = 387]
ref_lat <- 39.93535
ref_lng <- 116.4443

#Distance calculation for every point 
cafe_groove_odl$distance_meters <- distGeo(cbind(cafe_groove_odl$lng, cafe_groove_odl$lat), c(ref_lng, ref_lat))

# Printing the results
#View(cafe_groove_odl)

### Cafe Groove [422]
cafe_groove_2_tours <- filtered_cafe$trajectory_id[filtered_cafe$Location == "422"]
cafe_groove_2 <- filtered_cafe[filtered_cafe$trajectory_id %in% cafe_groove_2_tours, ]
#View(cafe_groove_2)
cafe_groove_2_dist <- cafe_groove_2 %>%
  distinct(lat, lng, .keep_all = TRUE)
#View(cafe_groove_2_dist)

cafe_groove_2_odl <- cafe_groove_2_dist

# Setting the reference point [dla ID = 422]
ref_lat <- 39.93535
ref_lng <- 116.4443

#Distance calculation for every point 
cafe_groove_2_odl$distance_meters <- distGeo(cbind(cafe_groove_2_odl$lng, cafe_groove_2_odl$lat), c(ref_lng, ref_lat))

# Printing the results
#View(cafe_groove_2_odl)

### Cafe Marco [397]
cafe_marco_tours <- filtered_cafe$trajectory_id[filtered_cafe$Location == "397"]
cafe_marco <- filtered_cafe[filtered_cafe$trajectory_id %in% cafe_marco_tours, ]
#View(cafe_marco)
cafe_marco_dist <- cafe_marco %>%
  distinct(lat, lng, .keep_all = TRUE)
#View(cafe_marco_dist)

cafe_marco_odl <- cafe_marco_dist

# Setting the reference point [dla ID = 397]
ref_lat <- 40.00473
ref_lng <- 116.4084

#Distance calculation for every point 
cafe_marco_odl$distance_meters <- distGeo(cbind(cafe_marco_odl$lng, cafe_marco_odl$lat), c(ref_lng, ref_lat))

# Printing the results
#View(cafe_marco_odl)

### Helens Cafe [392]
helens_cafe_tours <- filtered_cafe$trajectory_id[filtered_cafe$Location == "392"]
helens_cafe <- filtered_cafe[filtered_cafe$trajectory_id %in% helens_cafe_tours, ]
#View(helens_cafe)
helens_cafe_dist <- helens_cafe %>%
  distinct(lat, lng, .keep_all = TRUE)
#View(helens_cafe_dist)

helens_cafe_odl <- helens_cafe_dist

# Setting the reference point [dla ID = 392]
ref_lat <- 39.98902
ref_lng <- 116.3373

#Distance calculation for every point 
helens_cafe_odl$distance_meters <- distGeo(cbind(helens_cafe_odl$lng, helens_cafe_odl$lat), c(ref_lng, ref_lat))

# Printing the results
#View(helens_cafe_odl)

### Klub Cafe [374]
klub_cafe_tours <- filtered_cafe$trajectory_id[filtered_cafe$Location == "374"]
klub_cafe <- filtered_cafe[filtered_cafe$trajectory_id %in% klub_cafe_tours, ]
#View(klub_cafe)
klub_cafe_dist <- klub_cafe %>%
  distinct(lat, lng, .keep_all = TRUE)
#View(klub_cafe_dist)

klub_cafe_odl <- klub_cafe_dist

# Setting the reference point [dla ID = 374]
ref_lat <- 39.88001
ref_lng <- 116.4605

#Distance calculation for every point 
klub_cafe_odl$distance_meters <- distGeo(cbind(klub_cafe_odl$lng, klub_cafe_odl$lat), c(ref_lng, ref_lat))

# Printing the results
#View(klub_cafe_odl)

### Never Bar Cafe [434]
never_bar_cafe_tours <- filtered_cafe$trajectory_id[filtered_cafe$Location == "434"]
never_bar_cafe <- filtered_cafe[filtered_cafe$trajectory_id %in% never_bar_cafe_tours, ]
#View(never_bar_cafe)
never_bar_cafe_dist <- never_bar_cafe %>%
  distinct(lat, lng, .keep_all = TRUE)
#View(never_bar_cafe_dist)

never_bar_cafe_odl <- never_bar_cafe_dist

# Setting the reference point [dla ID = 434]
ref_lat <- 39.94034
ref_lng <- 116.3863

#Distance calculation for every point 
never_bar_cafe_odl$distance_meters <- distGeo(cbind(never_bar_cafe_odl$lng, never_bar_cafe_odl$lat), c(ref_lng, ref_lat))

# Printing the results
#View(never_bar_cafe_odl)

### The Bridge Cafe [420]
the_bridge_cafe_tours <- filtered_cafe$trajectory_id[filtered_cafe$Location == "420"]
the_bridge_cafe <- filtered_cafe[filtered_cafe$trajectory_id %in% the_bridge_cafe_tours, ]
#View(the_bridge_cafe)
the_bridge_cafe_dist <- the_bridge_cafe %>%
  distinct(lat, lng, .keep_all = TRUE)
#View(the_bridge_cafe_dist)

the_bridge_cafe_odl <- the_bridge_cafe_dist

# Setting the reference point [dla ID = 420]
ref_lat <- 39.99207
ref_lng <- 116.3369

#Distance calculation for every point 
the_bridge_cafe_odl$distance_meters <- distGeo(cbind(the_bridge_cafe_odl$lng, the_bridge_cafe_odl$lat), c(ref_lng, ref_lat))

# Printing the results
#View(the_bridge_cafe_odl)

### Xunchang Xiangmo [366]
xunchang_xiangmo_tours <- filtered_cafe$trajectory_id[filtered_cafe$Location == "366"]
xunchang_xiangmo <- filtered_cafe[filtered_cafe$trajectory_id %in% xunchang_xiangmo_tours, ]
#View(xunchang_xiangmo)
xunchang_xiangmo_dist <- xunchang_xiangmo %>%
  distinct(lat, lng, .keep_all = TRUE)
#View(xunchang_xiangmo_dist)

xunchang_xiangmo_odl <- xunchang_xiangmo_dist

# Setting the reference point [dla ID = 366]
ref_lat <- 39.92545
ref_lng <- 116.3999

#Distance calculation for every point 
xunchang_xiangmo_odl$distance_meters <- distGeo(cbind(xunchang_xiangmo_odl$lng, xunchang_xiangmo_odl$lat), c(ref_lng, ref_lat))

# Printing the results
#View(xunchang_xiangmo_odl)

# Creating the table
median_distances <- data.frame(
  Cafe = c("Barista Coffee Roasters", "Blenz Coffee", "Cafe Duet", "Cafe Groove", "Cafe Groove", "Cafe Marco",
           "Helens Cafe", "Klub Cafe", "Never Bar Cafe", "The Bridge Cafe", "Xunchang Xiangmo", "ARABICA COFFEE", "ACE Cafe"),
  Median_Distance = NA
)

# Calculating the median distance for Barista Coffee Roasters [412]
median_distances$Median_Distance[median_distances$Cafe == "Barista Coffee Roasters"] <- median(roast_odl$distance_meters)

# Calculating the median distance for Blenz Coffee [402]
median_distances$Median_Distance[median_distances$Cafe == "Blenz Coffee"] <- median(blenz_odl$distance_meters)

# Calculating the median distance for Cafe Duet [395]
median_distances$Median_Distance[median_distances$Cafe == "Cafe Duet"] <- median(cafe_duet_odl$distance_meters)

# Calculating the median distance for Cafe Groove [387]
median_distances$Median_Distance[median_distances$Cafe == "Cafe Groove"] <- median(cafe_groove_odl$distance_meters)

# Calculating the median distance for Cafe Groove [422]
median_distances$Median_Distance[median_distances$Cafe == "Cafe Groove"] <- median(cafe_groove_2_odl$distance_meters)

# Calculating the median distance for Cafe Marco [397]
median_distances$Median_Distance[median_distances$Cafe == "Cafe Marco"] <- median(cafe_marco_odl$distance_meters)

# Calculating the median distance for Helens Cafe [392]
median_distances$Median_Distance[median_distances$Cafe == "Helens Cafe"] <- median(helens_cafe_odl$distance_meters)

# Calculating the median distance for Klub Cafe [374]
median_distances$Median_Distance[median_distances$Cafe == "Klub Cafe"] <- median(klub_cafe_odl$distance_meters)

# Calculating the median distance for Never Bar Cafe [434]
median_distances$Median_Distance[median_distances$Cafe == "Never Bar Cafe"] <- median(never_bar_cafe_odl$distance_meters)

# Calculating the median distance for The Bridge Cafe [420]
median_distances$Median_Distance[median_distances$Cafe == "The Bridge Cafe"] <- median(the_bridge_cafe_odl$distance_meters)

# Calculating the median distance for Xunchang Xiangmo [366]
median_distances$Median_Distance[median_distances$Cafe == "Xunchang Xiangmo"] <- median(xunchang_xiangmo_odl$distance_meters)

# Calculating the median distance for ARABICA COFFEE [365]
median_distances$Median_Distance[median_distances$Cafe == "ARABICA COFFEE"] <- median(arabica_odl$distance_meters)

# Calculating the median distance for ACE_Cafe [365]
median_distances$Median_Distance[median_distances$Cafe == "ACE Cafe"] <- median(ace_odl$distance_meters)

# Arabica
# Ace'

# Median comparison #
never_median = median(never_odl$distance_meters)
roasters_median = median(roast_odl$distance_meters)
arabica_median = median(arabica_odl$distance_meters)
bridge_median = median(bridge_odl$distance_meters)

medians <- data.frame(Database = c("	ARABICA COFFEE", "	Barista Coffee Roasters", "Never Bar Cafe", "The Bridge Cafe"),
                      Median_Distance_Meters = c(arabica_median, roasters_median, never_median, bridge_median))


View(medians)

# Table with the median distances
View(median_distances)

write.csv(median_distances, "3.5.median distances.csv")



#### 3.7. THE BICYCLE STORE ANALYSIS ####

bicycle_transactions <- v2_merged$trajectory_id[v2_merged$types == "bicycle_store"]
filtered_bicycle <- v2_merged[v2_merged$trajectory_id %in% bicycle_transactions, ]
filtered_bicycle <- left_join(filtered_bicycle, pekin_filtered[, c("Location", "lat", "lng")], by = "Location")
View(filtered_bicycle)
View(visits_merged_filtered)


df_sorted <- visits_merged_filtered %>%
  arrange(trajectory_id, V6, V7)
View(df_sorted)

df_sorted2 <- df_sorted %>%
  group_by(trajectory_id, Location) %>%
  mutate(Location_Order = dense_rank(Location, ties.method = "first"))
View(df_sorted2)

# Sortowanie bazą danych
visits_merged_filtered_sorted <- visits_merged_filtered[order(visits_merged_filtered$trajectory_id, visits_merged_filtered$V6, visits_merged_filtered$V7), ]

# Ponumerowanie unikalnych "Location" dla danego "trajectory_ID"
visits_merged_filtered_sorted$Location_Number <- ave(visits_merged_filtered_sorted$Location, visits_merged_filtered_sorted$trajectory_id, FUN = function(x) match(x, unique(x)))

# Wyświetlenie wyniku
View(visits_merged_filtered_sorted)

# Sortowanie bazy danych
visits_merged_filtered_sorted <- visits_merged_filtered[order(visits_merged_filtered$trajectory_id, visits_merged_filtered$V6, visits_merged_filtered$V7), ]

# Inicjalizacja wektora dla wyników odległości
distances <- numeric(nrow(visits_merged_filtered_sorted))
distances[1] <- 0  # Odległość dla pierwszego punktu w trajektorii

# Obliczanie odległości dla kolejnych punktów w tej samej trajektorii
for (i in 2:nrow(visits_merged_filtered_sorted)) {
  if (visits_merged_filtered_sorted$trajectory_id[i] == visits_merged_filtered_sorted$trajectory_id[i-1]) {
    lat1 <- visits_merged_filtered_sorted$lat[i-1]
    lon1 <- visits_merged_filtered_sorted$lng[i-1]
    lat2 <- visits_merged_filtered_sorted$lat[i]
    lon2 <- visits_merged_filtered_sorted$lng[i]
    distances[i] <- distGeo(c(lon1, lat1), c(lon2, lat2))
  } else {
    distances[i] <- 0  # Resetuj odległość dla nowej trajektorii
  }
}

# Dodanie kolumny z odległościami do bazy danych
visits_merged_filtered_sorted$Distance <- distances

# Wyświetlenie wyniku
View(visits_merged_filtered_sorted)

# Sumowanie odległości dla każdego trajectory_id z zachowaniem kolumny "person_id"
sum_distances <- aggregate(Distance ~ trajectory_id + person_id, visits_merged_filtered_sorted, sum)

# Wyświetlenie wyniku
View(sum_distances)

avg_distances <- aggregate(Distance ~ person_id, sum_distances, mean)

# Wyświetlenie wyniku
View(avg_distances)

write_csv(avg_distances, "avg_distances.csv")

adbikes = v2_merged

adbikes <- left_join(adbikes, pekin_filtered[, c("Location", "lat", "lng")], by = "Location")
write.csv(adbikes, "adbikes.csv", fileEncoding = "UTF-8")
View(adbikes)

# Podmianka typu
bicycles <- adbikes %>%
  mutate(types = ifelse(grepl("Bicycle", name), "bicycle_store", types))

# bicycles = bicycles %>%
#  filter(types == "bicycle_store")

bikers <- bicycles %>%
  filter(types == "bicycle_store") %>%
  group_by(person_id) %>%
  summarise(count = n())

# Wyświetlanie wynikowej bazy danych
print(bikers)

write.csv(bikers, "3.6.bikers.csv")










