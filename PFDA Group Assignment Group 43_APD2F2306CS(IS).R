#Vinnie Teh, TP064168
#Edward Ding Hong Wai, TP065396
#Lai Tzi Syuen Suzanne, TP068173
#Tan Jia Ling, TP068255
#APD2F2306CS(IS)

#====LOAD LIBRARIES========
library(summarytools)
library(dplyr) #Data Manipulation
library(ggplot2) #Data Visualization
library(patchwork)
library(ggthemes)
library(corrplot)
library(ggridges)
library(ggrepel)
library(gridExtra)
library(cowplot)
library(ggforce)
library(ggmap)
library(magrittr)
library(party)
library(caTools)
library(datasets)
library(ggalt)

getwd();
setwd("C:/Users/vinni/OneDrive/Desktop/PFDA Group Assignment")

#======== DATA EXPLORATION =========
# Read the csv file
df = read.csv("House_Rent_Dataset.csv",header = TRUE)
#Read the CSV data in R
#View(df)
#get number of columns 
length(df) 
#get number of rows
nrow(df)
#Identify the data type
str(df)
#summary
summary(df)


#Check for any outlier (unusual value)
#df %>% group_by(BHK) %>%summarise(count=n()) %>%View() 
#df %>% group_by(Rent) %>%summarise(count=n()) %>%View() 
#df %>% group_by(Size) %>%summarise(count=n()) %>%View() 
#df %>% group_by(Floor) %>%summarise(count=n()) %>%View() 
#df %>% group_by(Area.Type) %>%summarise(count=n()) %>%View()  
#df %>% group_by(Area.Locality) %>%summarise(count=n()) %>%View()  
#df %>% group_by(City) %>%summarise(count=n()) %>%View()
#df %>% group_by(Furnishing.Status) %>%summarise(count=n()) %>%View()
#df %>% group_by(Tenant.Preferred) %>%summarise(count=n()) %>%View()
#df %>% group_by(Bathroom) %>%summarise(count=n()) %>%View() 
#df %>% group_by(Point.of.Contact) %>%summarise(count=n()) %>%View() 


#======== DATA MANIPULATION AND TRANSFORMATION =========
#Save original data of df before conducting data transformation
df_house_rent = df

#Data cleaning
#handle missing value (null value)
#This function checks whether the input value contains any missing or null values. 
#The length() function is applied to count the number of null values in values (df). 
CheckNUll=function(value){
  length(which(is.null(value)))
}

#The lapply() function applies the CheckNUll() function to each column of df
#return the number of null values in each column of df
lapply(df, CheckNUll)


#remove duplicated rows
df <- df[!duplicated(df), ]
nrow(df)

# remove the rows with outlier from df
df <- df[df$Area.Locality != 5000, ]
df <- df[df$Area.Locality != 700051, ]
df <- df[df$Area.Locality != "2 BHK", ]

# Modify the ambiguous value - made in assumption
df$Floor[df$Floor == "1"] = "1 out of 1"
df$Floor[df$Floor == "3"] = "3 out of 3"
df$Floor[df$Floor == "Ground"] = "Ground out of 1"
df$Size[df$Size <= 20] = mean(df$Size)

#remove leading space
df$Area.Locality <- trimws(df$Area.Locality)

#remove "in" at the beginning of the area locality 
df$Area.Locality <- gsub("^in ", "", df$Area.Locality)


#Show the column names
colnames(df)

#Rename the columns
colnames(df) = c("PostedOn","BHK","Rent","Size","Floor",
                 "AreaType","AreaLocality","City","FurnishingStatus","TenantPreferred","Bathroom","PointofContact")

#Extract month from DatePosted - *not sure (since it is year 2022 (April, May, June, July) data only)
df <- df %>%
  mutate(MonthPosted = as.Date(PostedOn, format = "%m/%d/%Y") %>% format(format = "%m"))
df$MonthPosted <- as.integer(df$MonthPosted)
is.numeric(df$MonthPosted)
head(df)

# Convert categorical variable into Factor for Level Indication
df$MonthPosted = factor(df$MonthPosted)
df$BHK = factor(df$BHK)
df$AreaType = factor(df$AreaType,levels = c("Super Area", "Carpet Area", "Built Area"))
df$City = factor(df$City)
df$FurnishingStatus = factor(df$FurnishingStatus,levels = c("Furnished", "Semi-Furnished", "Unfurnished"))
df$TenantPreferred = factor(df$TenantPreferred,levels = c("Bachelors", "Family", "Bachelors/Family"))
df$PointofContact = factor(df$PointofContact,,levels = c("Contact Owner", "Contact Agent", "Contact Builder"))

# Check the levels of a factor 
levels(df$MonthPosted)
levels(df$BHK)
levels(df$AreaType)
levels(df$City)
levels(df$FurnishingStatus)
levels(df$TenantPreferred)
levels(df$PointofContact)

#==========================================================================

#Data Exploration
#Data Exploration 1 - Rent Exploration
cat("Standard Deviation of Rent:", sd(df$Rent))

Rent_Q1 <- quantile(df$Rent, 0.25)
Rent_Q3 <- quantile(df$Rent, 0.75)
Rent_IQR <- Rent_Q3 - Rent_Q1
cat("Interquartile Range of Rent:", Rent_IQR)

df %>% 
  ggplot(aes(y = Rent)) +
  geom_boxplot(fill = "lightblue", color = "purple") +
  labs(x = "Rental Price", title = "Rental Price Distribution") +
  theme_minimal()

#==========================================================================

#Data Transformation
#Data transformation 1 -  Handle the rental price outlier.  
df$Rent[df$Rent == max(df$Rent)] = mean(df$Rent)
cat("Standard Deviation of Rent:", sd(df$Rent))
Rent_Q1 <- quantile(df$Rent, 0.25)
Rent_Q3 <- quantile(df$Rent, 0.75)
Rent_IQR <- Rent_Q3 - Rent_Q1
cat("Interquartile Range of Rent:", Rent_IQR)

#==========================================================================

#Data Manipulation 
#Data Manipulation 1– Convert floor description to numerical floor number
#ground floor = 0
#upper basement = -1
#lower basement = -2
df$Floor <- gsub("Ground out of (\\d+)", "0 out of \\1", df$Floor)
df$Floor <- gsub("Upper Basement out of (\\d+)", "-1 out of \\1", df$Floor)
df$Floor <- gsub("Lower Basement out of (\\d+)", "-2 out of \\1", df$Floor)
View(df)

SplitValues <- strsplit(as.character(df$Floor), " out of ")
FloorNumValues <- sapply(SplitValues, function(x) as.numeric(x[1]))
OutOfNumValues <- sapply(SplitValues, function(x) as.numeric(x[2]))

df <- df %>%
  mutate(PropertyFloorNum = FloorNumValues,
         TotalFloorNum = OutOfNumValues)
head(df)

str(df$PropertyFloorNum)
str(df$TotalFloorNum)

#Data Manipulation 2-  add CityPopulation in df
df <- df %>% 
  mutate(CityPopulation = case_when(
    City=="Kolkata" ~ 14974000,
    City == "Mumbai" ~ 24433357,
    City == "Bangalore" ~ 14254786,
    City == "Delhi" ~ 20591874,
    City == "Chennai" ~ 9722974,
    City == "Hyderabad" ~ 13871587,
    TRUE ~ NA_integer_
  ))
df$CityPopulation

#data transformation 3 - add CitySize column to df
#City size calculated in km square
df <- df %>% 
  mutate(CitySize = case_when(
    City=="Kolkata" ~ 206.08 ,
    City == "Mumbai" ~ 603.4 ,
    City == "Bangalore" ~ 709,
    City == "Delhi" ~ 1483,
    City == "Chennai" ~ 5904,
    City == "Hyderabad" ~ 625,
    TRUE ~ NA_integer_
  ))
df$CitySize

#Data Transformation 4-1: Mutate Distance from the capital Column 
#https://en.wikipedia.org/wiki/New_Delhi
#New Delhi is the main capital in India 
#Calculate driving distance (km)
#https://www.distancecalculator.net/country/india

df <- df %>% 
  mutate(CityDistanceFromCapital = case_when(
    City=="Kolkata" ~ 1471,
    City == "Mumbai" ~ 1433 ,
    City == "Bangalore" ~ 2148,
    City == "Delhi" ~ 4,
    City == "Chennai" ~ 2187,
    City == "Hyderabad" ~ 1560,
    TRUE ~ NA_integer_
  ))
df$CityDistanceFromCapital

#==========================================================================

#Question 1 - Does the spacial extent of a rental property affect its rental price?
#Exploration 1-1 – Summary statistics of area type, rent and size.
#descriptive statistics of Size and Rent
descr(df$Size)
descr(df$Rent)
#calculate frequency of Area Type
freq(df$AreaType)

#dispersion analysis - calculate standard deviation
SuperAreaSd <- df %>%
  filter(AreaType == "Super Area") %>%
  pull(Size) %>%
  sd()

CarpetAreaSd <- df %>%
  filter(AreaType == "Carpet Area") %>%
  pull(Size) %>%
  sd()

BuiltAreaSd <- df %>%
  filter(AreaType == "Built Area") %>%
  pull(Size) %>%
  sd()
  
cat("Standard Deviation of Size in Super Area:", SuperAreaSd)
cat("Standard Deviation of Size in Carpet Area:", CarpetAreaSd)
cat("Standard Deviation of Size in Built Area:", BuiltAreaSd)

#Data Exploration 1-2 – Distribution of size by area type

df %>% 
  ggplot(aes(Size)) +
  geom_histogram(fill = "lightblue", color = "purple") +
  labs(x = "Size", y = "Frequency", title = "Distribution of Rental Property Sizes",subtitle = "Wrapped by City") +
  facet_wrap(~AreaType)

#Data Exploration 1-3 – Distribution of size (with same area type) on rental price 
df %>%
  ggplot(aes(x = Size, y = Rent)) +
  geom_point(aes(color = Rent))+
  geom_smooth(method = 'loess')+
  labs(x = "Size", y = "Rental Price", title = "Size Distribution on Rental Price", subtitle = "Wrapped by Area Type") +
  facet_wrap(~ AreaType) +
  theme_minimal()

CorrelationSuperArea <- df %>%
  filter(AreaType == "Super Area") %>%
  summarise(correlation = cor(Size, Rent))

CorrelationCarpetArea <- df %>%
  filter(AreaType == "Carpet Area") %>%
  summarise(correlation = cor(Size, Rent))

CorrelationBuiltAarea <- df %>%
  filter(AreaType == "Built Area") %>%
  summarise(correlation = cor(Size, Rent))

# Combine the results into a data frame
CorrelationDf <- data.frame(
  AreaType = c("Super Area", "Carpet Area", "Built Area"),
  Correlation = c(CorrelationSuperArea$correlation,
                  CorrelationCarpetArea$correlation,
                  CorrelationBuiltAarea$correlation)
)

print(CorrelationDf)


#Data Exploration 1-4 - Rental price variation across size quartiles within each area type
# Calculate quartiles for property size within each area type

Quartiledf <- df %>%
  group_by(AreaType) %>%
  mutate(SizeQuartile = ntile(Size, 4)) %>%
  ungroup()

Quartiledf$SizeQuartile = factor(Quartiledf$SizeQuartile)

Quartiledf %>%
  ggplot(aes(x = SizeQuartile, y = Rent, fill = AreaType)) +
  geom_boxplot() +
  labs(x = "Size Quartile", y = "Rental Price", title = "Rental Price Variation by Property Size Quartiles",
       subtitle = "Within Each Area Type") +
  facet_wrap(~ AreaType) +
  theme_minimal()


#Data Exploration 1-5 – Distribution of size in different cities and area type on rental price
df %>%
  ggplot(aes(x = Size, y = Rent, color = City)) +
  geom_count()+
  labs(x = "Size", y = "Rent", title = "Size Distribution on Rental Price",
       subtitle = "Colored by City and Wrapped by AreaType") +
  facet_wrap(~ AreaType) +
  theme_minimal()


#Data Exploration 1-6 – Floor distribution 

# Data Exploration 1-6a - Property's Floor Number Distribution
df %>%
  ggplot(aes(x = PropertyFloorNum)) +
  geom_histogram(binwidth = 1, color = "steelblue", fill = "lightblue") +
  labs(title = "Distribution of Property's Floor Number",
       x = "Property's Floor Number",
       y = "Frequency")+
  scale_x_continuous(breaks = seq(0, max(df$PropertyFloorNum), by = 10))

# Data Exploration 1-6b - Distribution of Total Number of Floors in Properties 
df %>%
  ggplot(aes(x = TotalFloorNum)) +
  geom_histogram(binwidth = 1, color = "steelblue", fill = "lightblue") +
  labs(title = "Distribution of Total Number of Floors in Properties",
       x = "Total Number of Floors",
       y = "Frequency")+
  scale_x_continuous(breaks = seq(0, max(df$TotalFloorNum), by = 10))

# Data Exploration 1-6c - Distribution of Total Number of Floors by Property's Floor Number
cor(df$TotalFloorNum,df$PropertyFloorNum) #correlation
A = lm(df$PropertyFloorNum~df$TotalFloorNum) #get the equation lm(y,x)
print(A)
df %>% 
  ggplot(aes(x = TotalFloorNum, y = PropertyFloorNum))+
  geom_point(color = "blue")+
  geom_abline(intercept = coef(A)[1], slope = coef(A)[2], color = "red") +
  labs(title = "Distribution of Total Number of Floors by Property's Floor Number",
       x = "Total Number of Floors",
       y = "Property's Floor Number")


CorrelationMatrix <- df %>%
  filter(AreaType == "Super Area") %>%
  select(Size, Rent, PropertyFloorNum, TotalFloorNum) %>%
  cor()
CorrelationMatrix

#coef(A)[1] is y-intercept, coef(A)[2] is gradient

#Data Exploration 1-7 – Month posted distribution.
#get the frequency
table(df$MonthPosted)

df %>%
  ggplot(aes(x = "", fill = MonthPosted)) +
  geom_bar() +
  coord_polar(theta = "y") +
  labs(fill = "Month Posted", title = "Month Posted Distribution") +
  theme(legend.position = "right")

CorrelationBuiltAarea <- df %>%
  filter(AreaType == "Built Area") %>%
  summarise(correlation = cor(Size, Rent))

# Combine the results into a data frame
CorrelationDf <- data.frame(
  AreaType = c("Super Area", "Carpet Area", "Built Area"),
  Correlation = c(CorrelationSuperArea$correlation,
                  CorrelationCarpetArea$correlation,
                  CorrelationBuiltAarea$correlation)
)


#-----------------------------------------------------------------------------------
#Sub question 1 – Does the population of city and month posted affect the rental price on same area type and size? 
#Analysis 1-1 <- 
df %>% 
  filter(AreaType == "Super Area" | AreaType == "Carpet Area") %>% 
  ggplot(aes(x = CityPopulation, y = Size)) +
  geom_boxplot(aes(group = CityPopulation, fill = factor(City))) +
  labs(title = "Relationship between City Population and Property Size",
       x = "City Population",
       y = "Property Size") +
  facet_wrap(~ AreaType) +
  theme_minimal()+
  scale_fill_discrete(name = "City")


# Create a slope graph
MonthLabels <- c("April", "May", "June", "July")
Monthdf<- Quartiledf
Monthdf$MonthPosted <- factor(df$MonthPosted, levels = c(4, 5, 6, 7), labels = MonthLabels)
Monthdf %>% 
  filter(AreaType=="Super Area") %>% 
  ggplot(aes(x = MonthPosted, y = Rent, group = City )) +
  geom_point(aes(color = City), size = 3) +
  geom_line(aes(color = City), size = 1) +
  labs(title = "Rent Trends across Months for Different Cities in Super Area",
       x = "Month Posted", y = "Rent",
       subtitle = "Wrapped by Size Quartile") +
  theme_minimal() +
  geom_vline(xintercept=1, linetype="dashed", size=.1) + 
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  geom_vline(xintercept=3, linetype="dashed", size=.1) + 
  geom_vline(xintercept=4, linetype="dashed", size=.1) +
  facet_wrap(~SizeQuartile,labeller = as_labeller(~ paste0("Size Quartile: ", .x)))

Monthdf %>% 
  filter(AreaType=="Carpet Area") %>% 
  ggplot(aes(x = MonthPosted, y = Rent, group = City )) +
  geom_point(aes(color = City), size = 3) +
  geom_line(aes(color = City), size = 1) +
  labs(title = "Rent Trends across Months for Different Cities in Carpet Area",
       x = "Month Posted", y = "Rent",
       subtitle = "Wrapped by Size Quartile") +
  theme_minimal() +
  geom_vline(xintercept=1, linetype="dashed", size=.1) + 
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  geom_vline(xintercept=3, linetype="dashed", size=.1) + 
  geom_vline(xintercept=4, linetype="dashed", size=.1) +
  facet_wrap(~SizeQuartile,labeller = as_labeller(~ paste0("Size Quartile: ", .x)))

RDF <- df %>% 
  filter(AreaType != "Built Area") %>% 
  select(Rent, Size, City, CityPopulation, MonthPosted,AreaType)

Normalized <- function(x) 
  (x - min(x)) / (max(x) - min(x))
RDF <- RDF %>%
  mutate_at(vars(Rent, Size), Normalized)

RDF$CityPopulation = factor(RDF$CityPopulation)

RDF %>% 
  filter(AreaType=="Super Area") %>% 
  ggplot(aes(x = Size, y = Rent, color = CityPopulation)) +
  geom_polygon(aes(group = City), fill = NA, size = 1.5) +
  geom_path(aes(group = City), size = 1.5)+
  theme_minimal() +
  coord_polar() +
  labs(title = "Radar Chart - Relationship between Rent and Size in Super Area across City Population",
       subtitle = "Wrapped by Month Posted",
       x = "Property Size (Normalized)",
       y = "Rent (Normalized)") +
  facet_grid(~MonthPosted, 
             labeller = as_labeller(c("4" = "April", "5" = "May", "6" = "June", "7" = "July")))

RDF %>%
  filter(AreaType=="Carpet Area") %>% 
  ggplot(aes(x = Size, y = Rent, color = CityPopulation)) + 
  geom_line()+
  labs(title = "Relationship between Rent and Property Size in Carpet Area across City Population",
       subtitle = "Wrapped by Month Posted",
       x = "Property Size",
       y = "Rent") +
  facet_grid(~MonthPosted, 
             labeller = as_labeller(c("4" = "April", "5" = "May", "6" = "June", "7" = "July")))




#-----------------------------------------------------------------------------------
#Sub question 2 – Does the size of city and the number of floor affect the rental price on the same area type and size? 
#Analysis 1-2 <- 

Quartiledf %>% 
  filter(AreaType == "Super Area") %>% 
  ggplot(aes(x=CitySize, y=Rent)) +
  geom_segment( aes(x=CitySize, xend=CitySize, y=0, yend=Rent)) +
  geom_point( size=5, color="purple", fill=alpha("pink", 0.3), alpha=0.7, shape=21, stroke=2)+
  labs(title = "Relationship between City Size and Rent in Super Area",
       x = "City Size",
       y = "Rent",
       subtitle = "Faceted by Property Size") +
    facet_grid(~SizeQuartile,labeller = as_labeller(~ paste0("Size Quartile: ", .x)))


Quartiledf %>%
  filter(AreaType == "Carpet Area") %>%
  ggplot(aes(x = CitySize, y = Rent, color = City)) +
  geom_jitter() +
  labs(title = "Relationship between City Size and Rent in Carpet Area",
       x = "City Size",
       y = "Rent",
       subtitle = "Faceted by Property Size") +
  facet_grid(~SizeQuartile,labeller = as_labeller(~ paste0("Size Quartile: ", .x))) +  
  theme_minimal() +
  scale_color_discrete(name = "City") +
  theme(panel.border = element_rect(color = "black", fill = NA))


CorrelationMatrix <- cor(df[, c("PropertyFloorNum", "TotalFloorNum", "Rent", "Size","CitySize")])
print(CorrelationMatrix)

# Convert the correlation matrix to a data frame
CorDf <- as.data.frame(as.table(CorrelationMatrix))


# Create a heatmap
CorDf %>% 
  ggplot( aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "red") +
  geom_text(aes(label = round(Freq, 2)),
            color = "black",
            size = 3,
            vjust = 0.5) + 
  labs(title = "Correlation Matrix: 
       Relationship between City Size, Property Size, Rent, Total Floor Number of Rental Propertiess, 
       and Floor Number of Properties",
       x = "Variable",
       y = "Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Create a heatmap for a each city

CreateHeatmapForEachCity <- function(CityData) {
  CorrelationMatrix <- cor(CityData[, c("PropertyFloorNum", "TotalFloorNum", "Rent", "Size")])
  CorDf <- as.data.frame(as.table(CorrelationMatrix))
  ggplot(CorDf, aes(x = Var1, y = Var2, fill = Freq)) +
    geom_tile() +
    scale_fill_gradient(low = "blue", high = "red") +
    geom_text(aes(label = round(Freq, 2)),
              color = "black",
              size = 3,
              vjust = 0.5) + 
    labs(title = paste("Correlation Matrix for", unique(CityData$City)),
         x = "Variable",
         y = "Variable") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# List of unique city names
CityNames <- unique(df$City)

# Create a list of heatmaps, one for each city
HeatMaps <- lapply(CityNames, function(CityName) {
  CityData <- df %>% filter(City == CityName)
  CreateHeatmapForEachCity(CityData)
})

# Arrange all the heatmaps in a grid layout
HeatMapGrid <- wrap_plots(HeatMaps, ncol = 3)  
print(HeatMapGrid)


#Sub question 3 – Does the area type and size affect the furnishing status? 
#If so, does the furnishing status affect the rental price on same area type and size?  
#Analysis 1-3 <- 
# Group by AreaType, Size, and FurnishingStatus, and calculate mean Rent
furnishing_analysis <- df %>%
  group_by(AreaType, Size, FurnishingStatus) %>%
  summarize(mean_rent = mean(Rent))

Quartiledf %>%
  filter(AreaType != "Built Area") %>%
  group_by(SizeQuartile, FurnishingStatus, AreaType) %>%
  summarize(Count = n()) %>%
  ggplot(aes(x = SizeQuartile, y = Count, fill = FurnishingStatus)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Count of Furnishing Status by Size and Area Type",
    x = "Size",
    y = "Count",
    subtitle = "Faceted by Area Type"
  ) +
  facet_wrap(~ AreaType) +
  theme_minimal()

Quartiledf %>%
  filter(AreaType != "Built Area") %>%
  group_by(SizeQuartile, FurnishingStatus, AreaType) %>%
  summarize(Count = n()) %>%
  ggplot(aes(x = SizeQuartile, y = Count, fill = FurnishingStatus)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Count of Furnishing Status by Size Quartile and Area Type",
    x = "Size Quartile",
    y = "Count",
    fill = "Furnishing Status",
    subtitle = "Faceted by Area Type"
  ) +
  facet_wrap(~ AreaType) +
  theme_minimal() +
  theme(legend.position = "top")  # Place the legend at the top


Quartiledf <- df %>%
  mutate(RentQuartile = ntile(Rent, 4))

Quartiledf$RentQuartile = factor(Quartiledf$RentQuartile)

# Create the bubble chart

Quartiledff %>%
  filter(AreaType != "Built Area") %>%
  ggplot(aes(x = Size, y = Rent, color = FurnishingStatus)) +
  labs(subtitle = "Effect of Size on Rental Price Wrapped by Area Type, Size by Rent, Color by Furnishing Status",
       title = "Bubble Chart") +
  facet_grid(FurnishingStatus ~ AreaType) +
  geom_jitter(aes(size = Rent), alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_size_continuous(range = c(5, 20)) +
  scale_x_continuous(labels = scales::comma) +
  theme(legend.position = "top") +
  guides(size = guide_legend(title = "Rental Price"))



#Sub question 4 – Does the area type and size affect the number of bathrooms? 
#If so, does the number of bathrooms affect the rental price on same area type and size?
#Analysis 1-4 <- 
  
NegativeSuperArea <- df %>% 
  mutate(Size = ifelse(AreaType=="Super Area",Size*-1,Size))

NegativeSuperArea$Size
NegativeSuperArea$Bathroom = factor(NegativeSuperArea$Bathroom)
NegativeSuperArea %>% 
    filter(AreaType !="Built Area") %>% 
    ggplot(aes(x = Bathroom, y = Size, fill = AreaType))+
    geom_bar(stat = "identity",width = 0.6)+
    coord_flip()+
    labs(title = "Distribution of Bathroom on Size",
         subtitle = "Color by Area Type")+
  scale_y_continuous(labels = function(x) gsub("^-", "", x)) +  # Remove the negative sign
  theme_tufte() 


# Filter the data and select relevant columns
CorSuperData <- df %>%
  filter(AreaType == "Super Area") %>%
  select(Bathroom, Rent,Size)

# Calculate the correlation matrix
CorSuperDataMatrix <- cor(CorSuperData)
CorSuperDataMatrix

# Create the correlation plot
corrplot(CorSuperDataMatrix,
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.srt = 45)


#https://www.dairydatascience.com/posts/2022-08-11-overlaid-plots-in-ggplot2/

temp_df <- Quartiledf %>%  filter(AreaType !="Built Area")
temp_df$Bathroom = factor(temp_df$Bathroom)
temp_df %>%
  filter(AreaType == "Super Area") %>% 
  ggplot(aes(x=Rent, fill=Bathroom, group=Bathroom)) +
  geom_density(alpha=0.6) +
  labs(title="Density of Bathroom and Rent in Super Area", 
       subtitle = "Wrapped by Size Quartile",
       x="Rent", y="Density") +
  facet_grid(~SizeQuartile, labeller = labeller(SizeQuartile = function(label) {
    paste("Size Quartile:", label)
  })) 

temp_df %>%
  filter(AreaType == "Carpet Area") %>% 
  ggplot(aes(x=Rent, fill=Bathroom, group=Bathroom)) +
  geom_density(alpha=0.6) +
  labs(title="Density of Bathroom and Rent in Carpet Area", 
       subtitle = "Wrapped by Size Quartile",
       x="Rent", y="Density") +
  facet_grid(~SizeQuartile, labeller = labeller(SizeQuartile = function(label) {
    paste("Size Quartile:", label)
  })) 


#================================================================================

#Question 2 - How do the types of furnishing status (fully furnished, semi-furnished, unfurnished)
#and point of contact (Owner, Agent) affect the rental price?

# Exploration 2-1 - Summary of furnishing status and point of contact 
summary(df$FurnishingStatus)
summary(df$PointofContact) 


df %>%
  ggplot(aes(x = "", fill = FurnishingStatus)) +
  geom_bar() +
  coord_polar(theta = "y") +
  labs(fill = "Furnishing Status", title = "Distribution of Furnishing Status") +
  theme(legend.position = "bottom")

df %>% 
  ggplot(aes(x= "", fill = PointofContact))+
  geom_bar()+
  coord_polar(theta = "y")+
  labs(fill = "Point of Contact", title = "Distribution of Point of Contact")+
  theme(legend.position = "bottom")

# Exploration 2-2 - Distribution of furnishing status and point of contact. 
df %>% 
  ggplot(aes(x = PointofContact, fill = FurnishingStatus)) +
  geom_bar(position = "dodge") +
  geom_text(aes(label = ..count..), stat = "count", position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Distribution of Points of Contact on Rental Price",
       x = "Point of Contact",
       y = "Count",
       fill = "Furnishing Status") +
  theme_minimal()

# Exploration 2-3 - Distribution of furnishing status on the rental price 
df %>% 
  ggplot(aes(x = FurnishingStatus, y = Rent, fill = FurnishingStatus)) +
  geom_violin() +
  labs(x = "Furnishing Status", y = "Rental Price", title = "Distribution of Furnishing Status on Rental Price") 


# Exploration 2-4 - Distribution of points of contact on the rental price
df %>% 
  ggplot( aes(x = PointofContact, y = Rent, fill = PointofContact)) +
  geom_boxplot() +
  labs(x = "Point of Contact", y = "Rental Price", title = "Distribution of Points of Contact on Rental Price") 

df %>% 
  ggplot(aes(x = Rent, fill = PointofContact)) +
  geom_density(alpha = 0.6) +
  labs(title = "Distribution of Point of Contact on Rental Price",
       x = "Rental Price",
       y = "Density") 


# Exploration 2-5 - Distribution of furnishing status and point of contact on the rental price 
df %>% 
  ggplot(aes(x = PointofContact, y = Rent, color = FurnishingStatus)) +
  geom_jitter(alpha = 0.8) +
  labs(x = "Rental Price", y = "", title = "Distribution of Furnishing Status and Point of Contact on Rental Price")

#Sub question 1: How does the furnishing status and point of contact in each city affect the rental price. 
#Analysis 2-1
Plot1 <- 
  df %>% 
  ggplot(aes(x = City, y = Rent, fill = FurnishingStatus)) +
  geom_violin(alpha = 0.7) +
  labs(title = "Violin Chart",
       subtitle = "Furnishing status in each city affect the rental price",
       x = "City",
       y = "Rent",
       fill = "Furnishing Status") +
  scale_fill_manual(values = c("red", "orange", "yellow")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", fill = NA)) +
  guides(fill = guide_legend(title = "Furnishing Status"))

Plot2 <- 
  df %>% 
  ggplot(aes(x = City, y = Rent, fill = PointofContact)) +
  geom_violin(alpha = 0.7) +
  labs(title = "",
       subtitle = "Point of Contact in each city affect the rental price",
       x = "City",
       y = "Rent",
       fill = "Point of Contact") +
  scale_fill_manual(values = c("red", "orange", "yellow")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(color = "black", fill = NA),  
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  guides(fill = guide_legend(title = "Point of Contact"))


CombinedPlot <- plot_grid(Plot1, Plot2, ncol = 1, align = "v")
CombinedPlot

#density plots
city_names <- unique(df$City)
plot_list <- list()

# Iterate over each city and create the density plot
for (city in city_names) {
  plot <- ggplot(subset(df, City == city), aes(x = Rent, fill = PointofContact)) +
    geom_density(alpha = 0.7) +
    labs(title = "",
         subtitle = paste("Point of Contact in", city, "affect the rental price"),
         x = "Rent",
         y = "Density",
         fill = "Point of Contact") +
    scale_fill_manual(values = c("red", "orange", "yellow")) +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA))
  
  plot_list[[city]] <- plot
}


CombinedPlots <- plot_grid(plotlist = plot_list, ncol = 2)
CombinedPlots


#density plots for each city
city_names <- unique(df$City)

PlotList <- list()

for (city in city_names) {
  plot <- ggplot(subset(df, City == city), aes(x = Rent, fill = FurnishingStatus)) +  
    geom_density(alpha = 0.7) +
    labs(title = "",
         subtitle = paste("Furnishing Status in", city, "affect the rental price"),  
         x = "Rent",
         y = "Density",
         fill = "Furnishing Status") +  
    scale_fill_manual(values = c("red", "orange", "yellow")) +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA))
  
  PlotList[[city]] <- plot
}

# Combine the density plots into a grid
CombinedPlots3 <- plot_grid(plotlist = PlotList, ncol = 2)
CombinedPlots3


#Sub question 2: How does the point of contact listed affect the rental price.
#Analysis 2-2
# Calculate mean Rent for each PointofContact group
Data2 <- df %>%
  group_by(PointofContact) %>%
  summarize(MeanofRent = mean(Rent))

# Create the scatter plot
df %>% 
  ggplot(aes(x = PointofContact, y = Rent, color = Rent)) +
  geom_point(position = position_jitter(width = 0.2, height = 0), alpha = 0.7) +
  geom_polygon(data = Data2,
               aes(x = PointofContact, y = MeanofRent, group = PointofContact),
               fill = NA, color = "black", alpha = 0.2) +
  labs(title = "Dot Chart",
       subtitle = "Point of Contact affect the rental price", 
       x = "Point of Contact",
       y = "Rent") +
  theme_minimal() +
  scale_color_gradient(low = "lightblue", high = "darkblue")  

# Convert the Rent to numeric
df$Rent <- as.numeric(as.character(df$Rent))

Df2Owner <- df[df$PointofContact == "Contact Owner", ]
Df2agent <- df[df$PointofContact == "Contact Agent", ]
Df2Builder <- df[df$PointofContact == "Contact Builder", ]

df %>% 
  ggplot(aes(x = Rent, y = PointofContact, color = PointofContact)) +
  geom_point(aes(shape = PointofContact), size = 2) +
  labs(title = "Point of contact affect the rental price") +
  scale_y_discrete(limits = unique(df$PointofContact)) +  
  geom_encircle(data = Df2Owner, aes(x = Rent, y = PointofContact), expand = 0.1, color = "red", alpha = 0.2) +
  geom_encircle(data = Df2agent, aes(x = Rent, y = PointofContact), expand = 0.1, color = "green", alpha = 0.2) +
  geom_encircle(data = Df2Builder, aes(x = Rent, y = PointofContact), expand = 0.1, color = "blue", alpha = 0.2)

#Sub question 3: Does the number of BHK affect the rental price with same furnishing status and point of contact?
#Analysis 2-3
Df3 <- df %>%
  group_by(BHK, FurnishingStatus, PointofContact) %>%
  summarise(MeanofRent = mean(Rent))

# Bubble plot
Df3 %>% 
  ggplot(aes(x = BHK, y = MeanofRent, size = MeanofRent, fill = interaction(FurnishingStatus, PointofContact))) +
  geom_point(shape = 21, alpha = 0.7) +  # Use shape 21 for filled circles
  labs(title = "Number of BHK affect the rental price with same furnishing status and point of contact",
       x = "Number of BHK",
       y = "Average Rent",
       fill = "Combination",
       size = "Average Rent") +
  theme_minimal() +
  theme(legend.position = "top") +
  guides(fill = guide_legend(title = "Combination")) +
  scale_size_continuous(range = c(5, 20))  


# Sub question 4: Does the tenant's preference affect the rental price with same furnishing status and point of contact?
# Analysis 2-4
df %>% 
  ggplot(aes(x = TenantPreferred, y = Rent, fill = FurnishingStatus)) +
  geom_boxplot() +
  geom_jitter(aes(color = FurnishingStatus), width = 0.2, size = 2.5, alpha = 0.7) +
  theme_minimal() +
  labs(x = "Tenant Preferred", y = "Rent", title = "Box Plot with Points of Rent by Tenant Preferred") +
  scale_color_manual(values = c("Unfurnished" = "red", "Semi-Furnished" = "orange", "Furnished" = "green")) +
  scale_fill_manual(values = c("Unfurnished" = "red", "Semi-Furnished" = "orange", "Furnished" = "green")) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )


# base plot 
BasePlot4 <- df %>% 
  ggplot(aes(x = TenantPreferred, y = Rent, fill = FurnishingStatus)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x = "Tenant Preferred", y = "Rent", title = "Polar Bar Plot") +
  scale_fill_manual(values = c("Unfurnished" = "red", "Semi-Furnished" = "orange", "Furnished" = "green")) +
  coord_polar(theta = "x") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# Create facet plot for PointofContact
FacetPlot <- BasePlot4 + facet_grid(TenantPreferred ~ .) +
  labs(subtitle = "Tenant preferred affect rental price with same furnishing status and point of contact")
FacetPlot

#=====================================================================

#Data Exploration 3-1
#summary of Rent,BHK, and number of bathrooms
summary(df$Rent)
summary(df$BHK)
summary(df$Bathroom)

#plot
ggplot(df, aes(x = Rent)) + geom_freqpoly() + 
  labs(x = "Rent(Rupee)")
ggplot(df, aes(x = BHK)) + geom_bar()
ggplot(df, aes(x = Bathroom)) + geom_bar()

#check top 10 rows
head(arrange(dfbefore,desc(Rent)), 10)
head(arrange(dfbefore,desc(BHK)), 10)
head(arrange(dfbefore,desc(Bathroom)), 10)

#:remove out-liner by getting means or replace with similar data
dfnew = filter(df, BHK == 1)
mean(as.numeric(dfnew$Bathroom))
df$Bathroom[df$Bathroom == 10] = 1

#Exploration 3-2 - Distribution of BHK
ggplot(df, aes(x = BHK)) + 
  geom_bar(fill = "darkgreen") + 
  labs(title = "The distribution of BHK")

ggplot(df, aes(x = "", fill = as.character(BHK))) +
  geom_bar() +
  coord_polar(theta = "y") +
  labs(fill = "BHK", title = "Distribution of BHK") +
  theme(legend.position = "bottom")

#data transformation
df_bhk = group_by(df, BHK) %>% 
  count() %>% 
  ungroup() %>% 
  mutate (perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>% 
  mutate(labels = scales::percent(perc))
View(df_bhk)

#plot pie chart
ggplot(df_bhk, aes(x = "", y = perc, fill = BHK)) +
  geom_col() +
  geom_label(aes(label = labels),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  coord_polar(theta = "y")

#Exporation 3-3 - Distribution of number of Bathroom
ggplot(df, aes(x = Bathroom)) + 
  geom_bar(fill = "cyan3") + 
  labs(title = "The distibution of number of bathrooms")

ggplot(df, aes(x = "", fill = as.character(Bathroom))) +
  geom_bar() +
  coord_polar(theta = "y") +
  labs(fill = "Bathroom", title = "Distribution of number of bathrooms") +
  theme(legend.position = "bottom")
view(filter(df,df$Bathroom == 1))

summary(df$Bathroom)

#data transformation
df_bathroom = group_by(df, Bathroom) %>% 
  count() %>% 
  ungroup() %>% 
  mutate (perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>% 
  mutate(labels = scales::percent(perc))
View(df_bathroom)

ggplot(df_bathroom, aes(x = "", y = perc, fill = Bathroom)) +
  geom_col() +
  geom_label(aes(label = labels),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  coord_polar(theta = "y")

#Exploration 3-4 - Relationship between number of bathrooms and BHK
ggplot(df, aes(x = as.numeric(Bathroom), y = as.numeric(BHK)))+ 
  geom_count(aes(color = after_stat(n))) + 
  geom_smooth(method="lm", se=F) +
  labs(title = "The relationship between Bathroom and BHK", x = "Number of bathrooms")

ggplot(df, aes(Bathroom)) +
  geom_bar(aes(fill = BHK)) +
  labs(titl = "Histogram on BHK across number of bathrooms")

df_bb = df
df_bb = group_by(df_bb, BHK, Bathroom) %>% 
  count()
View(df_bb)
ggplot(df_bb, aes(x=BHKxBathroom, y = n)) +
  geom_point(size = 3, col = "mediumturquoise") +
  geom_segment(aes(x = BHKxBathroom, xend = BHKxBathroom, y=0, yend = n))+
  labs(title = "The distribution of the BHK and number of bathroom composition.", y = "count")

df_bb = group_by(df,BHK, Bathroom) %>% 
  count()
View(df_bb)
ggplot(df_bb, aes(BHK,Bathroom)) +
  geom_tile(aes(fill = n)) +
  labs(title = "The distribution of the BHK and number of bathroom composition.") +
  scale_fill_gradient(low = "ghostwhite", high = "blue4")

typeof(df$Bathroom)
typeof(df$BHK)
df$BHK = as.numeric((df$BHK))
df$Bathroom = as.numeric((df$Bathroom))
cor(df$Bathroom, df$BHK)


attach(df)
a = lm(Bathroom~BHK)
plot(Bathroom, BHK)

#Sub Question 1: How does the number of BHK affect the rent?
#Analysis 3-1
ggplot(df, aes(x = BHK, y = Rent, fill = BHK)) + 
  geom_violin() + 
  labs(title = "The relationship between BHK and Rent")

ggplot(df, aes(x = BHK, y = Rent, color = City)) + 
  geom_point() +
  labs(title = "The relationship between BHK and Rent(and City)")

#Sub Question 2: How does the number of bathrooms affect the rent?
#Data Analysis 3-2
ggplot(df, aes(x = Rent, fill = Bathroom)) + 
  geom_density(alpha = 0.8) +
  labs(title = "The distribution of Rent among different number of bathrooms.")

ggplot(df, aes(y = Rent, x = Bathroom, fill = Bathroom)) + 
  geom_boxplot() +
  labs(title = "The distribution of Rent among different number of bathrooms.")

ggplot(df, aes(y = Rent, x = Bathroom)) + 
  geom_point(aes(col = Rent ,size = Size, shape = City)) +
  labs(title = "The distribution of Size among different number of bathrooms.")

#Sub Question 3: What is the relationship between number of bathrooms, BHK and rental price?
#Analysis 3-3
library(ggcorrplot)
df_rbb = data.frame(rent = df$Rent, bhk = as.numeric(df$BHK), 
                    bathroom = as.numeric(df$Bathroom))
corr = round(cor(df_rbb),1)

ggcorrplot(corr,hc.order= TRUE, type = "lower", 
           lab = TRUE, lab_size = 3, method = "circle",
           colors = c("blue", "white", "purple"),
           title = "Correlogram of rent, bhk and bathroom")

ggplot(df, aes(x=BHK, y=Rent, fill =Bathroom)) +
  geom_boxplot()

ggplot(df, aes(x = Bathroom, y = Rent, fill = Bathroom)) + geom_boxplot() + 
  labs(title = "The relationship between Bathroom and Rent") +
  facet_wrap(~BHK)


#Question 4 - How does the location of rental space affect the rental price?

#Data Exploration 4-1: Summary Statistics of City (Distance from the capital , City Population, City Size)
#descriptive analysis
descr(df$CityDistanceFromCapital) 
descr(df$CityPopulation)
descr(df$CitySize)

Q1a <- quantile(df$CityDistanceFromCapital, 0.25)
Q3a <- quantile(df$CityDistanceFromCapital, 0.75)
IQRa <- Q3a - Q1a
message("Interquartile Range of City Distance from Capital (New Delhi):", IQRa)

Q1b <- quantile(df$CityPopulation, 0.25)
Q3b <- quantile(df$CityPopulation, 0.75)
IQRb <- Q3b - Q1b
message("Interquartile Range of City Population:", IQRb)

Q1c <- quantile(df$CitySize, 0.25)
Q3c <- quantile(df$CitySize, 0.75)
IQRc <- Q3c - Q1c
message("Interquartile Range of City Size:", IQRc)

#Data Exploration 4-2: Distribution of City on Rent
df %>% 
  ggplot(aes(x = City, y = Rent, color = City)) +
  geom_point()+
  labs(title = "Distribution of Rent on City")
  
df %>% 
  group_by(City) %>% 
  summarize(mean_rent = mean(Rent)) %>% 
  ggplot(aes(x = "", y = mean_rent, fill = City)) +
  geom_bar(stat = "identity")+
  coord_polar(theta = "y")+
  labs(title = "Distribution of Mean Rent by City", y = "Mean Rent")
  
#Data Exploration 4-3: Correlation between the Distance to the City Centre, City Population,City Size and its rental price
CorCityDistance<- df %>% 
  select(CityDistanceFromCapital,CityPopulation, CitySize, Rent)
CorCityDistanceMatrix <- cor(CorCityDistance)
CorCityDistanceMatrix

#Data Exploration 4-4: Distribution of Rental Properties’ Cities based on Month Posted
df %>% 
  group_by(City,MonthPosted) %>% 
  summarise(Frequency = n()) %>% 
  ggplot(aes(x = City,y = Frequency, fill = MonthPosted))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of Rental Properties’ Cities based on Month Posted", x = "City", 
       y = "Frequency")

#Data Exploration 4-5: Distribution of Floor Level in each city
df %>% 
  ggplot(aes(x = TotalFloorNum, y = PropertyFloorNum, colour = City))+
  geom_boxplot()+
  labs(title = "Distribution of Floor Level in Each City", x = "Total Floor Number", 
       y = "Property Floor Number", subtitle = "Color by City")
  
#Data Exploration 4-6: Distribution of Tenant Preferred in each city
df %>% 
  group_by(City,TenantPreferred) %>% 
  summarize(Frequency = n()) %>% 
  ggplot(aes(x = City, y = Frequency, fill = City))+
  geom_violin()+
  labs(title = "Distribution of Tenant Preferred in Each City", x = "City", y = "Frequency of Tenant Preferred")

#Sub Question 1 – Does the Distance From Capital, City Population and City Size affect the rental price?
#Analysis 4-1
#data transformation

CitySizeQuartile <- quantile(df$CitySize, probs = c(0, 0.25, 0.5, 0.75, 1))

CitySizeQuartileLabels <- c("City Size Quartile 1", "City Size Quartile 2", "City Size Quartile 3", "City Size Quartile 4")

# Categorize CitySize into quartiles
CityDf <- df %>% 
  mutate(CitySizeQuartiles = cut(CitySize, breaks = CitySizeQuartile, labels = CitySizeQuartileLabels, include.lowest = TRUE))

CityDf %>% 
  ggplot(aes(x = CityPopulationCategory, y = Rent, fill = CityPopulationCategory)) +
  geom_boxplot() +
  facet_grid(~CitySizeQuartiles)+
  labs(title = "Rental Prices by City Population Category",
       subtitle = "Wrapped by City Size Quartile",
       x = "City Population Category", y = "Rent")


CityDf %>% 
  ggplot(aes(x = CityDistanceFromCapital, y = RentQuartile, fill = RentQuartile)) +
  geom_density_ridges(alpha=0.6, stat="binline", bins=20) +
  theme_ridges() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  facet_grid(~CitySizeQuartiles)+
  labs(title = "Rental Prices by City Distance From Capital",
       subtitle = "Wrapped by City Size Quartile",
       x = "City Distance From Capital", y = "Rental Quartile")+
  theme(panel.border = element_rect(color = "black", fill = NA))
  
  
  
Category <- c(5000000, 10000000, 20000000, Inf) #Inf - infinity

# Mutate CityPopulation to categorical levels
CityDf <- df %>% 
  mutate(CityPopulationCategory = cut(CityPopulation, breaks = Category,
                                      labels = c("Low Population", "Medium Population", "High Population"),
                                      include.lowest = TRUE))


CityDf %>% 
  ggplot(aes(x = CitySize,y = Rent, color = CityDistanceFromCapital, shape = City))+
  geom_point()+
  facet_grid(~CityPopulationCategory)+
  labs(title = "Distribution of City Size on Rent", 
       subtitle = "Wrapped by City Population, Colored by Distance From Capital, Shape by City",
       x = "City Size", y = "Rent")
  
#Sub Question 2 – Are there any seasonal trends in the rental price across each city according to the month posted?
#Analysis 4-2
df %>% 
  ggplot(aes(x = MonthPosted, y = Rent, group = City )) +
  geom_point(aes(color = City), size = 3) +
  geom_line(aes(color = City), size = 1) +
  labs(title = "Rent Trends across Months for Different Cities",
       x = "Month Posted", y = "Rent") +
  theme_minimal() +
  geom_vline(xintercept=1, linetype="dashed", size=.1) + 
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  geom_vline(xintercept=3, linetype="dashed", size=.1) + 
  geom_vline(xintercept=4, linetype="dashed", size=.1) 


df %>%
  ggplot(aes(x = MonthPosted, y = Rent, group = City, color = City)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  facet_grid(~ CityPopulation, scales = "free_y", labeller = as_labeller(~ paste0("City Population: ", .x))) +
  labs(
    title = "Rent Trends across Months for Different Cities",
    x = "Month Posted",
    y = "Rent"
  ) +
  theme_minimal() +
  geom_vline(xintercept = c(1, 2, 3, 4), linetype = "dashed", size = 0.1) +
  theme(legend.position = "bottom")


#Sub Question 3 - Does the floor level in each city affect the rental price?
#Analysis 4-3
df %>%
  ggplot(aes(x = TotalFloorNum, y = Rent, color = City)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between Total Property Floor Number and Rental Price",
       x = "Total Property Floor Number",
       y = "Rental Price",
       color = "City") +
  theme_minimal()

df %>%
  ggplot(aes(x = PropertyFloorNum, y = Rent, color = City)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between Rented Property Floor Number and Rental Price",
       x = "Rented Property Floor Number",
       y = "Rental Price",
       color = "City") +
  theme_minimal()


df %>%
  ggplot() +
  geom_point(aes(x = TotalFloorNum, y = Rent, color = City, shape = "Total Property"), size = 2) +
  geom_smooth(aes(x = TotalFloorNum, y = Rent, color = City), method = "lm", se = FALSE, size = 1) +
  geom_point(aes(x = PropertyFloorNum, y = Rent, color = City, shape = "Rented Property"), size = 2) +
  geom_smooth(aes(x = PropertyFloorNum, y = Rent, color = City), method = "lm", se = FALSE, size = 1, linetype = "dashed") +
  labs(title = "Relationship between Property Floor Numbers and Rental Price",
       x = "Property Floor Number",
       y = "Rental Price",
       subtitle = "Solid Line = Total Property Floor Number | Dashed Line = Rented Property Floor Number",
       color = "City") +
  scale_shape_manual(
    values = c("Total Property" = 16, "Rented Property" = 1),
    guide = guide_legend(
      title = "Property Type",
      override.aes = list(shape = c(16, 1))
    )
  ) +
  theme_minimal() +
  facet_grid(~ City) +
  theme(legend.position = "bottom")




CorCityFloor<- df %>% 
  select(CityDistanceFromCapital,CityPopulation, CitySize, PropertyFloorNum,TotalFloorNum,Rent)
CorCityFloorMatrix <- cor(CorCityFloor)
CorCityFloorMatrix


#Sub Question 4 – Does the Tenant Preferred and Point of Contact in each city affect the rental price?
#Analysis 4-3
TPDf <- df %>% 
  select(TenantPreferred, Rent, City) %>%
  group_by(TenantPreferred, City) %>%
  summarize(mean_rent = mean(Rent))

# Create a dot chart using dotchart
dotchart(TPDf$mean_rent, labels = TPDf$TenantPreferred,
         groups = TPDf$City,
         main = "Relationship between Tenant Preferred and Rental Price",
         xlab = "Mean Rental Price",
         ylab = "Point of Contact",
         col = c("darkblue", "deepskyblue", "purple","pink","orange","darkgrey"),
         cex = 1.4,
         pch = 19)

df %>% 
  ggplot(aes(x = City, y = Rent, fill = PointofContact)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Rent by Point of Contact and City",
       x = "City",
       y = "Rent",
       fill = "Point of Contact") +
  theme_minimal()

#=========================================================================================

#=========================================================================================
#Extra Feature 1
#Question 1 - Does the spacial extent of a rental property affect its rental price?
# Split the data into 80% training, 20% testing
set.seed(123) 
SampleSize <- floor(0.8 * nrow(df))
TrainIndex <- sample(seq_len(nrow(df)), size = SampleSize)

TrainData <- df[TrainIndex, ]
TestData <- df[-TrainIndex, ]

# Linear regression model
lmModel <- lm(Rent ~ Size, data = TrainData)
summary(lmModel)

# Predict rental prices on the test data
Prediction <- predict(lmModel, newdata = TestData)
RMSE <- sqrt(mean((TestData$Rent - Prediction)^2))
MAE <- mean(abs(TestData$Rent - Prediction))

cat("Root Mean Squared Error:", RMSE, "\n")
cat("Mean Absolute Error:", MAE, "\n")

PredictedDf <- data.frame(Actual = TestData$Rent, Predicted = Prediction, Size = TestData$Size)

PredictedDf %>% 
  ggplot(aes(x = Actual, y = Predicted, color = Size)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(x = "Actual Rent", y = "Predicted Rent") +
  scale_color_gradient(low = "blue", high = "red", name = "Property Size") +
  ggtitle("Actual Rent VS Predicted Rent (Color by Property Size)")

#Extra Feature 2
df_cbbr = data.frame(rent = df$Rent, bhk = as.numeric(df$BHK), 
                     bathroom = as.numeric(df$Bathroom))
sample_data = sample.split(df_cbbr, SplitRatio = 0.8)
train_data = subset(df_cbbr, sample_data == TRUE)
test_data = subset(df_cbbr, sample_data == FALSE)

model_decision_tree = ctree(bhk~ ., train_data)
plot(model_decision_tree)

predict_model = predict(model_decision_tree, test_data)
m_at = table(test_data$bhk, predict_model)
m_at

ac_test = sum(diag(m_at)) / sum(m_at)
print(paste('Accuracy for the test is found to be', ac_test))

#Linear Regression
df_size = data.frame(rent = df$Rent, size = df$Size)
df_test = as.data.frame(4000)
colnames(df_test) = "size"
cor(df_size$rent, df_size$size)
View(df_size)
model_linear_regression = lm(rent ~ size, data = df_size)
summary(model_linear_regression)
predict(model_linear_regression, newdata = df_test)

#Extra Feature 3
#mapping
register_google(key ="AIzaSyBtDymvJjBP5VRjSa1Yt9-62vyjnKGcrrg",)
india_map = qmap("India", zoom = 5,source = "google", maptype = "roadmap")
df_city = group_by(df, City) %>% 
  summarise(meanrent = mean(Rent),.groups = 'drop') %>% 
  arrange(meanrent) %>% 
  mutate(loc = geocode(as.character(City)))
india_map + geom_point(aes(x = loc$lon, y = loc$lat, color = meanrent), data = df_city, size = 7) +
  geom_encircle(aes(x = loc$lon, y = loc$lat), data = filter(df_city,meanrent == max(meanrent)),size =2, color = "red") +
  labs(title = "The mean of rental price in different City")

#Decision Tree
ggplot(df, aes(x = Bathroom, y = Rent, color = City)) + 
  geom_point() + 
  labs(title = "The relationship between Bathroom and Rent") +
  facet_wrap(~BHK)



#=====END========
cat("\014")
rm(list = ls())