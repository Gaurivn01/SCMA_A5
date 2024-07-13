# HISTOGRAM AND BARPLOT

# Set the working directory and verify it
setwd("C:/Users/gauri/OneDrive/Documents/VCU/SCMA")
getwd()
#install.packages(dplyr)
# Function to install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Load required libraries
libraries <- c("dplyr", "readr", "readxl", "tidyr", "ggplot2", "BSDA")
lapply(libraries, install_and_load)

# Reading the file into R
data <- read.csv("NSSO68.csv")

# Filtering for GUJ
df <- data %>%
  filter(state_1 == "GUJ")

# Display dataset info
cat("Dataset Information:\n")
print(names(df))
print(head(df))
print(dim(df))

# Finding missing values
missing_info <- colSums(is.na(df))
cat("Missing Values Information:\n")
print(missing_info)

# Subsetting the data
gujnew <- df %>%
  select(state_1, District, Region, Sector, State_Region, Meals_At_Home, ricepds_v, Wheatpds_q, chicken_q, pulsep_q, wheatos_q, No_of_Meals_per_day)

# Impute missing values with mean for specific columns
impute_with_mean <- function(column) {
  if (any(is.na(column))) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  }
  return(column)
}
gujnew$Meals_At_Home <- impute_with_mean(gujnew$Meals_At_Home)

# Finding outliers and removing them
remove_outliers <- function(df, column_name) {
  Q1 <- quantile(df[[column_name]], 0.25)
  Q3 <- quantile(df[[column_name]], 0.75)
  IQR <- Q3 - Q1
  lower_threshold <- Q1 - (1.5 * IQR)
  upper_threshold <- Q3 + (1.5 * IQR)
  df <- subset(df, df[[column_name]] >= lower_threshold & df[[column_name]] <= upper_threshold)
  return(df)
}

outlier_columns <- c("ricepds_v", "chicken_q")
for (col in outlier_columns) {
  gujnew <- remove_outliers(gujnew, col)
}

# Summarize consumption
gujnew$total_consumption <- rowSums(gujnew[, c("ricepds_v", "Wheatpds_q", "chicken_q", "pulsep_q", "wheatos_q")], na.rm = TRUE)

# Summarize and display top consuming districts and regions
summarize_consumption <- function(group_col) {
  summary <- gujnew %>%
    group_by(across(all_of(group_col))) %>%
    summarise(total = sum(total_consumption)) %>%
    arrange(desc(total))
  return(summary)
}

district_summary <- summarize_consumption("District")
region_summary <- summarize_consumption("Region")

cat("Top Consuming Districts:\n")
print(head(district_summary, 4))
cat("Region Consumption Summary:\n")
print(region_summary)

# Rename districts and sectors
district_mapping <- c("1" = "Kachchh","2" = "Banas Kantha","3" = "Patan",
                      "4" = "Mahesana","5" = "Sabar Kantha","6" = "Gandhinagar",
                      "7" = "Ahmedabad","8" = "Surendranagar","9" = "Rajkot", 
                      "10" = "Jamnagar", "11" = "Porbandar", "12" = "Junagadh", 
                      "13" = "Amreli", "14" = "Bhavnagar", "15" = "Anand", 
                      "16" = "Kheda", "17" = "Panch Mahals", "18" = "Dohad",
                      "19" = "Vadodara", "20" = "Narmada", "21" = "Bharuch", 
                      "22" = "Surat", "23" = "The Dangs", "24" = "Navsari", 
                      "25" = "Valsad")
sector_mapping <- c("2" = "URBAN", "1" = "RURAL")

gujnew$District <- as.character(gujnew$District)
gujnew$Sector <- as.character(gujnew$Sector)
gujnew$District <- ifelse(gujnew$District %in% names(district_mapping), district_mapping[gujnew$District], gujnew$District)
gujnew$Sector <- ifelse(gujnew$Sector %in% names(sector_mapping), sector_mapping[gujnew$Sector], gujnew$Sector)

View(gujnew)

hist(gujnew$total_consumption, breaks = 10, col = 'blue', border = 'black', 
     xlab = "Consumption", ylab = "Frequency", main = "Consumption Distribution in Gujarat")

GUJ_consumption <- aggregate(total_consumption ~ District, data = gujnew, sum) 
View(GUJ_consumption)
??barplot
barplot(GUJ_consumption$total_consumption, 
        names.arg = GUJ_consumption$District, 
        las = 2, # Makes the district names vertical
        col = 'blue', 
        border = 'black', 
        xlab = "District", 
        ylab = "Total Consumption", 
        main = "Total Consumption per District",
        cex.names = 0.7) # Adjust the size of district names if needed


#-------------------------------------------------------------------------------


# MAPPING VARIABLE 'nontotal_v' TO KARNATAKA STATE
setwd("C:/Users/gauri/OneDrive/Documents/VCU/SCMA")
getwd()
#install.packages(dplyr)
# Function to install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Load required libraries
libraries <- c("dplyr", "readr", "readxl", "tidyr", "ggplot2", "BSDA")
lapply(libraries, install_and_load)

# Reading the file into R
data <- read.csv("NSSO68.csv")

# Filtering for KA
df <- data %>%
  filter(state_1 == "KA")

# Display dataset info
cat("Dataset Information:\n")
print(names(df))
print(head(df))
print(dim(df))

# Finding missing values
missing_info <- colSums(is.na(df))
cat("Missing Values Information:\n")
print(missing_info)

# Subsetting the data
kanew <- df %>%
  select(state_1, District, Region, Sector, State_Region, Meals_At_Home, 
         eggsno_v,	fishprawn_v,	goatmeat_v,	beef_v,	pork_v,	chicken_v,	
         othrbirds_v, No_of_Meals_per_day)

# Impute missing values with mean for specific columns
impute_with_mean <- function(column) {
  if (any(is.na(column))) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  }
  return(column)
}
kanew$Meals_At_Home <- impute_with_mean(kanew$Meals_At_Home)

# Finding outliers and removing them
remove_outliers <- function(df, column_name) {
  Q1 <- quantile(df[[column_name]], 0.25)
  Q3 <- quantile(df[[column_name]], 0.75)
  IQR <- Q3 - Q1
  lower_threshold <- Q1 - (1.5 * IQR)
  upper_threshold <- Q3 + (1.5 * IQR)
  df <- subset(df, df[[column_name]] >= lower_threshold & df[[column_name]] <= upper_threshold)
  return(df)
}

outlier_columns <- c('eggsno_v',	'fishprawn_v',	'goatmeat_v',	'beef_v',	'pork_v',	'chicken_v',	
                     'othrbirds_v')
for (col in outlier_columns) {
  kanew <- remove_outliers(kanew, col)
}

# Summarize consumption
kanew$nonvegtotal_v <- rowSums(kanew[, c('eggsno_v', 'fishprawn_v', 'goatmeat_v', 'beef_v', 'pork_v', 'chicken_v', 'othrbirds_v')], na.rm = TRUE)

# Summarize and display top consuming districts and regions
summarize_consumption <- function(group_col) {
  summary <- kanew %>%
    group_by(across(all_of(group_col))) %>%
    summarise(total = sum(nonvegtotal_v)) %>%
    arrange(desc(total))
  return(summary)
}

district_summary <- summarize_consumption("District")
region_summary <- summarize_consumption("Region")

cat("Top Consuming Districts:\n")
print(head(district_summary, 4))
cat("Region Consumption Summary:\n")
print(region_summary)

# Rename districts and sectors
district_mapping <- c("1" = "Belgaum","2" = "Bagalkot","3" = "Bijapur",
                      "4" = "Gulbarga","5" = "Bidar","6" = "Raichur",
                      "7" = "Koppal","8" = "Gadag","9" = "Dharwad", 
                      "10" = "Uttara Kannada", "11" = "Haveri", "12" = "Bellary", 
                      "13" = "Chitradurga", "14" = "Davanagere", "15" = "Shimoga", 
                      "16" = "Udupi", "17" = "Chikmagalur", "18" = "Tumkur",
                      "19" = "Kolar", "20" = "Bangalore", "21" = "Bangalore Rural", 
                      "22" = "Mandya", "23" = "Hassan", "24" = "Dakshina Kannada", 
                      "25" = "Kodagu", "26"="Mysore","27"="Chamarajanagar",
                      "28"="Ramanagar","29"="Chikkaballapura")
sector_mapping <- c("2" = "URBAN", "1" = "RURAL")

kanew$District <- as.character(kanew$District)
kanew$Sector <- as.character(kanew$Sector)
kanew$District <- ifelse(kanew$District %in% names(district_mapping), district_mapping[kanew$District], kanew$District)
kanew$Sector <- ifelse(kanew$Sector %in% names(sector_mapping), sector_mapping[kanew$Sector], kanew$Sector)

View(kanew)

KA_consumption <- aggregate(nonvegtotal_v ~ District, data = kanew, sum) 
View(KA_consumption)

##MAPPING
library(ggplot2) 
library(sf) # mapping
library(dplyr) 
Sys.setenv("SHAPE_RESTORE_SHX" = "YES") 

data_map <- st_read("C:/Users/gauri/Downloads/KARNATAKA_DISTRICTS.geojson") 
View(data_map)

data_map <- data_map %>% 
  rename(District = dtname) 
colnames(data_map) 
data_map_data <- merge(KA_consumption,data_map,by = "District") 
View(data_map_data)
ggplot(data_map_data) + 
  geom_sf(aes(fill =nonvegtotal_v, geometry = geometry)) + 
  scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Total Consumption_by_District") 

ggplot(data_map_data) + 
  geom_sf(aes(fill = nonvegtotal_v, geometry = geometry)) + 
  scale_fill_gradient(low = "yellow", high = "red") + 
  ggtitle("Total Consumption by District") +
  geom_sf_text(aes(label = District, geometry = geometry), size = 3, color = "black")

