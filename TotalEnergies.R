# Load libraries:
library(mosaic)
library(dplyr)
library(GGally)
library(LSD)
library(tidyr)
library(caTools)
library(splitstackshape)
library(tidyverse)
library(purrr)
library(readxl)
library(openxlsx)
library(data.table)
library(zoo)

sales_overview <- read.csv("/Users/mathiasrivetsorensen/Desktop/Universitet/7. Semester/Projekt/CSVfiler/Salgsoversigt.csv", sep=";")
productinformation <- read.csv("/Users/mathiasrivetsorensen/Desktop/Universitet/7. Semester/Projekt/CSVfiler/Produktinformation.csv",sep=";")
inventory_snapshot <- read.csv("/Users/mathiasrivetsorensen/Desktop/Universitet/7. Semester/Projekt/CSVfiler/LagerUdtræk11-09-2023.csv", sep=";")
sales_prices <- read.csv("/Users/mathiasrivetsorensen/Desktop/Universitet/7. Semester/Projekt/CSVfiler/SalesPrices.csv", sep=";")
incoming_inventory <- read.csv("/Users/mathiasrivetsorensen/Desktop/Universitet/7. Semester/Projekt/CSVfiler/IndadgåendeLager.csv", sep=";")
DHL_prices <- read.csv("/Users/mathiasrivetsorensen/Desktop/Universitet/7. Semester/Projekt/CSVfiler/DHLprices.csv", sep=";")
customer_list <- read.csv("/Users/mathiasrivetsorensen/Desktop/Universitet/7. Semester/Projekt/CSVfiler/CustomerList.csv", sep=";")

##### REMOVING IRRELEVANT/INCONSISTENT ROWS #####
incoming_inventory <- incoming_inventory %>% 
  filter(X.3 != "F")
incoming_inventory <- incoming_inventory[-5247,]

##### REMOVE COLUMNS THAT ONLY CONTAIN 1 VALUE #####
sales_overview <- remove_non_unique(sales_overview)
productinformation <- remove_non_unique(productinformation)
inventory_snapshot <-  remove_non_unique(inventory_snapshot)
sales_prices <- remove_non_unique(sales_prices)
incoming_inventory <- remove_non_unique(incoming_inventory)
DHL_prices <- remove_non_unique(DHL_prices)
customer_list <- remove_non_unique(customer_list)

df_summary <- incoming_inventory %>%
 group_by(X.4) %>%
 summarise(n_distinct_y = n_distinct(X.6))
# check if there is any group where n_distinct_y is greater than 1
any(df_summary$n_distinct_y > 1)

##### COLUMN RENAMING #####
customer_list <- customer_list %>% 
  rename(customer = 'Kunde', country_code = "Land", customer_name = "Navn")
DHL_prices <- DHL_prices %>% 
  rename(expenditure = "Post", cost = "Pris")
incoming_inventory <- incoming_inventory %>% 
  rename(item_id = "Materialenummer", item_name = "Materialenavn", shipment_id = "Forsendelsesnummer", date_of_receipt = "Dato.for.modtagelse", quantity = "Antal", unit = "Enheder")

sales_overview <- sales_overview %>% 
  rename(date = 'X', quantity = 'X.1')

# remove capitalisation of colnames:
colnames(sales_overview) <- tolower(colnames(sales_overview))

# data formatting:
sales_overview$materialenummer <- as.character(sales_overview$materialenummer)
sales_overview$date <- as.POSIXct(sales_overview$date, format = "%d.%m.%Y")
sales_overview$quantity <- as.numeric(gsub("-","",sales_overview$quantity))
sales_overview$forsendelsesnr <- as.character(sales_overview$forsendelsesnr)
sales_overview$kundenr <- as.character(sales_overview$kundenr)

# remove NA:
sales_overview_NA <- sales_overview[which(is.na(sales_overview$quantity)),]
sales_overview <- anti_join(sales_overview, sales_overview_NA)

# aggregate order quantity by customer during 2020-2023:
sum_by_customer <- sales_overview %>%
  group_by(kundenr) %>%
  summarise_at(vars(quantity), list(sum = sum))

mean_by_customer <- sales_overview %>%
  group_by(kundenr) %>%
  summarise_at(vars(quantity), list(mean = mean))

sd_by_customer <- sales_overview %>%
  group_by(kundenr) %>%
  summarise_at(vars(quantity), list(sd = sd))

# check na values:
count(is.na(sum_by_customer$sum))
count(is.na(mean_by_customer$mean))
count(is.na(sd_by_customer$sd))

# higher number of NA rows for SD calculation due to SD not being applicable in R when there is only 1 observation for a group of "kundenr"
# instead the SD is set to 0 for these "kundenr"
sd_by_customer <- replace(sd_by_customer, is.na(sd_by_customer), 0)

# combine information regarding customer orders:

combined_sales_data <- cbind(sum_by_customer, mean_by_customer, sd_by_customer)
combined_sales_data <- combined_sales_data[,-c(3, 5)]

# calculate percentage of total order volume by customer
combined_sales_data$percentage_of_total_volume <- combined_sales_data$sum / sum(combined_sales_data$sum) * 100
combined_sales_data <- combined_sales_data %>% 
  arrange(desc(percentage_of_total_volume))

# aggregate monthly sales data:
monthly_sales <- sales_overview %>%
  group_by(year_month = format(date, format = "%Y-%m")) %>%  # Group by year and month
  summarise(total_quantity = sum(quantity))

# create database featuring all combinations of month/year + materialenummer
all_combinations <- expand.grid(
  year_month = as.character(format(seq(min(sales_overview$date), max(sales_overview$date), by = "1 month"), format = "%Y-%m")),
  materialenummer = unique(sales_overview$materialenummer)
)

# calculate monthly sales by product by grouping by both variables
monthly_sales_by_product <- sales_overview %>% 
  group_by(year_month = format(date, format = "%Y-%m"), materialenummer) %>% 
  summarise(quantity = sum(quantity)) %>% 
  ungroup()

# insert 0 into the months where no quantity was sold
monthly_sales_by_product <- all_combinations %>%
  left_join(monthly_sales_by_product, by = c("year_month", "materialenummer")) %>%
  mutate(quantity = coalesce(quantity, 0)) %>%
  arrange(materialenummer, year_month)

# average monthly sales
average_monthly_sales_by_product <- monthly_sales_by_product %>% 
  group_by(materialenummer) %>% 
  summarise(AverageMonthlySales = mean(quantity))

# inventory rule calculation (average * 2)
inventory_rule <- data.frame(
  materialenummer = average_monthly_sales_by_product$materialenummer,
  two_month_rule = average_monthly_sales_by_product$AverageMonthlySales * 2
)

# 



# inventory turnover rate - kræver at vi har indgående/udgående + start
## evt over en 2 måneders periode for at se den discrepancy der er mellem 2 måneders lager og hvor mange gange lageret bliver tømt over denne periode.
### vel hovedproblemet at der er mere lager end der er behov herfor
#### transportation og holding vel inverst korrelerede.

# fill rate
# pipeline inventory
# trend af inventory buildup


# create barchart of monthly sales
barplot(height = monthly_sales$total_quantity, names = monthly_sales$year_month, main = "Monthly Sales", las = 2)

# histogram of aggregated customer order size
hist(sum_by_customer$sum, breaks = 200, xlim = c(0, 5000),  main = "Histogram of Aggregated Customer Size", xlab = "Aggregated Order Quantity", ylab = "Frequency of Customers")
  
  