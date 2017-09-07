requiredPackages = c('dplyr','stringr','tidyr')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)){ 
    install.packages(p)
  }
  library(p,character.only = TRUE)
}

# creating and setting working directory
dir.create("Upgrade_case")
setwd("upgrade_case")

# downlaoding project data sets and other files
myurl1 <- "https://cdn.upgrad.com/UpGrad/temp/d934844e-5182-4b58-b896-4ba2a499aa57/companies.txt"
myurl2 <- "https://cdn.upgrad.com/UpGrad/temp/4c3b5ed0-e5dc-4838-89a2-173d8707d857/rounds2.csv"
myurl3 <- "https://cdn.upgrad.com/UpGrad/temp/231dc91c-0642-470d-a362-29ddcd7142ce/mapping.csv"
myurl4 <- "https://cdn.upgrad.com/UpGrad/temp/4201978b-5ec1-4138-84aa-767bc385e6d7/investments.xlsx"
myurl5 <- "https://cdn.upgrad.com/UpGrad/temp/3bc93ac6-4c95-4a6a-8b23-17e1b8e055c1/Spark%20Funds%20Presentation.pptx"
myurl6 <- "https://raw.githubusercontent.com/navmasali/Course1/master/mapping_missing_sectors.csv"
download.file(myurl1, destfile="./companies.txt", method="curl")
download.file(myurl2, destfile="./rounds2.csv", method="curl")
download.file(myurl3, destfile="./mapping.csv", method="curl")
download.file(myurl4, destfile="./investments.xlsx", method="curl")
download.file(myurl5, destfile="./Spark Funds Presentation.pptx", method="curl")
download.file(myurl6, destfile="./mapping_missing_sectors.csv", method="curl")

# Load the data from companies and rounds2 file.
companies <- read.delim("companies.txt", header =  TRUE , stringsAsFactors = FALSE)
rounds2 <- read.csv("rounds2.csv", header = TRUE, stringsAsFactors = FALSE)

#load the mapping file and the missing mappings, mapping_missing_sectors is a CSV
# file that contains the mappings of the missing primary sectors from the mappings file provided.
mapping <- read.csv("mapping.csv", header =  TRUE, stringsAsFactors = FALSE)
missing_mappings <- read.csv("mapping_missing_sectors.csv", header =  TRUE, stringsAsFactors = FALSE)
colnames(missing_mappings) <- c("category_list", "main_sector")

#clean(convert to lower case) the permalink column in companies and rounds2 dataframe
companies <- mutate(companies, permalink1 = str_to_lower(companies$permalink))
companies <- companies[,-1]

rounds2 <- mutate(rounds2, permalink1 = str_to_lower(rounds2$company_permalink))
rounds2 <- rounds2[,-1]

# Find the distinct number of companies, permalink1 is the unique identifier for the company
n_distinct(companies$permalink1)
n_distinct(rounds2$permalink1)

# Are there any companies in the rounds2 file which are not present in companies? 
setdiff(rounds2$permalink1, companies$permalink1)

#merge companies and rounds2
master_frame <- inner_join(companies, rounds2, by = "permalink1")
#master_frame_left <- left_join(companies, rounds2, by = "permalink1")

# checking the number of NA values in the column "raised_amount_usd" of master_frame
sum(is.na(master_frame$raised_amount_usd))

#replacing NA values with numeric 0
master_frame$raised_amount_usd[is.na(master_frame$raised_amount_usd)] <- 0

#Grouping on Fund type, and calculating mean(raised_amount_type) 
master_fund_group <- group_by(master_frame, funding_round_type)
fund_investment <- summarise(master_fund_group, mean(raised_amount_usd))
colnames(fund_investment) <- c("fund_type", "avg_raised_amt")

#Group by country, and calculating mean(raised_amount_type) for Venture funding type.
master_country_group <- filter(group_by(master_frame, country_code), funding_round_type == "venture")
top9 <- summarise(master_country_group, sum(raised_amount_usd))
colnames(top9) <- c("Country_Code","Total_Sum")
top9 <- head(arrange(top9, desc(Total_Sum)),9)

#Primary Sector extract from category_list column
primary_sector_list <- str_split(master_frame$category_list, pattern="\\|")
primary_sector <- sapply(primary_sector_list, function(x) x[1][1])
master_frame[,"primary_sector"] <- primary_sector

#Take a backup of the mapping data frame
mapping_bkp <- mapping
#Take a backup of the master_frame
master_frame_bkp <- master_frame

# replace "0" with "na" and "2.na" with "2.0" one after another
mapping$category_list <- gsub("0", "na",mapping$category_list)
mapping$category_list <- gsub("2.na", "2.0", mapping$category_list)

#Converting the primary_sector and category_list columns to lowercase
master_frame$primary_sector <- str_to_lower(master_frame$primary_sector)
mapping$category_list <- str_to_lower(mapping$category_list)

#Wide to long conversion of the mapping dataframe
mapping_long <- gather(mapping, main_sector, nval, 2:10)
mapping_long <- mapping_long[!(mapping_long$nval == 0), ]
mapping_long <- mapping_long[,-3]

#Take backup of the mapping_long dataframe
mapping_long_bkp <- mapping_long

#Combine the original mappings file with the missing_mapping file
mapping_long <- rbind(mapping_long,missing_mappings)

#merge the mapping_long and master_frame on primary sector
final_master <- merge(master_frame, mapping_long, by.x = "primary_sector", by.y = "category_list")

#Creating the data frames for the 3 favourable english speaking countries and FT = venture

india_investment <- filter(final_master, country_code == "IND", funding_round_type == "venture")
usa_investment <- filter(final_master, country_code == "USA", funding_round_type == "venture")
gbr_investment <- filter(final_master, country_code == "GBR", funding_round_type == "venture")

#create data frames with groupings on main sector
group_main_sector <- function(p)
{
  sector_group <- group_by(p, main_sector)
}

india_invest_grp <- group_main_sector(india_investment)
usa_invest_grp <- group_main_sector(usa_investment)
gbr_invest_grp <- group_main_sector(gbr_investment)


# Summarises the main sectors with Avg raised amount, number of investments
# Also selects the main sectors where investments are between 5 and 15 million
avg_raised_amt <- function(p)
{
  country_main_sector <- summarise(p, mean(raised_amount_usd), n())
  colnames(country_main_sector) <- c("main_sector","avg_raised_amt_usd","no. of investments")
  country_main_sector <- subset(country_main_sector, avg_raised_amt_usd > 5000000 & avg_raised_amt_usd < 15000000)
  return(country_main_sector)
}

# calling the avg_raised_amt function 
india_main_sector <- avg_raised_amt(india_invest_grp)
usa_main_sector <- avg_raised_amt(usa_invest_grp)
gbr_main_sector <- avg_raised_amt(gbr_invest_grp)

# final output
write.csv(final_master,"final_master.csv")