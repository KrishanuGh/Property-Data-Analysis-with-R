#################################################################################################
# Krishanu Ghosh
# CS 544 Fall 2018
# Final PRoject : Real Estate data ANalysis
#################################################################################################
library(jsonlite)
library(rjson)
library(sampling)
library(tidyverse)

################### READING FILE ################################################################
setwd("C:/Krishanu/MS/Boston University/MS CIS/CS544/project/")
json_file <- stream_in(file("DataInfinityPropertyData.json"))

################### DISCARD COLUMNS OF NO INTEREST ##############################################
json_file$reviews <- NULL
json_file$mlsNumber <- NULL
json_file$phones <- NULL
json_file$people <- NULL
json_file$keys <- NULL
json_file$id <- NULL
json_file$leasingTerms <- NULL
property.df <- NULL

################### BUILD DATA FRAME ###########################################################
property.df <- data.frame (address <- json_file$address,
                          city <- json_file$city,
                          postalCode <- json_file$postalCode,
                          province <- json_file$province,
                          country <- json_file$country,
                          dateAdded <- json_file$dateAdded,
                          dateUpdated <- json_file$dateUpdated,
                          latitude <- json_file$latitude,
                          longitude <- json_file$longitude,
                          lotSizeValue <- json_file$lotSizeValue,
                          numBathroom <- json_file$numBathroom,
                          numBedroom <- json_file$numBedroom,
                          numFloor <- json_file$numFloor,
                          propertyType <- json_file$propertyType,
                          floorSizeValue <- json_file$floorSizeValue
                          )

                        
for (i in (1:nrow(property.df))) {
  tryCatch(
    {
      property.df[i,"pricetMax"] <- as.numeric(json_file$prices[[i]][1,"amountMax"])
      property.df[i,"pricetMin"] <- as.numeric(json_file$prices[[i]][1,"amountMin"])
      property.df[i,"pricePerSqaurFoot"] <- as.numeric(json_file$prices[[i]][1,8])
      property.df[i,"propertyTax"] <- as.numeric(json_file$propertyTaxes[[i]][1,"amount"])
    },
    error=function(error_message) {
      property.df[i,"pricePerSqaurFoot"] <- NA
      property.df[i,"pricetMax"] <- NA
      property.df[i,"pricetMin"] <- NA
      property.df[i,"propertyTax"] <- NA
    }
  )
  
  school <- json_file$nearbySchools[[i]]

  if (is.null(school)) {
    property.df[i,"nearByElementarySchool"] <- NA
    property.df[i,"nearByMiddleSchool"] <- NA
    property.df[i,"nearByHighSchool"] <- NA
  }
  else {
    tryCatch(
      {
        school <- school[order(school$distanceValue),]
        school <- school[,"name"]
        property.df[i,"nearByElementarySchool"] <- school[str_detect(school,"Elementary")][1]
        property.df[i,"nearByMiddleSchool"] <- school[str_detect(school,"Middle")][1]
        property.df[i,"nearByHighSchool"] <- school[str_detect(school,"High")][1]        
      },
      error=function(error_message) {
        property.df[i,"nearByElementarySchool"] <- NA
        property.df[i,"nearByMiddleSchool"] <- NA
        property.df[i,"nearByHighSchool"] <- NA
        return(NA)
      }
    )

  }
}

names(property.df) <- c("address", "city", "postalCode","province","country",
                        "dateAdded","dateUpdated","latitude","longitude","lotSizeValue",
                        "numBathroom","numBedroom","numFloor",
                        "propertyType","floorSizeValue",
                        "priceMax","priceMin",
                        "pricePerSqaurFoot","propertyTax",
                        "nearByElementarySchool","nearByMiddleSchool","nearByHighSchool"
                        )
json_file <- NULL
####################################  DATA CLEANING : REMOVE OUTLIERS ###########################
## Smooth all outliers values in floorsize value
f <- fivenum(property.df$floorSizeValue)
property.df[c(80,82,100,108,116,117),c("floorSizeValue")] <- f[2]

## Two PRices found as 3 training 0s (1000 times) than expected normal value
## Smoothened by deviding with 1000
property.df[c(304,588),c("priceMax")] <- property.df[c(304,588),c("priceMax")]/1000

## Address had zip plus four. Smoothened it to Zip
property.df[,c("postalCode")] <- substr(property.df[,c("postalCode")],1,5)

## Remove Rows with NULL Postal Code as they are bad addresses and wrong prices
property.df <- property.df[!(is.na(property.df$postalCode)),]

## Remove Rows with NULL prices
property.df <- property.df[!(is.na(property.df$priceMax)),]

## Remove Rows with NULL province
property.df <- property.df[!(is.na(property.df$province)),]

## Remove Row with province = IA of City Nashua
property.df <- property.df[!(property.df$province=="IA"),]
#################################### DATA ANALYSIS #############################################
par(mfrow = c(1,1))
## Analysis for categorical variable with Plot
# By Zip / Postal Code
table(property.df$postalCode)
barplot(table(property.df$postalCode),xlab="ZIP Code",ylab="Tot No of Houses",
        col= hcl(0),las=2)

# By No of Bed Rooms
table(property.df$numBedroom)
barplot(table(property.df$numBedroom),xlab="No of Bedrooms",ylab="Tot No of Houses",
        col= hcl(0),las=2)

# By Property Type
table(property.df$propertyType)
barplot(table(property.df$propertyType),xlab="Property Type",ylab="Tot No of Houses",
        col= hcl(0),las=2)

## Analysis for numerical variable with Plot
# By Price per Sq Ft
hist(property.df$pricePerSqaurFoot,col=hcl(0),xlab = "Price per Sq Ft",
     ylab = "Frequency",main = "Histogram of Price per Sq Ft")

# By Price
hist(round(property.df$priceMin/1000),col=hcl(2),xlab = "Price ($K)",
     ylab = "Frequency",main = "Histogram of Price")

#By Floor Size
hist(property.df$floorSizeValue,col=hcl(0),xlab = "Floor size",
     ylab = "Frequency (Sq Ft)",main = "Histogram of Floor size")

# Bivariate Data Analysis
table(property.df$numBedroom,property.df$propertyType)
property.df.bv <- property.df[property.df$numBedroom > 0 & property.df$numBedroom <= 4,
                              c("propertyType","numBedroom")]
mosaicplot(table(property.df.bv$numBedroom,property.df.bv$propertyType),
           col="cyan",main= "Property Type vs No of Bedrooms",las=1)

## Examining the distribution of Price
price <- round(property.df$priceMax/1000)
price <- price[!is.na(price)]

hist(price,col=hcl(2),xlab = "Price In Thousands",ylab = "Frequency",
     main = "Histogram of Price")

## Drawing various random samples showing the applicability of Central Limit Theorem for price
samples <- 1000
pricebar <- numeric(samples)
par(mfrow = c(2,2))

for (size in c(40,60,80,100)) {
  for (i in 1:samples) {
    pricebar[i] <- mean(sample(price, size = size, replace = FALSE))
  }
  hist(pricebar, prob = TRUE, col = hcl(0),
       main = paste("Sample Size =", size))
  cat("Sample Size = ", size, " Mean = ", mean(pricebar),
      " SD = ", sd(pricebar), "\n")
}
par(mfrow = c(1,1))

## Various sampling methods used on data.
# Original distribution
table(property.df$postalCode)
N <- nrow(property.df)

# Showing proportion with total number of rows
table(property.df$postalCode)/N


# Simple Random Sampling Without Replacement
sample.size <- 100
set.seed(123)
s <- srswor(sample.size, nrow(property.df))
simpleRandomSample <- property.df[s != 0, ]
table(simpleRandomSample$postalCode)
table(simpleRandomSample$postalCode)/N

# Systematic Sampling
set.seed(123)
N <- nrow(property.df)
n <- sample.size

k <- floor(N/n)
r <- sample(k,1)
s <- seq(r, by = k, length = n)
m <- N-((n*k))
m <- (n*k) + sample(m,1)
s <- c(s,m)

cat("k = ",k,"r =",r)
cat("s : ",s)
systematicSample <- property.df[s,]
table(systematicSample$postalCode)
table(systematicSample$postalCode)/N

# Stratified Sampling
set.seed(123)
sample.size <- 100
pik <- inclusionprobabilities(as.integer(property.df$postalCode), sample.size)
length(pik)
sum(pik)
s <- UPsystematic(pik)
stratifiedSample <- property.df[s != 0, ]
table(property.df$postalCode)
table(property.df$postalCode)/N

## Additional Analysis
property.tibble <- as_tibble(property.df)
# Analysis of Prices across Zip Code
property.tibble %>% 
  group_by(postalCode) %>% 
  summarise(
    totProperties=n(),
    avgPrice = mean(priceMax,na.rm = TRUE),
    minPrice = min(priceMax,na.rm = TRUE),
    maxPrice = max(priceMax,na.rm = TRUE))

# Analysis of Apartment Type to Prices per Sq Foot
property.tibble %>% 
  group_by(propertyType) %>% 
  summarise(
    totProperties=n(),
    maxPricePerSqFT = max(pricePerSqaurFoot,na.rm = TRUE,NaN.rm = TRUE))
