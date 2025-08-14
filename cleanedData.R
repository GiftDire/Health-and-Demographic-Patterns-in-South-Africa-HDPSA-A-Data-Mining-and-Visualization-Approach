library(tidyverse)
library(dplyr)
library(mice)

mortalitydata<- read.csv("child-mortality-rates_national_zaf.csv")
print(mortalitydata)

view(mortalitydata)

glimpse(mortalitydata) 


#----------------------Finding the metadata ------------------
 metadata_identified <-sapply(mortalitydata, function(col)any(grepl("#",col , fixed = TRUE)))
 if (any(metadata_identified)) {
   warning("Metadata found in columns :",
           paste(names(mortalitydata)[metadata_identified,collapse=","]))
 }
print(metadata_identified)
#----------------------Removing metadata -------------------

mortalitydata <- as.data.frame(lapply(mortalitydata, function(col) {
  if(is.character(col) || is.factor(col)) {
    col <- gsub("#.*", "", col)  
  }
  col  
}), stringsAsFactors = FALSE )

print(mortalitydata)

mortalitydata <- mortalitydata[ rowSums(mortalitydata!=""& !is.na(mortalitydata)) >0,] # Removes the empty row
view(mortalitydata)

#----------------------- Removing unwanted/ empty columns--------------------

mortalitydata <- mortalitydata %>% select(-DataId,-Precision,-DHS_CountryCode,-SurveyId,-SurveyYearLabel,-SurveyType,-IndicatorId,-IndicatorOrder,-IndicatorType,
                                           -CharacteristicId,-CharacteristicOrder,-CharacteristicCategory,-CharacteristicLabel,-IsTotal,-IsPreferred,-SDRID,-RegionId,
                                          -DenominatorUnweighted,-DenominatorWeighted,-LevelRank,-ByVariableId,-ByVariableLabel)
view(mortalitydata)

#-----------------------------------Renaming the columns--------------------------

mortalitydata <- mortalitydata %>% rename(

  CountryCode = ISO3,
  MortalityIndicator = Indicator,
  MortalityRate = Value,
  Year = SurveyYear,
  Confidence_Indicator_High = CIHigh,
  Confidence_Indicator_Low = CILow
)

#------------------------------------ changes the datatype ---------------------------------

mortalitydata <- mortalitydata %>% 
  mutate(
    MortalityRate = as.numeric(MortalityRate),
    Year = as.integer(Year),
    Confidence_Indicator_Low = as.numeric(Confidence_Indicator_Low),
    Confidence_Indicator_High = as.numeric(Confidence_Indicator_High)
  )
str(mortalitydata)

#--------------------------------Handling missing values (Data Imputation) in CIH AND CIL------------------------

md.pattern(mortalitydata)

summary(mortalitydata)

colSums(is.na(mortalitydata[, c("Confidence_Indicator_High", "Confidence_Indicator_Low")]))

mortalitydata$Confidence_Indicator_High[is.na(mortalitydata$Confidence_Indicator_High)] <- 
  median(mortalitydata$Confidence_Indicator_High, na.rm = TRUE)

mortalitydata$Confidence_Indicator_Low[is.na(mortalitydata$Confidence_Indicator_Low)] <- 
  median(mortalitydata$Confidence_Indicator_Low, na.rm = TRUE)


colSums(is.na(mortalitydata[, c("Confidence_Indicator_High", "Confidence_Indicator_Low")]))

