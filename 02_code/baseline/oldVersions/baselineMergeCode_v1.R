# ----------------------------------------------------------------------- #
# Path --------------------------------------------------------------------
# ----------------------------------------------------------------------- #

#setwd("C:/Users/feder.DESKTOP-471V2SD/OneDrive - CGIAR/RMI WP1/Armonizacion")
 setwd("C:/Users/rszap/OneDrive - Universidad EAFIT/CIAT/RFM WP1/Armonizacion/")


# ----------------------------------------------------------------------- #
# Dictionary --------------------------------------------------------------
# ----------------------------------------------------------------------- #

{prefixMapping <- c(
  "A2" = "surveyDate",
  "A4" = "department",
  "A5" = "municipality",
  "A6" = "communityOrHamlet",
  "prod" = "assignedPersonName",
  "prod2" = "otherProducerName",
  "C1" = "respondentGender",
  "B1" = "isCoffeeProducer",
  "BX" = "producedCoffeeLastHarvest",
  "C2" = "canProvideManagementInfo",
  "A3" = "sellsToIntermediary",
  "CR3" = "plannedIntermediaryNextHarvest",
  "A9" = "agreesToSurveyParticipation",
  "A10" = "surveyConsentSignature",
  "A11" = "authorizesPhotography",
  "lista" = "confirmPersonFromList",
  "A8" = "completeName",
  "vinc" = "relationshipWithProducer",
  "D1" = "householdHasMobilePhone",
  "D7" = "providePhoneNumber",
  "D2" = "hasSmartphone",
  "D3" = "hasInternetAtHome",
  "D4" = "usesWhatsAppForMessages",
  "D6" = "willingToReceiveWhatsAppUpdates",
  "D5" = "alternativeCommunicationMeans",
  "C3" = "isMarriedOrInPartnership",
  "C4" = "highestEducationLevel",
  "C5" = "worksFrequentlyAwayFromHome",
  "C6" = "numberOfPeopleInHousehold",
  "C7" = "respondentGender",
  "C8" = "respondentAge",
  "C9" = "hasMigratedInThePastYear",
  "C10" = "typeOfHousingOccupied",
  "C11" = "otherPropertyType",
  "C12" = "householdHasPipedWater",
  "C13" = "householdConnectedToElectricity",
  "B2" = "yearsExperienceInCoffeeFarming",
  "B3" = "typeOfSupportFromIntermediary",
  "BX1" = "durationOfWorkingWithIntermediary",
  "E2" = "totalAreaUsedForAgriculture",
  "E3" = "measurementUnitOfArea",
  "E4" = "otherAreaMeasurementUnit",
  "E4C" = "totalAreaUsedForAgricultureInHectares",
  "E2X" = "coffeeAreaLastHarvest",
  "E2XA" = "coffeeAreaMeasurementUnit",
  "E3X" = "productiveCoffeeAreaLastHarvest",
  "E3XA" = "productiveCoffeeAreaUnit",
  "E1" = "landLegalStatusForCultivation",
  "E5" = "totalParcelArea",
  "E6" = "parcelAreaMeasurementUnit",
  "E8" = "familyMemberWhoOwnsLand",
  "E14" = "whoWorkedOnLandLastHarvest",
  "E15" = "farmTerrainCharacteristics",
  "E16" = "otherCropsCultivated",
  "E17" = "specifyFirstAdditionalCrop",
  "E18" = "specifySecondAdditionalCrop",
  "E19" = "specifyThirdAdditionalCrop",
  "E21" = "lastCoffeeHarvestOutcome",
  "E22" = "amountOfCoffeeProducedLastHarvest",
  "E23" = "coffeeProductionMeasurementUnit",
  "E24" = "typeOfCoffeeProduced",
  "E24CA" =	"amountOfCoffeeProducedLastHarvestBase",
  "E49" = "harvestSeasonStartMonth",
  "E50" = "harvestSeasonEndMonth",
  "E51" = "conductedPreHarvestSampling",
  "E28" = "fertilizersAppliedLastHarvest",
  "E30" = "decisionProcessForFertilizers",
  "E31" = "yearOfLastSoilAnalysis",
  "E32" = "weedControlMethodUsed",
  "E33" = "performsSelectiveClearing",
  "E34" = "pestAndDiseaseMonitoringImplemented",
  "E35" = "mainPestsOrDiseasesLastYear",
  "E35A" = "otherPestsOrDiseases",
  "E36C" = "nameOfPest",
  "E37" = "pestAndDiseasePreventionMeasures",
  "E43" = "totalCoffeePlantsOnFarm",
  "E43A" = "spacingBetweenRows",
  "E43B" = "spacingBetweenPlants",
  "E44" = "numberProductivePlants",
  "E47" = "numberPlantsPruned",
  "E45" = "sourceOfNewSeeds",
  "E46" = "otherSeedSourceSpecified",
  "E45A" = "acquiredSeedlings",
  "E45B" = "sourceOfSeedlings",
  "E48" = "criteriaForCoffeeRenewal",
  "E38" = "varietiesPlanted",
  "VARR" = "varietyName",
  "E39" = "percentageOfVarietyPlanted",
  "E52" = "harvestAndPostHarvestPractices",
  "E53" = "mainHarvestCoffeeClassification",
  "E53A" = "coffeeClassificationBeforeSale",
  "E56" = "coffeeProcessingDoneOnFarm",
  "E57" = "timeBetweenHarvestAndDelivery",
  "E58" = "timeBetweenHarvestAndPulping",
  "E59" = "fermentationWaterTreatment",
  "E60" = "timeBetweenPulpingAndWashing",
  "E61" = "timeFromWashingToDryCoffee",
  "E62" = "coffeeDryingMethod",
  "E63" = "numberDifferentBuyersLastHarvest",
  "E63C" = "buyerNumber",
  "NC" = "buyerName",
  "E64" = "buyerType",
  "E64C" = "buyerCategory",
  "E64X" = "amountCoffeeSold",
  "E65" = "locationOfCoffeeSale",
  "E66" = "mainReasonForSellingCoffee",
  "E67" = "formInWhichCoffeeWasSold",
  "E67C" = "numberOfCoffeeSales",
  "E68C" = "methodOfCoffeeSale",
  "E69CA" =	"amountSoldInKgGreen",
  "E70CA" = "quintalConversionRate",
  "E69" = "saleAmountInUnits",
  "E70" = "salesMeasurementUnit",
  "E71" = "minimumReceivedPriceForCoffee",
  "E71A" = "priceUnitForMinimumPrice",
  "E72" = "maximumReceivedPriceForCoffee",
  "E72A" = "priceUnitForMaximumPrice",
  "E75" = "soldCoffeeByVolumeOrWeight",
  "E76" = "incomeVariationBySellingWeight",
  "E77" = "qualityEvaluationDone",
  "E77A" = "specificQualityEvaluationDone",
  "E78" = "receivedQualityBonus",
  "E78A" = "receivedQualityDiscount",
  "E79" = "incomeVariationByQuality",
  "E80" = "receivedCertificationBonus",
  "E81" = "typeOfCertificationBonus",
  "E82" = "otherTypeOfCertification",
  "E83" = "reasonsForSellingCoffee",
  "E83A" = "otherReasonsForSellingCoffee",
  "E84" = "necessityToSellCoffee",
  "F1" = "adoptedRecommendedTechnologies",
  "F2" = "sourceOfTechnicalAssistance",
  "F3" = "methodOfContactingTechnicians",
  "F4" = "farmAspectsImproved",
  "F5" = "aspectImproved",
  "F5A" = "aspectImprovedId",
  "F6" = "aspectsExperimentedWith",
  "F7" = "reasonsForExperimenting",
  "F8" = "otherReasonsForExperimenting",
  "F9" = "barriersToExperimentation",
  "F10" = "otherBarriersToExperimentation",
  "F11" = "changeInPracticeFrequency",
  "F12" = "sourceOfPracticeInformation",
  "F13" = "costOfInformation",
  "F14" = "helperInImplementingPractices",
  "F15" = "futurePracticesToImprove",
  "G1" = "registeredWithIHCAFE",
  "G2" = "householdMemberInAssociation",
  "G3" = "typeOfGroupOrAssociation",
  "G4" = "affiliationWithCoffeeOrganization",
  "G5" = "specificCoffeeOrganization",
  "G6" = "attitudeTowardsInnovation",
  "G7" = "spendingExtraIncomePreference",
  "G8" = "desireForChildrenToFarm",
  "G9" = "childrensInterestInFarming",
  "G10" = "overallLifeSatisfaction",
  "H1" = "beliefInSoilImprovement",
  "H2" = "beliefInWaterAvailability",
  "H3" = "beliefInWaterQuality",
  "H4" = "beliefInClimateForAgriculture",
  "H5" = "beliefInWildlifePopulation",
  "H6" = "beliefInForestResourceQuantity",
  "I1" = "seasonsWithLessFoodAvailability",
  "I2" = "monthsOfLessFoodAvailability",
  "I3" = "worstMonthForFoodAvailability",
  "I4" = "bestMonthForFoodAvailability",
  "J1" = "receivedGovernmentAid",
  "J2" = "typeOfAidReceived",
  "J3" = "specifyTypeOfAid",
  "J4" = "receivedCommunityHelp",
  "J5" = "typeOfCommunityGifts",
  "J6" = "specifyTypeOfGift",
  "J7" = "accessedCreditOrLoan",
  "J8" = "difficultyPayingDebt",
  "K1" = "otherSourcesOfIncome",
  "K2" = "proportionOfOtherIncome",
  "K3" = "proportionOfCoffeeSaleIncome",
  "K4" = "decisionMakerForNonFarmIncome",
  "K6" = "householdEconomicSituation",
  "K13" = "householdItemsPossessed",
  "K14" = "numberOfHouseholdPhones",
  "K15" = "numberOfHouseholdBicycles",
  "K16" = "numberOfHouseholdMotorcycles",
  "K17" = "numberOfHouseholdCars",
  "K18" = "numberOfHouseholdTractors",
  "K19" = "numberOfHouseholdBrushCutters",
  "K20" = "numberOfHouseholdRadios",
  "K21" = "numberOfHouseholdTVs",
  "K22" = "numberOfHouseholdSatelliteDishes",
  "K23" = "numberOfHouseholdComputers",
  "K24" = "numberOfHouseholdFridges",
  "K25" = "numberOfHouseholdGrainMills",
  "K26" = "lastMaintenanceOfPulper",
  "E85" = "decisionMakerForCoffeeIncome",
  "E20" = "cropTypeDecisionMaker",
  "L8" = "householdDecisionMakingAbility",
  "L14" = "controlOverHouseholdGoods",
  "L9" = "incomeSpendingAutonomy",
  "L10" = "marketParticipationDecision",
  "L2" = "influenceOnFamilySavingsUsage",
  "L1" = "influenceOnConsumptionCropChoice",
  "L3" = "influenceOnSalesCropChoice",
  "L12" = "controlOverCultivationLand",
  "L13" = "controlOverLandAssets",
  "L7" = "autonomyToAttendTraining",
  "L6" = "participationInCommunityActivitiesLevel",
  "L11" = "choiceToJoinCommunityCommittees",
  "L17" = "communityLeadershipRolesHeld",
  "L19" = "personPreparingLand",
  "L20" = "genderContributionToLandPreparation",
  "L21" = "personHarvestingCoffee",
  "I22" = "genderContributionToCoffeeHarvest"
)}



# ----------------------------------------------------------------------- #
# Functions ---------------------------------------------------------------
# ----------------------------------------------------------------------- #

library(dplyr)
library(readxl)
library(writexl)
library(purrr)

# Replace column names with interpretable identifiers
replaceColumnPrefixes <- function(dataframe, prefixMapping) {
  # Iterate over each prefix mapping
  for (oldPrefix in names(prefixMapping)) {
    # Construct a pattern to find columns that are exactly the old prefix or
    # start with the old prefix followed by an underscore and any characters
    pattern <- paste0("^", oldPrefix, "(_|$)")
    
    # Find column names matching the pattern
    colsToChange <- grep(pattern, colnames(dataframe), value = TRUE)
    
    # Replace old prefix with new prefix in these column names
    newColNames <- sub(oldPrefix, prefixMapping[[oldPrefix]], colsToChange)
    
    # Assign the new column names back to the dataframe
    names(dataframe)[names(dataframe) %in% colsToChange] <- newColNames
  }
  
  return(dataframe)
}

# Function to merge two data frames and identify non-matching columns
merge_tabs <- function(df1, df2) {
  # Perform a full join to ensure all columns from both data frames are included
  merged_df <- full_join(df1, df2, by = intersect(names(df1), names(df2)))
  
  # Identify columns that didn't merge
  non_matching_columns <- setdiff(union(names(df1), names(df2)), names(merged_df))
  
  list(
    merged_df = merged_df,
    non_matching_columns = non_matching_columns
  )
}

# Function to capitalize all column names in a dataframe
capitalize_column_names <- function(df) {
  names(df) <- toupper(names(df))
  return(df)
}

# Function to process each tab
process_tabs <- function(tab_name, baseline_files) {
  # Read all corresponding tabs from different versions
  tabs <- lapply(baseline_files, function(file) {
    df <- read_excel(file, sheet = tab_name)
    df <- capitalize_column_names(df)
    return(df)
  })
  
  # Merge all tabs pairwise
  merged_data <- tabs[[1]]
  non_matching_columns_all <- list()
  
  for (i in 2:length(tabs)) {
    result <- merge_tabs(merged_data, tabs[[i]])
    merged_data <- result$merged_df
    non_matching_columns_all[[i]] <- result$non_matching_columns
  }
  
  # Return merged data and non-matching columns
  list(
    merged_data = merged_data,
    non_matching_columns = non_matching_columns_all
  )
}


# ----------------------------------------------------------------------- #
# Baseline merge ----------------------------------------------------------
# ----------------------------------------------------------------------- #

# Define the path to the folder containing the baseline files
baseline_folder <- "baseline"

# List all Excel files in the folder
baseline_files <- list.files(path = baseline_folder, pattern = "^RM2023CAFE.*\\.xlsx$", full.names = TRUE)

# Get a list of unique sheet names (tabs) from all the Excel files
sheet_names <- unique(unlist(lapply(baseline_files, excel_sheets)))

# Initialize lists to store results
final_merged_data <- list()
non_matching_columns_report <- list()
mod_tabs_data <- list()

# Process each tab across all files
for (sheet_name in sheet_names) {
  result <- process_tabs(sheet_name, baseline_files)
  
  # Replace column names with meaningful names
  result$merged_data <- replaceColumnPrefixes(result$merged_data, prefixMapping)
  
  # Check if the tab name starts with "mod"
  if (startsWith(sheet_name, "mod") || sheet_name == "maintable") {
    mod_tabs_data[[sheet_name]] <- result$merged_data
  } else {
    final_merged_data[[sheet_name]] <- result$merged_data
  }
}

# Merge all "mod" tabs and "main table" by rowuuid
if (length(mod_tabs_data) > 0) {
  mod_merged_data <- reduce(mod_tabs_data, full_join, by = "ID_ENCUESTA")
  final_merged_data[["mod_combined"]] <- mod_merged_data
}

# Save all tabs to a single Excel file, each as a separate sheet
write_xlsx(final_merged_data, "Baseline/baselineRaw.xlsx")

