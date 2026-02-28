# Path --------------------------------------------------------------------
#setwd("D:/OneDrive - CGIAR/RMI WP1/Armonizacion")
#setwd("C:/Users/feder.DESKTOP-471V2SD/OneDrive - CGIAR/RMI WP1/Armonizacion")
 setwd("C:/Users/rszap/OneDrive - Universidad EAFIT/CIAT/RFM WP1/Armonizacion/")
 rm(list=ls())

# Dictionary --------------------------------------------------------------
{prefixMapping <- c(
  "A2" = "surveyDate",
  "A4" = "department",
  "A5" = "municipality",
  "A6" = "communityOrHamlet",
  "prod" = "assignedPersonName",
  "prod2" = "otherProducerName",
  "id_encuesta" = "surveyID",
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
  "E4C" = "totalAreaUsedForAgricultureInHa",
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
  "E49" = "harvestSeasonStartMonth_aux",
  "E50" = "harvestSeasonEndMonth_aux",
  "E51" = "conductedPreHarvestSampling",
  "E28" = "fertilizersAppliedLastHarvest",
  "E30" = "decisionProcessForFertilizers",
  "E31" = "yearOfLastSoilAnalysis",
  "E32" = "weedControlMethodUse",
  "E33" = "performsSelectiveClearing",
  "E34" = "pestAndDiseaseMonitoringImplemented",
  "E35" = "mainPestsOrDiseasesLastYear",
  "E35A" = "otherPestsOrDiseases",
  "E36C" = "nameOfPest",
  "E37" = "pestControlActions",
  "E43" = "totalCoffeePlantsOnFarm",
  "E43A" = "spacingBetweenRowsInMt",
  "E43B" = "spacingBetweenPlantsInMt",
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
  "E69" = "amountSold",
  "E69CA" =	"amountSoldInKgGreen_aux",
  "E69C" = "saleAmountInUnitsInGreenKgsByBuyer",
  "E70CA" = "quintalConversionRate",
  "E70CB" = "saleAmountInUnitsInGreenKgsByType",
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
  "E83" = "reasonsForSellingCoffee_aux",
  "E83A" = "otherReasonsForSellingCoffee_aux",
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
  "I22" = "genderContributionToCoffeeHarvest",
  "start_time" = "timeStart",
  "end_time" = "timeEnd"
)}


# Functions ---------------------------------------------------------------
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


# Baseline merge ----------------------------------------------------------

# Define the path to the folder containing the baseline files
baseline_folder <- "baseline/New folder"

# List all Excel files in the folder
baseline_files <- list.files(path = baseline_folder, pattern = "^RM2023CAFE.*\\.xlsx$", full.names = TRUE)

# Get a list of unique sheet names (tabs) from all the Excel files
sheet_names <- unique(unlist(lapply(baseline_files, excel_sheets)))
sheet_names <- setdiff(sheet_names, "repeatcla")
#Note: there is a sheet named "repeatcla" in version 4. There is only one observation,
#      therefore is was deleted. PENDING: verify.

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

# Change some variables values
final_merged_data[["sales_process_repeat"]] <- final_merged_data[["sales_process_repeat"]] %>%
  mutate(methodOfCoffeeSale = case_when(
    methodOfCoffeeSale == "Pergamino mojado" ~ "soaking_wet_parchment",
    methodOfCoffeeSale == "Pergamino seco"   ~ "dry_parchment",
    methodOfCoffeeSale == "Oro"              ~ "green",
    methodOfCoffeeSale == "Uva"              ~ "cherry",
    methodOfCoffeeSale == "Tostado"          ~ "roasted",
    TRUE ~ methodOfCoffeeSale  # Keeps other values unchanged
  )) %>%
  mutate(reasonsForSellingCoffee_aux = case_when(
    reasonsForSellingCoffee_aux == 2  ~ "inmediate_income",
    reasonsForSellingCoffee_aux == 4  ~ "limited_market_access",
    reasonsForSellingCoffee_aux == 1  ~ "ofered_price",
    reasonsForSellingCoffee_aux == 99 ~ "other",
    reasonsForSellingCoffee_aux == 5  ~ "personal_preference",
    reasonsForSellingCoffee_aux == 3  ~ "previous_relation",
    TRUE ~ reasonsForSellingCoffee_aux
  ))

final_merged_data[["mod_combined"]] <- final_merged_data[["mod_combined"]] %>%
  mutate(typeOfSupportFromIntermediary = case_when(
    typeOfSupportFromIntermediary == 1  ~ "technical_assistance",
    typeOfSupportFromIntermediary == 2  ~ "inputs",
    typeOfSupportFromIntermediary == 3  ~ "personal_loans",
    typeOfSupportFromIntermediary == 4  ~ "coffee_loans",
    typeOfSupportFromIntermediary == 5  ~ "none",
    typeOfSupportFromIntermediary == 99 ~ "no_answer",
    TRUE ~ typeOfSupportFromIntermediary  # Keeps other values unchanged
  )) %>%
  mutate(sellsToIntermediary = case_when(
    sellsToIntermediary == 1  ~ "dario",
    sellsToIntermediary == 2  ~ "ramiro",
    sellsToIntermediary == 3  ~ "becamo",
    sellsToIntermediary == 4  ~ "ninguno",
    TRUE ~ sellsToIntermediary  # Keeps other values unchanged
  )) %>%
  mutate(plannedIntermediaryNextHarvest = case_when(
    plannedIntermediaryNextHarvest == 1  ~ "dario",
    plannedIntermediaryNextHarvest == 2  ~ "ramiro",
    plannedIntermediaryNextHarvest == 3  ~ "becamo",
    plannedIntermediaryNextHarvest == 4  ~ "ninguno",
    TRUE ~ plannedIntermediaryNextHarvest  # Keeps other values unchanged
  )) %>%
  mutate(worksFrequentlyAwayFromHome = case_when(
    worksFrequentlyAwayFromHome == 1  ~ "no",
    worksFrequentlyAwayFromHome == 2  ~ "yes_me",
    worksFrequentlyAwayFromHome == 3  ~ "yes_spouse",
    worksFrequentlyAwayFromHome == 4  ~ "yes_both",
    TRUE ~ worksFrequentlyAwayFromHome  # Keeps other values unchanged
  ))

final_merged_data[["tech_repeat"]] <- final_merged_data[["tech_repeat"]] %>%
  mutate(aspectImproved = case_when(
    aspectImproved == "Fertilidad del suelo y nutrición de las plantas" ~ "fertility",
    aspectImproved == "Ninguno" ~ "none",
    aspectImproved == "Gestión de la sombra de los árboles" ~ "shade",
    aspectImproved == "Rehabilitación y renovación" ~ "rnr",
    aspectImproved == "Gestión integrada de plagas y enfermedades" ~ "nursery",
    aspectImproved == "Semilleros y viveros" ~ "ipm",
    aspectImproved == "Control de calidad y recolección selectiva" ~ "quality",
    aspectImproved == "Gestión de tejidos" ~ "tissue",
    aspectImproved == "Establecimiento de plantaciones" ~ "establishment",
    aspectImproved == "Conservación del suelo y gestión de las malas hierbas" ~ "conservation",
    aspectImproved == "Mantenimiento de registros y planes de inversión" ~ "records",
    TRUE ~ aspectImproved  # Keeps other values unchanged
  ))

# new_names_mapping <- c(
#   "dario" = 1,
#   "ramiro" = 2,
#   "becamo" = 3,
#   "ninguno" = 4
# )
# 
# # Use mutate to replace numerical values with corresponding text
# final_merged_data[["mod_combined"]] <- final_merged_data[["mod_combined"]] %>%
#   mutate(typeOfSupportFromIntermediary = factor(typeOfSupportFromIntermediary,
#                                                 levels = new_names_mapping,
#                                                 labels = names(new_names_mapping)))

# Save all tabs to a single Excel file, each as a separate sheet
write_xlsx(final_merged_data, "Baseline/baselineRaw.xlsx")

