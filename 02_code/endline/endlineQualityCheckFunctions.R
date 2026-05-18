# Data quality###
# Function to find columns with suffixed
find_columns_with_suffixes <- function(df) {
  # Define the suffixes to look for
  suffixes <- c("InHa", "InHa_trr", "PerHa", "InKgGreen", "InKgGreen_typ")
  
    # Create a regular expression pattern to match these suffixes
  pattern <- paste0(suffixes, "$", collapse = "|")
  
  # Find and return column names that match the pattern
  matching_columns <- grep(pattern, names(df), value = TRUE)
  return(matching_columns)
}

# Helper function to check if a relevant question was answered
checkIfRelevantThenAnswered <- function(data, condition_question_pairs) {
  # This will store the results
  relevant_unanswered <- list()
  
  # Iterate over each pair and check the corresponding question
  for (pair in condition_question_pairs) {
    # Evaluate the relevant condition to get the indices of applicable rows
    # Adjusting for select_multiple type questions
    condition_expression <- gsub("==", "%in%", pair$condition)
    condition_expression <- gsub("&&", "&", condition_expression)
    condition_expression <- gsub("\\|\\|", "|", condition_expression)
    applicable_indices <- which(eval(parse(text = condition_expression), envir = data))
    
    # Check if the question was answered in these rows
    question_name <- pair$question
    unanswered_indices <- applicable_indices[is.na(data[applicable_indices, question_name])]
    
    # Add the indices of rows where the question was relevant but unanswered
    if (length(unanswered_indices) > 0) {
      relevant_unanswered[[question_name]] <- data[unanswered_indices, ]
    }
  }
  
  return(relevant_unanswered)
}

# Function to check if points are within a given polygon read from a shapefile
check_points_in_polygon <- function(df) {
  library(sf)
  # Read the shapefile
  #polygon_sf <- st_read("C:/Users/feder.DESKTOP-471V2SD/OneDrive - CGIAR/RMI WP1/Baseline survey data/Maps/hnd_admbnda_adm3_sinit_20161005.shp")
  polygon_sf <- st_read("01_data/maps/hnd_admbnda_adm3_sinit_20161005.shp")
  
  #municipalities <- c("La Union", "San Francisco de Ojuera", "San Nicolas", "El Nispero", "Arada", "San Rafael", "Atima", "Santa Rita", "La Iguala")
  municipalities <- c("La Union", "San Francisco de Ojuera", "San Nicolas", "El Nispero", "Arada", "San Rafael", "Atima", "Santa Rita", "La Iguala", "Victoria", "Yoro", "Yorito")
  
  # Subset the shapefile for specified municipalities
  if (!is.null(municipalities)) {
    #polygon_sf <- polygon_sf[polygon_sf$admin1Name %in% c("Lempira", "Santa Barbara") & polygon_sf$admin2Name %in% municipalities, ]
    polygon_sf <- polygon_sf[polygon_sf$admin1Name %in% c("Lempira", "Santa Barbara", "Yoro") & polygon_sf$admin2Name %in% municipalities, ]
  }
  
  # Ensure CRS of points is the same as the polygon
  crs_polygon <- st_crs(polygon_sf)
  
  # Create an sf object from the latitude and longitude columns
  #points_sf <- st_as_sf(df, coords = c("Longitud", "Latitud"), crs = crs_polygon)
  points_sf <- df %>%
       filter(!is.na(Longitud) & !is.na(Latitud)) %>%
       st_as_sf(coords = c("Longitud", "Latitud"), crs = crs_polygon)
  
  # Check if each point falls within the polygon
  points_within_matrix <- st_within(points_sf, polygon_sf, sparse = FALSE)
  
  # Convert the matrix to a logical vector, where TRUE indicates the point is within any polygon
  points_within_vector <- apply(points_within_matrix, 1, function(row) any(row))
  
  return(points_within_vector)
}

# Function to extract invalid respondent names from the "completeName" column
extract_invalid_respondent_names <- function(df) {
  # Check if the "completeName" column exists
  if(!"completeName" %in% names(df)) { 
       stop("Column 'completeName' not found in the dataframe.")
  }
  
  # Function to check if a name is invalid
  is_name_invalid <- function(name) {
    # Check for the presence of at least two words (first name and last name)
    has_less_than_two_words <- length(strsplit(name, " ")[[1]]) < 2
    
    # Check for unwanted characters
    #has_unwanted_chars <- grepl("[.,]", name)
    
    # Name is invalid if it has less than two words or any unwanted characters
    #has_less_than_two_words || has_unwanted_chars
    has_less_than_two_words
  }
  
  # Extract invalid names
  invalid_names <- df$completeName[sapply(df$completeName, is_name_invalid)]
  
  return(invalid_names[!is.na(invalid_names)])
}

# Function to extract invalid buyer names from "buyerName#" columns
extract_invalid_buyer_names <- function(df, buyer) {

     # Function to check if a name is invalid
     is_name_invalid <- function(name) {
          # Check for the presence of at least two words (first name and last name)
          has_less_than_three_char <- nchar(name) < 3
          
          # Check for unwanted characters
          has_unwanted_chars <- grepl("[.,]", name)
          
          # Name is invalid if it has less than two words or any unwanted characters
          #has_less_than_three_char || has_unwanted_chars
          has_less_than_three_char
     }

     invalid_names <- list()
     
     # Iterar sobre las columnas que existen en el dataframe
     for (col in buyer) {
          if (col %in% names(df)) {
               invalid_in_col <- df[[col]][sapply(df[[col]], is_name_invalid)]
               invalid_names[[col]] <- invalid_in_col[!is.na(invalid_in_col)]  # Excluir NAs
          }
     }

     return(invalid_names)
}

# Function to check if values of coffee produced and sold are 
extract_outside_ballpark <- function(df, column1, column2, percent = 10) {
  # Calculate the difference as a percentage
  difference <- abs(df[[column1]] - df[[column2]])
  acceptable_difference <- ((percent / 100) * df[[column1]])
  
  # Identify rows where the difference is greater than the acceptable difference
  outside_ballpark <- difference > acceptable_difference
  
  # Extract rows where values are not within the "ballpark"
  df_outside_ballpark <- df[outside_ballpark, c(column1,column2)]
  
  return(df_outside_ballpark)
}

# Function to extract invalid cell phone numbers and corresponding respondent names
extract_invalid_phone_numbers_with_names <- function(df, phone_number_column, name_column) {
  # Check if the phone number and name columns exist
  if(!phone_number_column %in% names(df)) {
    stop(paste("Column", phone_number_column, "not found in the dataframe."))
  }
  if(!name_column %in% names(df)) {
    stop(paste("Column", name_column, "not found in the dataframe."))
  }
  
  # Function to validate phone number length
  is_phone_number_invalid <- function(phone_number) {
    if(is.na(phone_number) || phone_number == "") {
      return(FALSE) # Ignore NA and empty values
    }
    
    # Ensure the phone number is treated as a string
    phone_number_str <- as.character(phone_number)
    
    # Check if the phone number does not have exactly 8 digits
    return(nchar(gsub("[^0-9]", "", phone_number_str)) != 8)
  }
  
  # Filter for invalid phone numbers
  invalid_indices <- sapply(df[[phone_number_column]], is_phone_number_invalid)
  invalid_phone_numbers <- df[invalid_indices, phone_number_column]
  corresponding_names <- df[invalid_indices, name_column]
  
  # Combine invalid phone numbers with corresponding names
  invalid_data <- data.frame(
    Name = corresponding_names,
    InvalidPhoneNumber = invalid_phone_numbers
  )
  
  return(invalid_data)
}

# Function to check if constraints were complied with
check_all_constraints <- function(df, constraints_list) {
  # Ensure column is numeric
  ensure_numeric <- function(df, column) {
    if(!is.numeric(df[[column]])) {
      df[[column]] <- as.numeric(as.character(df[[column]]))
    }
    return(df)
  }
  
  # Check constraint for each matched column
  check_constraint <- function(df, column_pattern, constraint) {
    # Find columns that match the pattern
    matched_columns <- grep(column_pattern, names(df), value = TRUE)
    violations_list <- list()
    
    for (column in matched_columns) {
      df <- ensure_numeric(df, column)
      # Evaluate the constraint for each row
      #eval(parse(text = paste("violations <- !(", constraint, ")")))
      eval(parse(text = paste("violations <- !is.na(df[[column]]) & !(", constraint, ")")))
      # Store the violations for the specific column
      violations_list[[column]] <- df[violations, column, drop = FALSE]
    }
    
    # Combine violations from all matched columns into a single data frame
    if (length(violations_list) > 0) {
      return(do.call(cbind, violations_list))
    } else {
      return(data.frame())
    }
  }
  
  # Initialize an empty list to store results
  results <- list()
  
  # Iterating over constraints using a for loop
  for(i in seq_along(constraints_list)) {
    pair <- constraints_list[[i]]
    column_to_check <- pair[["column"]]
    # Create regex pattern for column names with suffix
    column_pattern <- paste0("^", column_to_check, "(__[0-9]+)?$")
    constraint <- pair[["constraint"]]
    violations <- check_constraint(df, column_pattern, constraint)
    
    results[[i]] <- list(column = column_to_check, violations = violations)
  }
  
  # Setting names for the results list
  names(results) <- sapply(constraints_list, function(pair) pair[["column"]])
  
  return(results)
}

# Function to validate a dataframe of survey responses (with suffixed columns) and create a frequency table of invalid responses
validate_and_tabulate_invalid_responses_with_suffixes <- function(df, questions_choices) {
  # Initialize a list to hold the frequency tables of invalid responses for each base question
  freq_tables_invalid <- list()
  
  # Helper function to find columns in df that match the base question name
  find_matching_columns <- function(base_question, df_names) {
    grep(paste0("^", base_question, "(__\\d+)?$"), df_names, value = TRUE)
  }
  
  # Iterate over each base question in the questions_choices
  for(base_question in names(questions_choices)) {
    # Find all matching column names in df for this base question
    matching_columns <- find_matching_columns(base_question, names(df))
    
    # Get possible choices for this base question
    possible_choices <- questions_choices[[base_question]]
    
    # Initialize a vector to store invalid responses for all matching columns
    invalid_responses <- character(0)
    
    # Iterate over each matching column
    for(column in matching_columns) {
      # Get responses for this column from the dataframe
      responses <- df[[column]]
      
      # Check if the response is valid and collect invalid responses
      for(response in responses) {
        if(!is.na(response)) {
          if(length(possible_choices) == 1 && !is.list(possible_choices[[1]])) {
            if(!(response %in% possible_choices)) {
              invalid_responses <- c(invalid_responses, response)
            }
          } else { # For multiple choice questions
            split_responses <- unlist(strsplit(as.character(response), ";", fixed = TRUE))
            invalid_items <- split_responses[!split_responses %in% possible_choices]
            invalid_responses <- c(invalid_responses, invalid_items)
          }
        }
      }
    }
    
    # Create frequency table for invalid responses
    if(length(invalid_responses) > 0) {
      freq_table <- table(invalid_responses)
      freq_tables_invalid[[base_question]] <- freq_table
    } else {
      freq_tables_invalid[[base_question]] <- table(NA_integer_)  # No invalid responses found
    }
  }
  
  # Return the list of frequency tables of invalid responses
  return(freq_tables_invalid)
}


# - -----------------------------------------------------------------------

# Main workflow for data validation
checkDataQuality <- function(data) {
  results <- list()
  
  # 1. Check for Duplicates
  results$duplicates <- data[duplicated(data) | duplicated(data, fromLast = TRUE), ]
  
  # 2. Check for Outliers in Key Variables
  key_variables = find_columns_with_suffixes(data)
  print(key_variables)
  results$outliers    <- lapply(key_variables, function(v) {
    if (v %in% names(data)) {
      # Calculate the Interquartile Range (IQR)
      IQR_value <- IQR(data[[v]], na.rm = TRUE)
      
      # Calculate the first and third quartiles
      Q1 <- quantile(data[[v]], 0.25, na.rm = TRUE)
      Q3 <- quantile(data[[v]], 0.75, na.rm = TRUE)
      
      # Adjust the outlier criteria (e.g., using 2 or 3 instead of 1.5)
      lower_bound <- Q1 - 3 * IQR_value
      upper_bound <- Q3 + 3* IQR_value
      
      # Identify and return outliers
      #return(data[[v]][data[[v]] < lower_bound | data[[v]] > upper_bound])
      outliers <- data[[v]][data[[v]] < lower_bound | data[[v]] > upper_bound]
      
      return(outliers[!is.na(outliers)])
    }
  })
  results$lower_bound <- lapply(key_variables, function(v) {
       if (v %in% names(data)) {
            # Calculate the Interquartile Range (IQR)
            IQR_value <- IQR(data[[v]], na.rm = TRUE)
            # Calculate the first and third quartiles
            Q1 <- quantile(data[[v]], 0.25, na.rm = TRUE)
            # Adjust the outlier criteria (e.g., using 2 or 3 instead of 1.5)
            lower_bound <- Q1 - 3 * IQR_value
            return(lower_bound)
       }
  })
  results$upper_bound <- lapply(key_variables, function(v) {
       if (v %in% names(data)) {
            # Calculate the Interquartile Range (IQR)
            IQR_value <- IQR(data[[v]], na.rm = TRUE)
            # Calculate the first and third quartiles
            Q3 <- quantile(data[[v]], 0.75, na.rm = TRUE)
            # Adjust the outlier criteria (e.g., using 2 or 3 instead of 1.5)
            upper_bound <- Q3 + 3 * IQR_value
            return(upper_bound)
       }
  })
  names(results$outliers) <- key_variables
  names(results$lower_bound) <- key_variables
  names(results$upper_bound) <- key_variables
  
  # 3. Check if a relevant question was answered   #!!!!!!!!!!!!!!!!!!!!!!!!!
  {condition_question_pairs <- list(
       list(condition = "triedFormalCredit == 'Y'", question = "accessedFormalCredit"),
       list(condition = "usesWhatsAppForMessages == 'N'", question = "alternativeCommunicationMeans"),
       list(condition = "agreesToSurveyParticipation == 'Y'", question = "authorizesPhotography"),
       list(condition = "numberDifferentBuyersLastHarvest>=1", question = "buyerName1"),
       list(condition = "numberDifferentBuyersLastHarvest>=10", question = "buyerName10"),
       list(condition = "numberDifferentBuyersLastHarvest>=2", question = "buyerName2"),
       list(condition = "numberDifferentBuyersLastHarvest>=3", question = "buyerName3"),
       list(condition = "numberDifferentBuyersLastHarvest>=4", question = "buyerName4"),
       list(condition = "numberDifferentBuyersLastHarvest>=5", question = "buyerName5"),
       list(condition = "numberDifferentBuyersLastHarvest>=6", question = "buyerName6"),
       list(condition = "numberDifferentBuyersLastHarvest>=7", question = "buyerName7"),
       list(condition = "numberDifferentBuyersLastHarvest>=8", question = "buyerName8"),
       list(condition = "numberDifferentBuyersLastHarvest>=9", question = "buyerName9"),
       list(condition = "producedCoffeeLastHarvest2 == 'Y' | produceCoffeeFutureHarvest == 'Y'", question = "canProvideManagementInfo"),
       list(condition = "coffeeProcessingDoneOnFarm == 'secado'", question = "coffeeDryingMethod"),
       list(condition = "same_community == 'N'", question = "communityOrHamlet"),
       list(condition = "isProducerReplacement == 'N' | producerCheck == 'N'", question = "completeName"),
       list(condition = "surveyID == 'other'", question = "completeNameReplacement"),
       list(condition = "adoptedRecommendedTechnologies == 'Y' & !(farmAspectsImproved == 'none')", question = "costOfInformation"),
       list(condition = "creditSources == '11'", question = "creditSourcesOther"),
       list(condition = "agreesToDataUse == 'Y'", question = "dataUseConsentSignature"),
       list(condition = "expressDisagreementToPartner == '1' | expressDisagreementToPartner == '2' | expressDisagreementToPartner == '3' ", question = "decisionMakingAfterDisagreement"),
       list(condition = "!(fertilizersAppliedLastHarvest == 'ninguno')  & fertilizersAppliedLastHarvest != 'NA'", question = "decisionProcessForFertilizers"),
       list(condition = "same_community == 'N'", question = "department"),
       list(condition = "usedCreditInCoffee>0", question = "difficultyPayingDebt"),
       list(condition = "difficultyPayingDebt == '13'", question = "difficultyPayingDebtOther"),
       list(condition = "!(isProducerReplacement == 'Y')", question = "DNI"),
       list(condition = "surveyID == 'other'", question = "DNIReplacement"),
       list(condition = "adoptedRecommendedTechnologies == 'Y'", question = "farmAspectsImproved"),
       list(condition = "assisGroup == 'Y'", question = "feedbackToGroupTraining"),
       list(condition = "assisIndiv == 'Y'", question = "feedbackToTechnicalAssistance"),
       list(condition = "familyWomen>0", question = "femaleElementary"),
       list(condition = "coffeeProcessingDoneOnFarm == 'fermentado'", question = "fermentationWaterTreatment"),
       list(condition = "householdHasMobilePhone == 'Y'", question = "hasSmartphone"),
       list(condition = "isProducerReplacement == 'N' | producerCheck == 'N'", question = "headGender"),
       list(condition = "surveyID == 'other'", question = "headGenderReplacement"),
       list(condition = "surveyID == 'other'", question = "isProducerReplacement"),
       list(condition = "numberOfPeopleInHousehold-familyWomen>0", question = "maleElementary"),
       list(condition = "same_community == 'N'", question = "municipality"),
       list(condition = "gpsWhere == 'other'", question = "otherGpsWhere"),
       list(condition = "typeOfHousingOccupied == 'otra'", question = "otherPropertyType"),
       list(condition = "('reasonNotCoffeeLastHarvest2/5' == '0') & ('reasonNotCoffeeLastHarvest2/8' == '0') & ('reasonNotCoffeeLastHarvest2/9' == '0') & producedCoffeeLastHarvest == 'Y' & producedCoffeeLastHarvest2 == 'N'", question = "produceCoffeeFutureHarvest"),
       list(condition = "isCoffeeProducer == 'Y'", question = "producedCoffeeLastHarvest"),
       list(condition = "isCoffeeProducer == 'Y'", question = "producedCoffeeLastHarvest2"),
       list(condition = "!(surveyID == 'other')", question = "producerCheck"),
       list(condition = "willingToReceiveWhatsAppUpdates == 'Y' & producerCheck == 'N'", question = "providePhoneNumber"),
       list(condition = "reasonForNotUsingGroupTrainingKnowledge == 'hard' | reasonForNotUsingGroupTrainingKnowledge == 'mistrust'", question = "reasonForNotTrustingGroupTrainingInfo"),
       list(condition = "reasonForNotUsingTechnicalAssistanceKnowledge == 'hard' | reasonForNotUsingTechnicalAssistanceKnowledge == 'mistrust'", question = "reasonForNotTrustingTechnicalAssistanceInfo"),
       list(condition = "usedGroupTrainingKnowledgeForCropDecisions == 'N'", question = "reasonForNotUsingGroupTrainingKnowledge"),
       list(condition = "usedTechnicalAssistanceKnowledgeForCropDecisions == 'N'", question = "reasonForNotUsingTechnicalAssistanceKnowledge"),
       list(condition = "womenParticipationInGroupTraining == 'N'", question = "reasonForNoWomenParticipationInGroupTraining"),
       list(condition = "womenParticipationInTechnicalAssistance == 'N'", question = "reasonForNoWomenParticipationInTechnicalAssistanceInfo"),
       list(condition = "producedCoffeeLastHarvest == 'Y' & producedCoffeeLastHarvest2 == 'N'", question = "reasonNotCoffeeLastHarvest2"),
       list(condition = "triedFormalCredit == 'Y' & accessedFormalCredit == 'N'", question = "reasonsDidNotAccessed"),
       list(condition = "reasonsDidNotAccessed == '10'", question = "reasonsDidNotAccessedOther"),
       list(condition = "triedFormalCredit == 'N'", question = "reasonsDidNotTriedCredit"),
       list(condition = "reasonsDidNotTriedCredit == '10'", question = "reasonsDidNotTriedCreditOther"),
       list(condition = "adoptedRecommendedTechnologies == 'Y'", question = "reasonsForExperimenting"),
       list(condition = "canProvideManagementInfo == 'Y'", question = "sellsToIntermediary"),
       list(condition = "otherCropsCultivated == 'other1'", question = "specifyFirstAdditionalCrop"),
       list(condition = "otherCropsCultivated == 'other2'", question = "specifySecondAdditionalCrop"),
       list(condition = "otherCropsCultivated == 'other3'", question = "specifyThirdAdditionalCrop"),
       list(condition = "agreesToSurveyParticipation == 'Y'", question = "surveyConsentSignature"),
       list(condition = "coffeeProcessingDoneOnFarm == 'N'", question = "timeBetweenHarvestAndDelivery"),
       list(condition = "coffeeProcessingDoneOnFarm == 'despulpado'", question = "timeBetweenHarvestAndPulping"),
       list(condition = "coffeeProcessingDoneOnFarm == 'lavado'", question = "timeBetweenPulpingAndWashing"),
       list(condition = "coffeeProcessingDoneOnFarm == 'secado'", question = "timeFromWashingToDryCoffee"),
       list(condition = "accessedCreditOrLoan>0", question = "usedCreditInCoffee"),
       list(condition = "assisGroup == 'Y'", question = "usedGroupTrainingKnowledgeForCropDecisions"),
       list(condition = "assisIndiv == 'Y'", question = "usedTechnicalAssistanceKnowledgeForCropDecisions"),
       list(condition = "hasSmartphone == 'Y'", question = "usesWhatsAppForMessages"),
       list(condition = "isProducerReplacement == 'N' | producerCheck == 'N'", question = "vinc"),
       list(condition = "identifyIndigenous == 'Y'", question = "whichIndigenous"),
       list(condition = "usesWhatsAppForMessages == 'Y'", question = "willingToReceiveWhatsAppUpdates"),
       list(condition = "assisGroup == 'Y' & familyWomen>'0'", question = "womenParticipationInGroupTraining"),
       list(condition = "assisIndiv == 'Y' & familyWomen>'0'", question = "womenParticipationInTechnicalAssistance"),
       list(condition = "decisionProcessForFertilizers == 'soil_test'", question = "yearOfLastSoilAnalysis")
  )  }
  results$relevant_unanswered <- checkIfRelevantThenAnswered(data, condition_question_pairs)
  
  # 4.Check if constraint was respected 
  {  constraints_list <- list(
       list(column = 'accessedCreditOrLoan', constraint = 'df$accessedCreditOrLoan>=0 & df$accessedCreditOrLoan<=30'),
       list(column = 'commMigrate', constraint = 'df$commMigrate<=df$commSize'),
       list(column = 'familyWomen', constraint = 'df$familyWomen<=df$numberOfPeopleInHousehold'),
       list(column = 'numberDifferentBuyersLastHarvest', constraint = 'df$numberDifferentBuyersLastHarvest>=0'),
       list(column = 'numberOfLands', constraint = 'df$numberOfLands>0'),
       list(column = 'numberOfPeopleInHousehold', constraint = 'df$numberOfPeopleInHousehold>0 & df$numberOfPeopleInHousehold<15'),
       list(column = 'numberProductivePlants', constraint = 'df$numberProductivePlants>=0 & df$numberProductivePlants<=df$totalCoffeePlantsOnFarm'),
       list(column = 'spacingBetweenPlants', constraint = 'df$spacingBetweenPlants>0'),
       list(column = 'spacingBetweenRows', constraint = 'df$spacingBetweenRows>0'),
       list(column = 'timeBetweenHarvestAndDelivery', constraint = 'df$timeBetweenHarvestAndDelivery>0'),
       list(column = 'timeBetweenHarvestAndPulping', constraint = 'df$timeBetweenHarvestAndPulping>0'),
       list(column = 'timeBetweenPulpingAndWashing', constraint = 'df$timeBetweenPulpingAndWashing>0'),
       list(column = 'timeFromWashingToDryCoffee', constraint = 'df$timeFromWashingToDryCoffee>0'),
       list(column = 'totalCoffeePlantsOnFarm', constraint = 'df$totalCoffeePlantsOnFarm>0'),
       list(column = 'usedCreditInCoffee', constraint = 'df$usedCreditInCoffee>=0 &  df$usedCreditInCoffee<=df$accessedCreditOrLoan'),
       list(column = 'yearOfLastSoilAnalysis', constraint = 'df$yearOfLastSoilAnalysis>=1980 & df$yearOfLastSoilAnalysis<=2024'),
       list(column = 'yearsExperienceInCoffeeFarming', constraint = 'df$yearsExperienceInCoffeeFarming<df$respondentAge')
  )}
  results$constraintComply <- check_all_constraints(data,constraints_list)
  
  # 5. Check if the responses to select_one and select_multiple are valid
  #load("choiceMapping.RData")
  #choiceMapping <- remove_accents_from_list(choiceMapping, removeSpanishAccents)
  #results$correctChoice <- validate_and_tabulate_invalid_responses_with_suffixes(df,choiceMapping)
  
  # 6. Check that names are correct
  results$correctName  <- extract_invalid_respondent_names(df)
  names_variables <- c(paste0("buyerName", 1:10))
  results$correctBuyer <- extract_invalid_buyer_names(df, names_variables)
  
  # 7. Check that phone numbers have 8 digits
  results$validPhone <- extract_invalid_phone_numbers_with_names(df,"providePhoneNumber","nameCheck")
  
  # # 8. Check if surveys are in area of interest
  library(sf)
  results$validLocation <- check_points_in_polygon(df)
  false_indices <- which(results[["validLocation"]] == FALSE)
  print(false_indices)
  
  # 9. Check if coffee produced and coffee sold are in the ballpark of each other
  #results$ballpark <- extract_outside_ballpark(df,"amountOfCoffeeProducedLastHarvestInGreenKgs","amountSoldInKgGreen", percent=25)

  return(results)
}
