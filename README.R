#' ---------------------------------
#'  Readme – RFMcoffeeHonduras
#'  Last updated: May, 2026
#' ---------------------------------

#'  This folder contains the code for cleaning and performing exploratory data analysis for the Rethinking Food Markets CGIAR initiative regarding the coffee supply chain in Honduras. The results were obtained using R 4.5.1, and the necessary libraries can be downloaded from The Comprehensive R Archive Network (CRAN).
#'  The folder is organized as follows:
#'
#'  “01_data": includes the raw and processed databases from the baseline and endline surveys, as well as the map files for the cleaning and exploratory data analysis process:
#'    1.1. “maps”: contains shapefiles for Honduras at the departmental, municipal, and village levels, updated as of October 2016.
#'    1.2. “raw”: contains original databases from the baseline and endline surveys, as well as the treatment assignment and implementation data. The three main files are:
#'      Baseline survey: ./raw/baseline/baselineRaw.xlsx
#'      Endline survey: ./raw/endline/endlineRaw.xlsx
#'      Treatment group assignment: ./implementation/ttmAssign_labels.xlsx
#'    1.3. “processed”: contains the intermediate and final databases; that is, data that has undergone some level of processing. The four main files are:
#'      Baseline aggregated: ./processed/baseline/baselineAgg.csv
#'      Endline aggregated: ./processed/endline/endlineAgg.csv
#'      Long-form surveys joined: ./processed/harmonization/longDF.csv
#'      Wide-form surveys joined: ./processed/harmonization/wideDF.csv
#'  “02_code”: includes the files used for cleaning, merging, quality checks, and exploratory data analysis of the baseline and endline surveys:
#'    2.1. “dictionary.xlsx”: This is the main file for data merging and analysis. It is dynamic and contains a list of all variables, including the survey to which they belong, the variable type, the level of aggregation within the survey and the operations used to aggregate them, as well as their use in regression models and the generation of graphs.
#'    2.2. “baseline”: contains the code used for merging, cleaning checking the quality of the baseline survey data. The main file is “baselineProcessingCode.R”, which calls the other scripts to perform each task.
#'    2.3. “endline”: contains the code used for merging, cleaning checking the quality of the endline survey data. The main file is “endlineProcessingCode.R,” which calls the other scripts to perform each task.
#'    2.4. “endline”: contains the code used to merge the initial and final databases, as well as to generate graphs and calculate regression models. The main file is “mainCode.R,” which calls the other scripts to perform each task.
#'  “03_tables”: includes the results of the quality checks and the regression models that were performed. It contains folders for “baseline,” “endline,” and “harmonization,” depending on the outcome.
#'  “04_plots”: includes the results plots and graphs that were constructed. It contains folders for “baseline,” “endline,” and “harmonization,” depending on the outcome.
#'
#'  The process for executing the codes is as follows:
#'    1. Update the “./02_code/dictionary.xlsx” with the desired aggregation of variables. At this point, updates can also be made to the graphs and regressions needed for step 5
#'    2. Open the “./RFMcoffeeHonduras.Rproj” and use it to open the scripts below. This allows for the use of standardized paths.
#'    3. If a change is needed to the baseline processing, run the “./02_code/baseline/baselineProcessingCode.R”
#'    4. If a change is needed to the endline processing, run the “./02_code/endline/endlineProcessingCode.R”
#'    5. If any changes were made to steps 3 and 4, the entire “./02_code/harmonization/mainCode.R” script must be run. If only graphs or regressions need to be added, the last two sections of this file - “Graphs (longDF)” and “Regression models (wideDF)” - can be run after updating the dictionary.
