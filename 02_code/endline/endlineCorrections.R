#' This script cleans multiple variables in "01_data/raw/endline/endlineRaw.csv", 
#' using the "endlineCorrections.xlsx" Excel.


#' ========================================================================
#  Main sheet -------------------------------------------------------------
#' ========================================================================

#' ------------------------------------------------------------------------
## Duplicates and IDs -----------------------------------------------------
#' Correction of duplicates and valid observations is based on 
#' "01_data/raw/endline/Entregable [2] 2024-09-05 Duplicados WP1 Café"

# Reading Excel
changes_df  <- read_excel(changes_path, sheet = "observations")

# Deleting observations
df_delete <- base_df %>%
  anti_join(
    changes_df %>% filter(delete == 1),
    by = c("surveyDate", "enc", "_uuid")
  )
changes_df <- changes_df %>% filter(is.na(delete))


# Changing values

## Variables to change
vars_corregidas <- names(changes_df) %>%
  str_subset("_corregido$") %>%
  str_remove("_corregido")

## New df
df_updated <- df_delete

## Loop by var
for (var in vars_corregidas) {
  ### Name
  var_cor <- paste0(var, "_corregido")
  ### Map for observations with changes
  map_var <- changes_df %>%
    filter(!is.na(.data[[var_cor]])) %>%
    select("surveyDate", "enc", "_uuid", !!sym(var), !!sym(var_cor)) %>%
    distinct()
  ### Changing df
  df_updated <- df_updated %>%
    left_join(map_var,
              by = c("surveyDate", "enc", "_uuid"), 
              suffix = c("", "_mapvars")) %>%
    mutate(
      !!sym(var) := ifelse(!is.na(.data[[var_cor]]),
                           .data[[var_cor]],
                           .data[[var]])
    )
  ### Cleaning auxiliary columns
  df_updated <- df_updated %>%
    select(-all_of(var_cor), -matches("_mapvars$"))
}

base_df <- df_updated
rm("changes_df", "df_delete", "df_updated", "map_var",
           "vars_corregidas", "var", "var_cor")


#' ------------------------------------------------------------------------
## Variables from main sheet ----------------------------------------------

# Reading Excel
base_chg <- read_excel(changes_path, sheet = "Encuesta de Línea Intermedia...")

# Changing values
## Variables to change
vars_corregidas <- names(base_chg)[
  !(names(base_chg) %in% c("surveyID","_uuid","commentsOnChanges"))]

## Loop by var
for (var in vars_corregidas) {
  ### Matching name
  var_cor <- paste0(var, "_mapvar")
  ### Map for observations with changes
  map_var <- base_chg %>%
    filter(!is.na(.data[[var]])) %>%
    select("surveyID","_uuid", !!sym(var))
  ### Changing df
  base_df <- base_df %>%
    left_join(map_var,
              by = c("surveyID","_uuid"), 
              suffix = c("", "_mapvar")) %>%
    mutate(
      !!sym(var) := ifelse(!is.na(.data[[var_cor]]),
                           .data[[var_cor]],
                           .data[[var]])
    )
  ### Cleaning auxiliary columns
  base_df <- base_df %>%
    select(-all_of(var_cor), -matches("_mapvar$"))
}



#' ========================================================================
#  lands sheet --------------------------------------------------------
#' ========================================================================

# Reading Excel
lands_chg <- read_excel(changes_path, sheet = "lands")


# Deleting observations
lands_df <- lands_df %>%
  anti_join(lands_chg %>% filter(delete == 1),by = c("_submission__uuid"))
lands_chg <- lands_chg %>% filter(is.na(delete))


# Changing values
## Variables to change
vars_corregidas <- names(lands_chg)[
  !(names(lands_chg) %in% c("landNumber","_submission__uuid","delete","commentsOnChanges"))]

## Loop by var
for (var in vars_corregidas) {
  ### Matching name
  var_cor <- paste0(var, "_mapvar")
  ### Map for observations with changes
  map_var <- lands_chg %>%
    filter(!is.na(.data[[var]])) %>%
    select("landNumber","_submission__uuid", !!sym(var))
  ### Changing df
  lands_df <- lands_df %>%
    left_join(map_var,
              by = c("landNumber","_submission__uuid"),
              suffix = c("", "_mapvar")) %>%
    mutate(
      !!sym(var) := ifelse(!is.na(.data[[var_cor]]),
                           .data[[var_cor]],
                           .data[[var]])
    )
  ### Cleaning auxiliary columns
  lands_df <- lands_df %>%
    select(-all_of(var_cor), -matches("_mapvar$"))
}



#' ========================================================================
#  sales_process_repeat sheet ---------------------------------------------
#' ========================================================================

# Reading Excel
sales_process_repeat_chg <- read_excel(changes_path, sheet = "sales_process_repeat")


# Changing values
## Variables to change
vars_corregidas <- names(sales_process_repeat_chg)[
  !(names(sales_process_repeat_chg) %in% c(
    "_index","_submission__uuid","commentsOnChanges"))]

## Loop by var
for (var in vars_corregidas) {
  ### Matching name
  var_cor <- paste0(var, "_mapvar")
  ### Map for observations with changes
  map_var <- sales_process_repeat_chg %>%
    filter(!is.na(.data[[var]])) %>%
    select("_index","_submission__uuid", !!sym(var))
  ### Changing df
  sales_process_repeat_df <- sales_process_repeat_df %>%
    left_join(map_var,
              by = c("_index","_submission__uuid"), 
              suffix = c("", "_mapvar")) %>%
    mutate(
      !!sym(var) := ifelse(!is.na(.data[[var_cor]]),
                           .data[[var_cor]],
                           .data[[var]])
    )
  ### Cleaning auxiliary columns
  sales_process_repeat_df <- sales_process_repeat_df %>%
    select(-all_of(var_cor), -matches("_mapvar$"))
}

rm(sales_process_repeat_chg,lands_chg,vars_corregidas,var,var_cor,map_var)
