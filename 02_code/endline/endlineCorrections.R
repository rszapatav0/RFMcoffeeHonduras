#' ------------------------------------------------------------------------
# Main sheet --------------------------------------------------------------
#' ------------------------------------------------------------------------
#' endlineCorrections.xlsx is an edited from 
#' "01_data/raw/endline/Entregable [2] 2024-09-05 Duplicados WP1 Café"
#' so that corrections can be implemented directly through code.

# Reading Excel
changes_df  <- read_excel(changes_path, sheet = "Encuesta de Línea Intermedia...")

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
