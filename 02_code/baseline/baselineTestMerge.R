sheet_names <- "" 
#maintable mod_cierre mod_l mod_k mod_j mod_i mod_h mod_g mod_f               
#tech_repeat mod_e sales_repeat sales_process_repeat repeatvar pnd_repeat
#area_terr mod_d mod_c hh_pop_repeat mod_b mod_a

#maintable mod_cierre mod_l mod_k mod_j mod_i mod_h mod_g mod_f mod_e mod_d mod_c mod_b mod_a 
mod_tabs_data2 <- list(mod_tabs_data[["maintable"]], mod_tabs_data[["mod_l"]])

mod_merged_data <- mod_tabs_data[["maintable"]]

mod_merged_data <- reduce(mod_tabs_data2, full_join, by = "ID_ENCUESTA")
final_merged_data[["mod_combined"]] <- mod_merged_data


mod_merged_data <- full_join(mod_tabs_data[["maintable"]], mod_tabs_data[["mod_l"]], by = "ID_ENCUESTA")
