# Loading Packages
packageList <- c("dplyr", "maps", "ggplot2", "sf", "scales", "viridis", "fuzzyjoin")
lapply(packageList,require,character.only=TRUE)

# Custom function to generate plots based on a dictionary
generate_plots <- function(var_dict_file, data_file, output_dir, spatial_data_dep, spatial_data_mun, spatial_data_com) {
  
  # Load dictionary, data
  var_dict <- read.csv(var_dict_file)
  data <- data_file
  data <- data %>%
    mutate(treatment_eng = case_when(
      treatment_eng == "Quality evaluation" ~ "QE",
      treatment_eng == "Technical assistance" ~ "TA",
      treatment_eng == "Technical assistance and quality evaluation" ~ "QE + TA",
      TRUE ~ treatment_eng  # Keep the original value if no match
    )) %>%
    mutate(time = case_when(
      time == 0 ~ "Baseline",
      time == 1 ~ "Endline",
      TRUE ~ as.character(time)  # Keep the original value if no match
    ))
  # Relevel
  data <- data %>%
    mutate(treatment_eng = factor(
        treatment_eng,levels = c("Control", "QE", "TA", "QE + TA")))
  
  # Colors
  color1 <- "#440154"
  color2 <- "#5ec962"
  
  #Load spatial data
  ## National
  countries <- world.cities %>% filter(country.etc == "Honduras")
  spatial_data_hnd <- st_as_sf(map("world", regions = "Honduras", fill = TRUE, plot = FALSE))
  ## Department
  spatial_data_dep <- st_read(spatial_data_dep)
  spatial_data_dep <- spatial_data_dep %>%
    rename(department_name = ADM1_ES)
  ## Municipality
  spatial_data_mun <- st_read(spatial_data_mun)
  spatial_data_mun <- spatial_data_mun %>%
    rename(department_name = ADM1_ES,
           municipality_name = ADM2_REF)
  ## Community
  spatial_data_com <- st_read(spatial_data_com) # For choropleth maps
  spatial_data_com <- st_transform(spatial_data_com, crs = 4326) #Adjusting coordenates
  coords <- st_coordinates(spatial_data_com)
  
  
  # Extract variables for each plot type from the dictionary
  bar_avg_vars    <- var_dict[which(var_dict$bar_plot_avg == 1), "new"]
  bar_cnt_vars    <- var_dict[which(var_dict$bar_plot_cnt == 1), "new"]
  line_vars_y     <- var_dict[which(var_dict$line_plot_y == 1), "new"]
  line_vars_x     <- var_dict[which(var_dict$line_plot_x == 1), "new"]
  donut_vars      <- var_dict[which(var_dict$donut_plot == 1), "new"]
  choropleth_vars <- var_dict[which(var_dict$choropleth_plot == 1), "new"]
  grouping_vars   <- var_dict[which(var_dict$grouping_var == 1), "new"]
  
  
  # Generate grouped bar plots for averages
  print(" ********** Grouped bar plots for averages ********** ")
  for (var in bar_avg_vars) {
    
    ## Grouped graphs
    if (!is.null(grouping_vars) && any(grouping_vars != "")) {
      for (grouping_var in grouping_vars) {
        ### Average by treatment and grouping_var
        summary_data <- data %>%
          mutate(!!sym(grouping_var) := as.factor(!!sym(grouping_var))) %>%  # Convert grouping_var column to factor
          filter(!is.na(treatment_eng) & !is.na(!!sym(grouping_var))) %>%     # To avoid NA's 
          group_by(treatment_eng, !!sym(grouping_var)) %>%
          summarise(
            mean_var = mean(get(var), na.rm = TRUE),              # Promedio
            se       = sd(get(var), na.rm = TRUE) / sqrt(n()),          # Error estándar
            ci_lower = mean_var - qt(0.975, df = n() - 1) * se,   # Límite inferior del IC 95%
            ci_upper = mean_var + qt(0.975, df = n() - 1) * se    # Límite superior del IC 95%
          )
        ### Getting averages by time
        line_data <- summary_data %>%
          group_by(!!sym(grouping_var)) %>% summarise(
            avg = mean(mean_var, na.rm = TRUE),.groups = "drop")
        ### Grouped bar plot
        p <- ggplot(summary_data, aes_string(x = "treatment_eng", y= "mean_var", fill = grouping_var)) +
          geom_col(position = "dodge") +
          geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), position = position_dodge(width = 0.9), width = 0.25) +
          geom_hline(data = line_data,aes(yintercept = avg, color = !!sym(grouping_var)),linewidth = 1,linetype = "dashed", show.legend = FALSE) +
          #scale_fill_viridis_d(option = "H") +
          #scale_color_viridis_d(option = "H") +
          scale_fill_manual(values = c(color1, color2)) +
          scale_color_manual(values = c(color1, color2)) +
          labs(title = paste("Bar chart of", var, "\ngrouped by", grouping_var), 
               x = "Treatment", y = "Average",
               caption = "QE: Quality Evaluation. TA: Technical Assistance.") +
          theme_minimal() +
          theme(legend.position = "bottom") +
          guides(fill = guide_legend(title = NULL))
        ### Save plot
        ggsave(filename = paste0(output_dir, "/", var, "_", grouping_var, "_bar_plot_avg.png"), plot = p)
      }
    }
    
    ## General graph.
    ### Average by treatment
    summary_data <- data %>% 
      filter(!is.na(treatment_eng)) %>%     # To avoid NA's 
      group_by(treatment_eng) %>%
      summarise(
        mean_var = mean(get(var), na.rm = TRUE),              # Promedio
        se       = sd(get(var), na.rm = TRUE) / sqrt(n()),          # Error estándar
        ci_lower = mean_var - qt(0.975, df = n() - 1) * se,   # Límite inferior del IC 95%
        ci_upper = mean_var + qt(0.975, df = n() - 1) * se    # Límite superior del IC 95%
      )
    ### Regular bar plot
    p <- ggplot(summary_data, aes_string(x = "treatment_eng", y = "mean_var", fill = "treatment_eng")) +
      geom_col() +
      scale_fill_viridis_d(option = "C") +  # Viridis color for the regular bar plot
      labs(title = paste("Bar chart of", var), 
           x = "Treatment", y = "Average",
           caption = "QE: Quality Evaluation. TA: Technical Assistance.") +
      theme_minimal() +
      theme(legend.position = "none")
    ### Save plot
    #ggsave(filename = paste0(output_dir, "/", var, "_bar_plot_avg.png"), plot = p)
  }
  
  
  
  # Generate grouped bar plots for sum
  print(" ********** Grouped bar plots for sum ********** ")
  for (var in bar_cnt_vars) {
    
    ## Grouped graphs
    if (!is.null(grouping_vars) && any(grouping_vars != "")) {
      for (grouping_var in grouping_vars) {
        ### Count by treatment and grouping_var
        summary_data <- data %>%
          mutate(!!sym(grouping_var) := as.factor(!!sym(grouping_var))) %>%  # Convert grouping_var column to factor
          filter(!is.na(treatment_eng) & !is.na(!!sym(grouping_var))) %>%     # To avoid NA's 
          group_by(treatment_eng, !!sym(grouping_var)) %>%
          summarise(mean_var = sum(get(var), na.rm = TRUE))
        ### Getting averages by time
        line_data <- summary_data %>%
          group_by(!!sym(grouping_var)) %>% summarise(
            avg = mean(mean_var, na.rm = TRUE),.groups = "drop")
        ### Grouped bar plot
        p <- ggplot(summary_data, aes_string(x = "treatment_eng", y= "mean_var", fill = grouping_var)) +
          geom_col(position = "dodge") +
          geom_hline(data = line_data,aes(yintercept = avg, color = !!sym(grouping_var)),linewidth = 1,linetype = "dashed", show.legend = FALSE) +
          scale_fill_manual(values = c(color1, color2)) +
          scale_color_manual(values = c(color1, color2)) +
          labs(title = paste("Bar chart of", var, "\ngrouped by", grouping_var), 
               x = "Treatment", y = "Count",
               caption = "QE: Quality Evaluation. TA: Technical Assistance.") +
          theme_minimal() +
          theme(legend.position = "bottom") +
          guides(fill = guide_legend(title = NULL))
        ### Save plot
        ggsave(filename = paste0(output_dir, "/", var, "_", grouping_var, "_bar_plot_cnt.png"), plot = p)
      }
    }
    
    ## General graph.
    ### Count by treatment
    summary_data <- data %>% 
      filter(!is.na(treatment_eng)) %>%     # To avoid NA's 
      group_by(treatment_eng) %>%
      summarise(mean_var = sum(get(var), na.rm = TRUE))
    ### Regular bar plot
    p <- ggplot(summary_data, aes_string(x = "treatment_eng", y = "mean_var", fill = "treatment_eng")) +
      geom_col() +
      scale_fill_viridis_d(option = "C") +  # Viridis color for the regular bar plot
      labs(title = paste("Bar chart of", var), 
           x = "Treatment", y = "Count",
           caption = "QE: Quality Evaluation. TA: Technical Assistance.") +
      theme_minimal() +
      theme(legend.position = "none")
    ### Save plot
    #ggsave(filename = paste0(output_dir, "/", var, "_bar_plot_cnt.png"), plot = p)
  }
  
  
  
  # Generate grouped line plots (assuming time series or continuous variables)
  print(" ********** Grouped line plots ********** ")
  for (line_var_y in line_vars_y) {
    for (line_var_x in line_vars_x) {
      
      ## Grouped graphs
      if (!is.null(grouping_vars) && any(grouping_vars != "")) {
        for (grouping_var in grouping_vars) {
          ### Grouped line plot
          p <- ggplot(data, aes_string(x = line_var_x, y = line_var_y, color = grouping_var)) +
            geom_point() +
            geom_smooth(method = "lm",se = FALSE,linewidth = 1,linetype = "dashed") +
            scale_fill_manual(values = c(color1, color2)) +
            scale_color_manual(values = c(color1, color2)) +
            labs(title = paste("Grouped Line Plot of", line_var_y, "\nover", line_var_x,", by", grouping_var),
                 x = line_var_x, y = line_var_y) +
            theme_minimal() +
            theme(legend.position = "bottom")
          ### Save plot
          ggsave(filename = paste0(output_dir, "/", line_var_y, "Over", line_var_x, "_", grouping_var, "_line_plot.png"), plot = p)
        }
      }
      
      ## General graph.
      ### Regular line plot
      p <- ggplot(data, aes_string(x = line_var_x, y = line_var_y)) +
        geom_point() +
        labs(title = paste("Line Plot of", line_var_y, "\nover", line_var_x), x = line_var_x, y = line_var_y) +
        theme_minimal()
      ### Save plot
      #ggsave(filename = paste0(output_dir, "/", var, "_line_plot.png"), plot = p)
    }
  }
  
  
  
  # Generate donut charts (for categorical variables)
  print(" ********** Donut charts ********** ")
  for (var in donut_vars) {
    ## Collapsing data
    data_donut <- data %>%
      mutate(!!sym(var) := as.factor(!!sym(var))) %>%  # Convert var column to factor
      filter(!is.na(!!sym(var))) %>%     #To avoid NAs
      group_by_at(var) %>%
      summarise(count = n()) %>%
      mutate(percentage = count / sum(count))
    ## Donut plot
    p <- ggplot(data_donut, aes(x = 2, y = percentage, fill = !!sym(var))) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      scale_fill_viridis_d(option = "C") +  # Viridis color for donut chart
      xlim(0.5, 2.5) +
      theme_void() +
      theme(legend.title = element_blank()) +
      labs(title = paste("Donut Chart of", var))
    ## Save plot
    ggsave(filename = paste0(output_dir, "/", var, "_donut_chart.png"), plot = p)
  }
  
  
  
  # Generate choropleth maps
  print(" ********** Choropleth maps ********** ")
  for (var in choropleth_vars) {
    ## Map by departments
    ### Average by department
    summary_data <- data %>%
      filter(!is.na(department_name)) %>%     # To avoid NA's 
      group_by(department_name) %>%
      summarise(mean_var = mean(!!sym(var), na.rm = TRUE))
    ### Combining data
    merged_data <- spatial_data_dep %>%
      left_join(summary_data, by = "department_name")
    ### Choroplet
    p <- ggplot(data = merged_data) +
      geom_sf(aes(fill = mean_var), color = "black", alpha = 0.6) +
      labs(title = paste("Mapa de", var, "por departamento"), fill = "") +
      theme_minimal() +
      scale_fill_gradientn(colors = rev(viridis(256)), na.value = "white") + 
      theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
    ### Save plot
    #ggsave(filename = paste0(output_dir, "/", var, "_choropleth_map_department.png"), plot = p)       
    
    ## May by municipality
    ### Average by municipality
    summary_data <- data %>%
      filter(!is.na(municipality_name)) %>%     # To avoid NA's 
      group_by(municipality_name, department_name) %>%
      summarise(mean_var = mean(!!sym(var), na.rm = TRUE))
    ### Combining data
    merged_data <- stringdist_left_join(
      spatial_data_mun, summary_data,
      by = c("municipality_name", "department_name"),
      max_dist = 1,  # Set a maximum distance for fuzzy matching
      distance_col = NULL
    ) %>% # Remove duplicate columns from the join
      select(-ends_with(".y")) %>%  # Remove all columns that end with ".y" (i.e., from ttmAssign)
      rename_with(~ gsub(".x$", "", .), ends_with(".x")) # Remove ".x" suffix from the original columns
    #mutate(municipality_name = if_else(is.na(mean_var), NA_character_, municipality_name))
    ### Choroplet
    p <- ggplot(data = merged_data) +
      geom_sf(aes(fill = mean_var, geometry = geometry), color = "black", alpha = 0.6) +
      #geom_sf_text(aes(label = municipality_name, geometry = geometry), size = 2, color = "black") +
      labs(title = paste("Mapa de", var, "por municipio"), fill = "") +
      theme_minimal() +
      scale_fill_gradientn(colors = rev(viridis(256)), na.value = "white") + 
      theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
    ### Save plot
    ggsave(filename = paste0(output_dir, "/", var, "_choropleth_map_municipality.png"), plot = p)
    
    ## Map by community
    ### Average by community
    summary_data <- data %>%
      filter(!is.na(communityOrHamlet_name)) %>%
      group_by(communityOrHamlet_name, municipality_name, department_name) %>%
      summarise(mean_var = mean(!!sym(var), na.rm = TRUE)) %>%
      mutate(communityOrHamlet_name = recode(communityOrHamlet_name,
                                             "Aguacatal" = "El Aguacatal",
                                             "Macuzal"   = "El Macuzal"))
    ### Creating data: coordenates, location names, mean_var
    spatial_data_com_new <- data.frame(
      long = coords[, 1],
      lat = coords[, 2],
      communityOrHamlet_name = spatial_data_com$CASERIO,
      municipality_name = spatial_data_com$Municipio,
      department_name = spatial_data_com$Departamen) %>%
      filter(!is.na(communityOrHamlet_name) & !is.na(municipality_name) & !is.na(department_name)) #Se van alredor de 5000 comunidades sin coordenadas
    ### Combining data
    merged_data <- stringdist_left_join(
      spatial_data_com_new, summary_data,
      by = c("communityOrHamlet_name", "municipality_name", "department_name"),
      max_dist = 1,  # Set a maximum distance for fuzzy matching
      distance_col = NULL
    ) %>% # Remove duplicate columns from the join
      select(-ends_with(".y")) %>%  # Remove all columns that end with ".y" (i.e., from ttmAssign)
      rename_with(~ gsub(".x$", "", .), ends_with(".x")) %>% # Remove ".x" suffix from the original columns
      filter(!is.na(mean_var)) # To avoid other points.
    ### Choroplet
    p <- ggplot() +
      geom_sf(data = spatial_data_hnd, fill = "white", color = "black", alpha = 0.5) +  # Mapa de Honduras
      geom_point(data = merged_data, aes(x = long, y = lat, color = mean_var), size = 1, alpha = 0.7) +  # Puntos del df_basic
      scale_color_viridis(direction = -1) +
      labs(title = paste("Mapa de", var, "por comunidad"), color = "") +
      theme_minimal() +
      theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
    ### Save plot
    #ggsave(filename = paste0(output_dir, "/", var, "_choropleth_map_community.png"), plot = p)
  }
  
}
