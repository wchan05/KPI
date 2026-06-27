# Add the required columns ------------------------------------------------
get_region <- function(file_path)
{
  file <- readxl::read_excel(file_path)
  
  regions <- readxl::read_excel(
    system.file(
      "extdata",
      "go_code_zone_mapping.xlsx",
      package = "KPIcalc"))
  
  file <- dplyr::left_join(
    file,
    regions,
    by = c("go_code" = "GO CODE"))
  
  return(file)
}

get_TATs <- function(file)
{
  file$tot_TAT <- as.numeric(
    difftime(file$`Record Received`,
      file$`Order Date`,
      units = "days"))
  
  file$transmission_TAT <- as.numeric(
    difftime(file$`Record Received`,
      file$`Client Submitted Username/Password`,
      units = "days"))
  
  return(file)
}

# TAT - Average and Median Total and Transmission -------------------------
get_tot_TAT_avg <- function(file)
{
  return(round(mean(file$tot_TAT, na.rm = TRUE)))
}

get_tot_TAT_med <- function(file)
{
  return(round(median(file$tot_TAT, na.rm = TRUE)))
}

get_transm_TAT_avg <- function(file)
{
  return(round(mean(file$transmission_TAT, na.rm = TRUE)))
}

get_transm_TAT_med <- function(file)
{
  return(round(median(file$transmission_TAT, na.rm = TRUE)))
}

# Click Rate --------------------------------------------------------------
get_CR <- function(file)
{ 
  num <- file %>% filter(!is.na(`Client Last Accessed Site`))
  numerator <- nrow(num)
  
  denominator <- nrow(file)
  
  return (numerator/denominator * 100)
}

# Validation Rate ---------------------------------------------------------
get_VR <- function(file)
{
  num <- file %>% filter(!is.na(`Client Submitted Username/Password`))
  numerator <- nrow(num)
  
  denominator <- nrow(file)
  
  return(numerator/denominator * 100)
}

# Retrieval Rate ----------------------------------------------------------
get_RR <- function(file)
{
  num <- file %>% filter(!is.na(`Record Received`))
  numerator <- nrow(num)
  
  denominator <- nrow(file)
  
  return(numerator/denominator * 100)
}

# True Validation Rate ----------------------------------------------------
get_TVR <- function(file)
{
  num <- file %>% filter(!is.na(`Client Submitted Username/Password`))
  numerator <- nrow(num)
  
  denom <- file %>% filter(!is.na(`Client Last Accessed Site`))
  denominator <- nrow(denom)
  
  return(numerator/denominator * 100)
}

# Transmission Rate -------------------------------------------------------
get_TR <- function(file)
{
  num <- file %>% filter(!is.na(`Record Received`))
  numerator <- nrow(num)
  
  denom <- file %>% filter(!is.na(`Client Submitted Username/Password`))
  denominator <- nrow(denom)
  
  return(numerator/denominator * 100)
}


# How many days are there? -------------------------------------------------
get_days_count <- function(file)
{
  temp <- file
  temp$day <- as.Date(temp$`Order Date`)
  return(length(unique(temp$day)))
}

# Average Orders/Day ------------------------------------------------------
avg_orders_day <- function(file)
{
  num_days <- n_distinct(as.Date(file$`Order Date`)) 
  num_orders <- n_distinct(file$policy_number)
  return(num_orders/num_days)
}

# Records Received/Day -------------------------------------------------------------
records_day <- function(file)
{
  num_days <- n_distinct(as.Date(file$'Order Date'))
  num_records <- nrow(file %>% filter(!is.na(`Record Received`)))
  return(num_records/num_days)
}

# Average Orders/Week -----------------------------------------------------
avg_orders_week <- function(file)
{
  num_weeks <- n_distinct(floor_date(as.Date(file$`Order Date`), "week"))
  num_orders <- n_distinct(file$policy_number)
  return(num_orders/num_weeks)
}

# Click Rate for Each Region ----------------------------------------------
CR_NE <- function(file)
{
  sub <- file %>% filter(ZONE == "NE")
  num <- sub %>% filter(!is.na(`Client Last Accessed Site`))
  numerator <- nrow(num)
  
  denominator <- nrow(sub)
  
  return (numerator/denominator * 100)
}

CR_WC <- function(file)
{
  sub <- file %>% filter(ZONE == "WC")
  num <- sub %>% filter(!is.na(`Client Last Accessed Site`))
  numerator <- nrow(num)
  
  denominator <- nrow(sub)
  
  return (numerator/denominator * 100)
}

CR_SC <- function(file)
{
  sub <- file %>% filter(ZONE == "SC")
  num <- sub %>% filter(!is.na(`Client Last Accessed Site`))
  numerator <- nrow(num)
  
  denominator <- nrow(sub)
  
  return (numerator/denominator * 100)
}
CR_PA <- function(file)
{
  sub <- file %>% filter(ZONE == "PA")
  num <- sub %>% filter(!is.na(`Client Last Accessed Site`))
  numerator <- nrow(num)
  
  denominator <- nrow(sub)
  
  return (numerator/denominator * 100)
}

CR_NA <- function(file)
{
  sub <- file %>% filter(is.na(ZONE))
  num <- sub %>% filter(!is.na(`Client Last Accessed Site`))
  numerator <- nrow(num)
  
  denominator <- nrow(sub)
  
  return (numerator/denominator * 100)
}

# Validation Rate for Each Region -----------------------------------------
VR_NE <- function(file)
{
  sub <- file %>% filter(ZONE == "NE")
  num <- sub %>% filter(!is.na(`Client Submitted Username/Password`))
  numerator <- nrow(num)
  
  denominator <- nrow(sub)
  
  return(numerator/denominator * 100)
}

VR_WC <- function(file)
{
  sub <- file %>% filter(ZONE == "WC")
  num <- sub %>% filter(!is.na(`Client Submitted Username/Password`))
  numerator <- nrow(num)
  
  denominator <- nrow(sub)
  
  return(numerator/denominator * 100)
}
VR_SC <- function(file)
{
  sub <- file %>% filter(ZONE == "SC")
  num <- sub %>% filter(!is.na(`Client Submitted Username/Password`))
  numerator <- nrow(num)
  
  denominator <- nrow(sub)
  
  return(numerator/denominator * 100)
}

VR_PA <- function(file)
{
  sub <- file %>% filter(ZONE == "PA")
  num <- sub %>% filter(!is.na(`Client Submitted Username/Password`))
  numerator <- nrow(num)
  
  denominator <- nrow(sub)
  
  return(numerator/denominator * 100)
}

VR_NA <- function(file)
{
  sub <- file %>% filter(is.na(ZONE))
  num <- sub %>% filter(!is.na(`Client Submitted Username/Password`))
  numerator <- nrow(num)
  
  denominator <- nrow(sub)
  
  return(numerator/denominator * 100)
}

# Retrieval Rate for Each Region ------------------------------------------
RR_NE <- function(file)
{
  sub <- file %>% filter(ZONE == "NE")
  num <- sub %>% filter(!is.na(`Record Received`))
  numerator <- nrow(num)
  
  denominator <- nrow(sub)
  
  return(numerator/denominator * 100)
}

RR_WC <- function(file)
{
  sub <- file %>% filter(ZONE == "WC")
  num <- sub %>% filter(!is.na(`Record Received`))
  numerator <- nrow(num)
  
  denominator <- nrow(sub)
  
  return(numerator/denominator * 100)
}

RR_SC <- function(file)
{
  sub <- file %>% filter(ZONE == "SC")
  num <- sub %>% filter(!is.na(`Record Received`))
  numerator <- nrow(num)
  
  denominator <- nrow(sub)
  
  return(numerator/denominator * 100)
}

RR_PA <- function(file)
{
  sub <- file %>% filter(ZONE == "PA")
  num <- sub %>% filter(!is.na(`Record Received`))
  numerator <- nrow(num)
  
  denominator <- nrow(sub)
  
  return(numerator/denominator * 100)
}

RR_NA <- function(file)
{
  sub <- file %>% filter(is.na(ZONE))
  num <- sub %>% filter(!is.na(`Record Received`))
  numerator <- nrow(num)
  
  denominator <- nrow(sub)
  
  return(numerator/denominator * 100)
}

# Order Volume for each and overall ---------------------------------------
get_vol <- function(file)
{
  return(nrow(file))
}

get_vol_NE <- function(file) sum(file$ZONE == "NE", na.rm = TRUE)
get_vol_WC <- function(file) sum(file$ZONE == "WC", na.rm = TRUE)
get_vol_SC <- function(file) sum(file$ZONE == "SC", na.rm = TRUE)
get_vol_PA <- function(file) sum(file$ZONE == "PA", na.rm = TRUE)
get_vol_NA <- function(file) sum(is.na(file$ZONE))

# Calc_KPI ----------------------------------------------------------------
#' Calculate all KPI metrics
#'
#' Main function that processes a file path and returns all KPIs.
#'
#' @param file_path path to Excel file
#' @return data.frame of KPI metrics
#' @export
calc_kpi <- function(file_path)
{
  compiled <- get_TATs(get_region(file_path))
  
  data.frame(
    metric = c("tot_volume",
      "avg_tat",
      "med_tat",
      "avg_transm_tat",
      "med_transm_tat",
      "click_rate",
      "validation_rate",
      "retrieval_rate",
      "true_validation_rate",
      "transmission_rate",
      "avg_orders_per_day",
      "avg_orders_per_week",
      "record_rate",
      
      # Click Rate by Region
      "CR_NE", "CR_WC", "CR_SC", "CR_PA", "CR_NA",
      
      # Validation Rate by Region
      "VR_NE", "VR_WC", "VR_SC", "VR_PA", "VR_NA",
      
      # Retrieval Rate by Region
      "RR_NE", "RR_WC", "RR_SC", "RR_PA", "RR_NA",
      
      # Volume by Region
      "vol_NE", "vol_WC", "vol_SC", "vol_PA", "vol_NA"
    ),
    
    value = c(get_vol(compiled),
      get_tot_TAT_avg(compiled),
      get_tot_TAT_med(compiled),
      get_transm_TAT_avg(compiled),
      get_transm_TAT_med(compiled),
      get_CR(compiled),
      get_VR(compiled),
      get_RR(compiled),
      get_TVR(compiled),
      get_TR(compiled),
      avg_orders_day(compiled),
      avg_orders_week(compiled),
      records_day(compiled),
      
      # Click Rate by Region
      CR_NE(compiled),
      CR_WC(compiled),
      CR_SC(compiled),
      CR_PA(compiled),
      CR_NA(compiled),
      
      # Validation Rate by Region
      VR_NE(compiled),
      VR_WC(compiled),
      VR_SC(compiled),
      VR_PA(compiled),
      VR_NA(compiled),
      
      # Retrieval Rate by Region
      RR_NE(compiled),
      RR_WC(compiled),
      RR_SC(compiled),
      RR_PA(compiled),
      RR_NA(compiled),
      
      # Volume by Region
      get_vol_NE(compiled),
      get_vol_WC(compiled),
      get_vol_SC(compiled),
      get_vol_PA(compiled),
      get_vol_NA(compiled)
    )
  )
}

