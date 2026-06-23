library(readxl)
library(openxlsx)
library(tidyverse)

file <- read_excel("fake_policy_data_150_rows.xlsx")
regions <- read_excel("go_code_zone_mapping.xlsx")
view(file)
view(regions)


# Add the required columns ------------------------------------------------
get_region <- function(file)
{
  file = file %>% left_join(regions, by = c("go_code" = "GO CODE"))
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
  return(paste("The average of the total TAT is", 
               round(mean(file$tot_TAT, na.rm = TRUE)), "days"))
}

get_tot_TAT_med <- function(file)
{
  return(paste("The median of the total TAT is", 
               round(median(file$tot_TAT, na.rm = TRUE)), "days"))
}

get_transm_TAT_avg <- function(file)
{
  return(paste("The average of the transmission TAT is", 
               round(mean(file$transmission_TAT, na.rm = TRUE)), "days"))
}

get_transm_TAT_med <- function(file)
{
  return(paste("The median of the transmission TAT is",
               round(median(file$transmission_TAT, na.rm = TRUE)), "days"))
}

# Click Rate --------------------------------------------------------------
get_CR <- function(file)
{ 
  num <- file %>% filter(!is.na(`Client Last Accessed Site`))
  numerator <- nrow(num)
  
  denominator <- nrow(file)
  
  return (numerator/denominator)
}

# Validation Rate ---------------------------------------------------------
get_VR <- function(file)
{
  num <- file %>% filter(!is.na(`Client Submitted Username/Password`))
  numerator <- nrow(num)
  
  denominator <- nrow(file)
  
  return(numerator/denominator)
}

# Retrieval Rate ----------------------------------------------------------
get_RR <- function(file)
{
  num <- file %>% filter(!is.na(`Record Received`))
  numerator <- nrow(num)
  
  denominator <- nrow(file)
  
  return(numerator/denominator)
}

# True Validation Rate ----------------------------------------------------
get_TVR <- function(file)
{
  num <- file %>% filter(!is.na(`Client Submitted Username/Password`))
  numerator <- nrow(num)
  
  denom <- file %>% filter(!is.na(`Client Last Accessed Site`))
  denominator <- nrow(denom)
  
  return(numerator/denominator)
}

# Transmission Rate -------------------------------------------------------
get_TR <- function(file)
{
  num <- file %>% filter(!is.na(`Record Received`))
  numerator <- nrow(num)
  
  denom <- file %>% filter(!is.na(`Client Submitted Username/Password`))
  denominator <- nrow(denom)
  
  return(numerator/denominator)
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
  
  return (numerator/denominator)
}

CR_WC <- function(file)
{
  sub <- file %>% filter(ZONE == "WC")
  num <- sub %>% filter(!is.na(`Client Last Accessed Site`))
  numerator <- nrow(num)
  
  denominator <- nrow(sub)
  
  return (numerator/denominator)
}

CR_SC <- function(file)
{
  sub <- file %>% filter(ZONE == "SC")
  num <- sub %>% filter(!is.na(`Client Last Accessed Site`))
  numerator <- nrow(num)
  
  denominator <- nrow(sub)
  
  return (numerator/denominator)
}
CR_PA <- function(file)
{
  sub <- file %>% filter(ZONE == "PA")
  num <- sub %>% filter(!is.na(`Client Last Accessed Site`))
  numerator <- nrow(num)
  
  denominator <- nrow(sub)
  
  return (numerator/denominator)
}

CR_NA <- function(file)
{
  sub <- file %>% filter(is.na(ZONE))
  num <- sub %>% filter(!is.na(`Client Last Accessed Site`))
  numerator <- nrow(num)
  
  denominator <- nrow(sub)
  
  return (numerator/denominator)
}

# Validation Rate for Each Region -----------------------------------------
VR_NE <- function(file)
{
  sub <- file %>% filter(ZONE == "NE")
  num <- sub %>% filter(!is.na(`Client Submitted Username/Password`))
  numerator <- nrow(num)
  
  denominator <- nrow(sub)
  
  return(numerator/denominator)
}

VR_WC <- function(file)
{
  sub <- file %>% filter(ZONE == "WC")
  num <- sub %>% filter(!is.na(`Client Submitted Username/Password`))
  numerator <- nrow(num)
  
  denominator <- nrow(sub)
  
  return(numerator/denominator)
}
VR_SC <- function(file)
{
  sub <- file %>% filter(ZONE == "SC")
  num <- sub %>% filter(!is.na(`Client Submitted Username/Password`))
  numerator <- nrow(num)
  
  denominator <- nrow(sub)
  
  return(numerator/denominator)
}

VR_PA <- function(file)
{
  sub <- file %>% filter(ZONE == "PA")
  num <- sub %>% filter(!is.na(`Client Submitted Username/Password`))
  numerator <- nrow(num)
  
  denominator <- nrow(sub)
  
  return(numerator/denominator)
}

VR_NA <- function(file)
{
  sub <- file %>% filter(is.na(ZONE))
  num <- sub %>% filter(!is.na(`Client Submitted Username/Password`))
  numerator <- nrow(num)
  
  denominator <- nrow(sub)
  
  return(numerator/denominator)
}

# Retrieval Rate for Each Region ------------------------------------------
RR_NE <- function(file)
{
  sub <- file %>% filter(ZONE == "NE")
  num <- sub %>% filter(!is.na(`Record Received`))
  numerator <- nrow(num)
  
  denominator <- nrow(sub)
  
  return(numerator/denominator)
}

RR_WC <- function(file)
{
  sub <- file %>% filter(ZONE == "WC")
  num <- sub %>% filter(!is.na(`Record Received`))
  numerator <- nrow(num)
  
  denominator <- nrow(sub)
  
  return(numerator/denominator)
}

RR_SC <- function(file)
{
  sub <- file %>% filter(ZONE == "SC")
  num <- sub %>% filter(!is.na(`Record Received`))
  numerator <- nrow(num)
  
  denominator <- nrow(sub)
  
  return(numerator/denominator)
}

RR_PA <- function(file)
{
  sub <- file %>% filter(ZONE == "PA")
  num <- sub %>% filter(!is.na(`Record Received`))
  numerator <- nrow(num)
  
  denominator <- nrow(sub)
  
  return(numerator/denominator)
}

RR_NA <- function(file)
{
  sub <- file %>% filter(ia.na(ZONE))
  num <- sub %>% filter(!is.na(`Record Received`))
  numerator <- nrow(num)
  
  denominator <- nrow(sub)
  
  return(numerator/denominator)
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



# Testing -----------------------------------------------------------------
test <- get_region(file)
test <- get_TATs(test)
view(test)
get_vol(test)
get_vol_NE(test)
get_vol_WC(test)
get_vol_SC(test)
get_vol_PA(test)
get_vol_NA(test)


  