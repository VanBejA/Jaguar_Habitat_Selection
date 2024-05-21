## Title: "utm_epsg" function
## Author:  Vanesa Bejarano Alegre
## Mail: vanesa.bejarano@gmail.com

# utm_epsg function -------------------------------------------------------

utm_epsg <- function(lat, lon) {
  zone_number <- (floor((lon + 180) / 6) %% 60) + 1
  # North of Mexico zone 12
  cond_12 <- lat >= 23.9 & lat < 31.9 & lon >= -113.9 & lon < -107.9
  zone_number[cond_12] <- 12
  # Yucatan peninsula  zone 15 and 16
  cond_lat <- lat >= 15.9 & lat < 23.81
  
  cond_15 <- cond_lat & lon >= -95.9 & lon < -89.9
  zone_number[cond_15] <- 15
  
  cond_16 <- cond_lat & lon >= -89.9 & lon < -83.9
  zone_number[cond_16] <- 16
  # South America zone 20, 21, 22 and 23
  cond_lat1 <- lat >= -32.0 & lat < -0.1
  
  cond_20 <- cond_lat1 & lon >= -65.8 & lon < -59.9  
  zone_number[cond_20] <- 20
  
  cond_21 <- cond_lat1 & lon >= -59.9 & lon < -53.9  
  zone_number[cond_21] <- 21
  
  cond_22 <- cond_lat1 & lon >= -53.9 & lon < -47.9  
  zone_number[cond_22] <- 22
  
  cond_23 <- cond_lat1 & lon >= -47.9 & lon < -41.9  
  zone_number[cond_23] <- 23
  
  # EPSG code
  utm <- zone_number
  utm[lat > 0] <- utm[lat > 0] + 32600
  utm[lat <= 0] <- utm[lat <= 0] + 32700
  
  return(utm)
}


### Time differences
# Calculation of time between fixes
time.between <- function(individual, dat) {
  # Select individual
  ind <- dat[dat$individual.local.identifier == individual,]
  # Calculate difference in time, in hours, and return
  c(as.numeric(diff.POSIXt(ind$timestampUTC), units = 'hours'), NA)
}