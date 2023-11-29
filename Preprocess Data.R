remove(list=ls())

tiers = read.csv("./data/raw_data/drug_tiers.csv")
data = read.csv("./data/raw_data/data.csv")
data = subset(data, select = -c(FUSIDIC.ACID, CEFOPERAZONE))
rownames(data) = NULL
data[15:69][data[15:69] != 'R' & data[15:69] != 'S' & data[15:69] != 'I'] = NA

get_multi_col_count = function(frame) {
  counts = apply((frame), 2, table) # Get the frequency of each resistance measure per microbe in the zipcode

  r = c()
  s = c()
  i = c()
  for (col in names(counts)) {
    single_count = as.data.frame(counts[[col]])
    
    r = append(r, single_count$Freq[single_count$Var1 == "R"])
    s = append(s, single_count$Freq[single_count$Var1 == "S"])
    i = append(i, single_count$Freq[single_count$Var1 == "I"])
  }
  
  return (data.frame(Var1=c("R", "S", "I"), Freq=c(sum(r), sum(s), sum(i))))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Processed Resistance Rate~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
zip_data_antibiotic = data.frame(
  Zip=integer(),
  Tier=integer(), 
  Antibiotic=character(), 
  ResistanceLevel=character(), 
  PercentAntibiotic=double(),
  AntibioticFrequencyCount=integer(),
  PercentTier=double(), 
  stringsAsFactors=FALSE
)
zip_data_microbe = data.frame(
  Zip=integer(),
  Microbe=character(),
  ResistanceLevel=character(), 
  PercentMicrobe=double(),
  MicrobeFrequencyCount=integer(),
  stringsAsFactors=FALSE
)

for (zip in unique(data$zip_3_level)) {
  zip_rows = data[data$zip_3_level == zip,] # Get zipcode rows
  
  # Process Antibiotic data for zipcode
  for (col in colnames(data)[15:69]) {
    tier = tiers$tier[tiers$pseud_drug == col] # Get the tier of the antibiotic
    
    antibiotic_counts = as.data.frame(table(zip_rows[[col]])) # Get the frequency of each resistance measure per antibiotic in the zipcode
    num_antibiotic_counts = sum(antibiotic_counts$Freq)
    
    for (type in c("R", "S", "I")) {
      antibiotic_freq = antibiotic_counts$Freq[antibiotic_counts$Var1 == type]
      antibiotic_percent = (antibiotic_freq / num_antibiotic_counts) # P(r_level | zip & antibiotic)
      
      if (identical(antibiotic_freq, integer(0))) antibiotic_freq = 0
      if (identical(antibiotic_percent, numeric(0))) antibiotic_percent = 0
      
      zip_data_antibiotic[nrow(zip_data_antibiotic) + 1,] = c(zip, tier, col, type, antibiotic_percent, antibiotic_freq, NA)
    }
  }
  
  # Process Microbe data for zipcode
  for (microbe in unique(zip_rows$org_standard)) {
    microbe_rows = zip_rows[zip_rows$org_standard == microbe,] # Get microbe rows in zipcode
    microbe_counts = get_multi_col_count(microbe_rows[15:69]) # Get the frequency of each resistance measure per microbe in the zipcode
    num_microbe_counts = sum(microbe_counts$Freq)
    
    for (type in c("R", "S", "I")) {
      microbe_freq = microbe_counts$Freq[microbe_counts$Var1 == type]
      microbe_percent = (microbe_freq / num_microbe_counts) # P(r_level | zip & microbe)
      
      if (identical(microbe_percent, numeric(0))) microbe_percent = 0
      
      zip_data_microbe[nrow(zip_data_microbe) + 1,] = c(zip, microbe, type, microbe_percent, microbe_freq)
    }
  }
}

# Process Rates by Tier
for (zip in unique(zip_data_antibiotic$Zip)) {
  zip_rows = zip_data_antibiotic[zip_data_antibiotic$Zip == zip,] # Get zipcode rows
  for (tier in unique(zip_rows$Tier)) {
    tier_rows = zip_rows[zip_rows$Tier == tier,] # Get tier rows
    tier_sum = sum(as.numeric(tier_rows$AntibioticFrequencyCount)) # Get the total number of observations in the tier
    for(type in unique(tier_rows$ResistanceLevel)) {
      type_rows = tier_rows[tier_rows$ResistanceLevel == type,] # Get the resistance measure rows
      type_sum = sum(as.numeric(type_rows$AntibioticFrequencyCount)) # Get the total number of resistance measures of a specific type in the tier and zipcode
      
      zip_data_antibiotic$PercentTier[zip_data_antibiotic$Zip == zip & zip_data_antibiotic$Tier == tier & zip_data_antibiotic$ResistanceLevel == type] = type_sum / tier_sum # Get the percentage this resistance measure contributes to the tier in the zipcode
    }
  }
}

readr::write_rds(zip_data_antibiotic, "./data/dashboard_data/zip_data_antibiotic_resistance_rate.Rds")
readr::write_rds(zip_data_microbe, "./data/dashboard_data/zip_data_microbe_resistance_rate.Rds")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Processed Test Rate~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
zip_data_antibiotic = data.frame(
  Zip=integer(),
  Tier=integer(), 
  Antibiotic=character(), 
  PercentAntibiotic=double(),
  AntibioticFrequencyCount=integer(),
  PercentTier=double(), 
  stringsAsFactors=FALSE
)
zip_data_microbe = data.frame(
  Zip=integer(),
  Microbe=character(),
  PercentMicrobe=double(),
  MicrobeFrequencyCount=integer(),
  stringsAsFactors=FALSE
)

num_tests = get_multi_col_count(data[15:69])
num_tests = sum(num_tests$Freq)

for (zip in unique(data$zip_3_level)) {
  zip_rows = data[data$zip_3_level == zip,] # Get zipcode rows
  
  # Process Antibiotic data for zipcode
  for (col in colnames(data)[15:69]) {
    tier = tiers$tier[tiers$pseud_drug == col] # Get the tier of the antibiotic
    
    antibiotic_counts = as.data.frame(table(zip_rows[[col]])) # Get the frequency of each resistance measure per antibiotic in the zipcode
    antibiotic_freq = sum(antibiotic_counts$Freq)
    antibiotic_percent = (antibiotic_freq / num_tests) # P(test_performed | zip & antibiotic)
    
    if (identical(antibiotic_freq, integer(0))) antibiotic_freq = 0
    if (identical(antibiotic_percent, numeric(0))) antibiotic_percent = 0
    
    zip_data_antibiotic[nrow(zip_data_antibiotic) + 1,] = c(zip, tier, col, antibiotic_percent, antibiotic_freq, NA)
  }
  
  # Process Microbe data for zipcode
  for (microbe in unique(zip_rows$org_standard)) {
    microbe_rows = zip_rows[zip_rows$org_standard == microbe,] # Get microbe rows in zipcode
    microbe_counts = get_multi_col_count(microbe_rows[15:69]) # Get the frequency of each resistance measure per microbe in the zipcode
    microbe_freq = sum(microbe_counts$Freq)
    microbe_percent = (microbe_freq / num_tests) # P(test_performed | zip & microbe)
    
    if (identical(microbe_percent, numeric(0))) microbe_percent = 0
    
    zip_data_microbe[nrow(zip_data_microbe) + 1,] = c(zip, microbe, microbe_percent, microbe_freq)
  }
}

# Process Rates by Tier
for (zip in unique(zip_data_antibiotic$Zip)) {
  zip_rows = zip_data_antibiotic[zip_data_antibiotic$Zip == zip,] # Get zipcode rows
  for (tier in unique(zip_rows$Tier)) {
    tier_rows = zip_rows[zip_rows$Tier == tier,] # Get tier rows
    tier_sum = sum(as.numeric(tier_rows$AntibioticFrequencyCount)) # Get the total number of observations in the tier
    
    zip_data_antibiotic$PercentTier[zip_data_antibiotic$Zip == zip & zip_data_antibiotic$Tier == tier] = tier_sum / num_tests # Get the percentage this resistance measure contributes to the tier in the zipcode
  }
}

readr::write_rds(zip_data_antibiotic, "./data/dashboard_data/zip_data_antibiotic_test_rate.Rds")
readr::write_rds(zip_data_microbe, "./data/dashboard_data/zip_data_microbe_test_rate.Rds")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Processed Moran's I values~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
moran_data = data.frame(
  Zip=character(),
  Microbe=character(),
  ResistantCount=integer(), 
  TestCount=integer(), 
  ResistanceRate=double(),
  stringsAsFactors=FALSE
)
for (zip in unique(data$zip_3_level)) {
  zip_rows = data[data$zip_3_level == zip,] # Get zipcode rows
  for (microbe in unique(zip_rows$org_standard)) {
    test_count = get_multi_col_count(data[data$zip_3_level == zip & data$org_standard == microbe,][15:69])
    
    num_tests = sum(test_count$Freq)
    resistance_count = test_count$Freq[test_count$Var1 == "R"]
    resistance_rate = resistance_count / num_tests
    
    moran_data[nrow(moran_data) + 1,] = c(zip, microbe, resistance_count, num_tests, resistance_rate)
  }
}

readr::write_rds(moran_data, "./data/dashboard_data/spatial_autocorrelation_data.Rds")
