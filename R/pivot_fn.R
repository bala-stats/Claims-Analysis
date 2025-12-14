
# aa= data_convert_fn(Ipdata, "ClmDiagnosisCode_" )
# bb = data_convert_fn(Ipdata, "ClmProcedureCode_" )
# bb = data_convert_fn(Ipdata, "DiagnosisGroupCode" )

data_convert_fn <- function(data, word = "ClmDiagnosisCode_") {

df <- Ipdata %>% select(Provider, starts_with(word))

# Step 1: Combine all codes per row
df$all_codes <- apply(df[, -1], 1, function(x) x[!is.na(x)])

# Step 2: Aggregate all codes by provider
merged <- tapply(df$all_codes, df$Provider, function(x) unique(unlist(x)))

# Step 3: Find all unique codes in dataset
all_codes <- sort(unique(unlist(merged)))

# Step 4: Create binary matrix manually
binary_mat <- t(sapply(merged, function(x) as.integer(all_codes %in% x)))

# Step 5: Combine with patient_id
df_binary <- data.frame(
  Provider = rownames(binary_mat),
  binary_mat,
  row.names = NULL
)
names(df_binary)[-1] <- all_codes

return(df_binary)

}
