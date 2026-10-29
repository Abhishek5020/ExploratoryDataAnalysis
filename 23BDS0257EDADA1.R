#Question 1: Data Frame Operations
# -------------------------------
# DATAFRAME 1
# -------------------------------
dataframe1 <- data.frame(
  ProductID = c(101,102,103,104,105,106,107,108,109,110),
  NameofProd = c("Laptop","Mobile","Keyboard","Mouse","Monitor",
                 "Printer","Speaker","Headphone","Tablet","Camera"),
  Price = c(55000,25000,1500,800,12000,9000,2500,1800,30000,45000)
)

# -------------------------------
# DATAFRAME 2
# (Some ProductIDs are same, some different)
# -------------------------------
dataframe2 <- data.frame(
  ProductID = c(101,102,103,104,105,111,112,113,114,115),
  Company_Location = c("Delhi","Mumbai","Bangalore","Chennai","Hyderabad",
                       "Pune","Kolkata","Ahmedabad","Jaipur","Lucknow")
)

# -------------------------------
# DATAFRAME 3
# -------------------------------
dataframe3 <- data.frame(
  ProductID = c(111,112,113,114,115,116,117,118,119,120),
  NameofProd = c("SSD","HardDisk","Router","Pendrive","Webcam",
                 "Microphone","Projector","Scanner","SmartWatch","PowerBank"),
  Price = c(4500,5500,2500,700,2200,3500,40000,8500,12000,1500)
)

# Display DataFrames
print(dataframe1)
print(dataframe2)
print(dataframe3)

# ----------------------------------------------------
# 1. Append dataframe1 and dataframe3
# ----------------------------------------------------
append_df <- rbind(dataframe1, dataframe3)

cat("\nAppended DataFrame:\n")
print(append_df)

# ----------------------------------------------------
# 2. Concatenate DataFrames
# ----------------------------------------------------
concat_df <- cbind(dataframe1,
                   Company_Location = c("Delhi","Mumbai","Bangalore",
                                        "Chennai","Hyderabad","Pune",
                                        "Kolkata","Ahmedabad",
                                        "Jaipur","Lucknow"))

cat("\nConcatenated DataFrame:\n")
print(concat_df)

# ----------------------------------------------------
# 3. Merge dataframe1 and dataframe2
# ----------------------------------------------------
merged_df <- merge(dataframe1, dataframe2, by = "ProductID")

cat("\nMerged DataFrame:\n")
print(merged_df)

# ----------------------------------------------------
# 4. Join Operations
# ----------------------------------------------------

# Inner Join
inner_join <- merge(dataframe1, dataframe2, by = "ProductID")

cat("\nInner Join:\n")
print(inner_join)

# Left Join
left_join <- merge(dataframe1, dataframe2,
                   by = "ProductID",
                   all.x = TRUE)

cat("\nLeft Join:\n")
print(left_join)

# Right Join
right_join <- merge(dataframe1, dataframe2,
                    by = "ProductID",
                    all.y = TRUE)

cat("\nRight Join:\n")
print(right_join)

# Full Outer Join
full_join <- merge(dataframe1, dataframe2,
                   by = "ProductID",
                   all = TRUE)

cat("\nFull Outer Join:\n")
print(full_join)

#Question 2: Data Transformations
# -----------------------------------
# Numeric Vector
# Object Name = Abhishek
# Registration Number Last 4 Digits = 0257
# -----------------------------------

Abhishek <- c(0257, 845, 1234, 678, 4321, 987, 2456, 7890, 1357, 2468)

cat("Original Vector:\n")
print(Abhishek)

# -----------------------------------
# Function for Min-Max Normalization
# -----------------------------------

normalize <- function(x, new_min, new_max)
{
  ((x - min(x)) / (max(x) - min(x))) *
    (new_max - new_min) + new_min
}

# -----------------------------------
# 1. Normalize to [-3, +3]
# -----------------------------------

norm1 <- normalize(Abhishek, -3, 3)

cat("\nNormalization [-3,3]:\n")
print(norm1)

# -----------------------------------
# Normalize to [-1, +1]
# -----------------------------------

norm2 <- normalize(Abhishek, -1, 1)

cat("\nNormalization [-1,1]:\n")
print(norm2)

# -----------------------------------
# Normalize to [0,1]
# -----------------------------------

norm3 <- normalize(Abhishek, 0, 1)

cat("\nMin-Max Normalization [0,1]:\n")
print(norm3)

# -----------------------------------
# 2. Z-Score Normalization
# -----------------------------------

zscore <- (Abhishek - mean(Abhishek)) / sd(Abhishek)

cat("\nZ-Score Transformation:\n")
print(zscore)

# -----------------------------------
# 3. Decimal Scaling Normalization
# -----------------------------------

j <- nchar(as.character(max(abs(Abhishek))))
decimal_scaling <- Abhishek / (10^j)

cat("\nDecimal Scaling Normalization:\n")
print(decimal_scaling)