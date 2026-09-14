
# SESSION 01 --------------------

# Strings
new_names <- c('Benta', 'Kariuki', 'Mwangi', 'Kipchoge', 'Ruto')
new_height <- c(196, 189, 167, 180, 201)

# Lists
benta_data <- list(new_names, new_height)   # function(to these guys)

# Data frames
new_data_2 <- data.frame(benta_data)

# Combination of all of the above
new_data_3 <- data.frame(new_names = c('Benta', 'Kariuki', 'Mwangi', 'Kipchoge', 'Ruto'), 
                         new_height = c(196, 189, 167, 180, 201))


# Create a
# 01. List of Mount Kenya subcounties, population size, year of that data.
# 02. Convert it to a data frame
# 03. Create a data frame directly, this time add another variable of popular names (one for each subcounty)