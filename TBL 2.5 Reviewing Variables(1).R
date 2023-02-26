names(housing.df)# print a list of variables to the screen
t(t(names(housing.df))) # print the list in a useful column format
colnames(housing.df)[1] <- c("TOTAL_VALUE") # change the fist column name

colnames(housing.df)[1] <- c("TOTAL.VALUE") # Rechange the fist column name

class(housing.df$REMODEL) # CHECK REMODEL is a factor variable
class(housing.df[,14]) # Same

levels(housing.df[,14]) # CHECK It takes one of three levels
class(housing.df$BEDROOMS) # BEDROOMS is an integer variable
class(housing.df[,1]) # ToTAL.VALUE is a numeric variable

# How to convert the class of a variable? e.g., from "factor" to "character" and vice-versa

housing.df[,14] <- as.factor(housing.df$REMODEL)
