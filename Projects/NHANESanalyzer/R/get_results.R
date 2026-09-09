#' Create a data frame object from the data set
#' @return data frame with the data set data
#' @export
#' @examples
#' get_data()

get_data <- function() {as.data.frame(bmx)}
#--------------------------------------------------------------------------------------------------------

#We create a "dictionary" of the human-readable names of each measurement type as keys and assign
#them to their corresponding column name as values.


variables <- list("recumbent length" = c("BMXRECUM", "BMIRECUM"),
                  "head circumference" = c("BMXHEAD","BMIHEAD"),
                  "height" = c("BMXHT", "BMIHT"), "BMI" = c("BMXBMI"),
                  "upper leg length" = c("BMXLEG", "BMILEG"),
                  "upper arm length" = c("BMXARML", "BMIARML"),
                  "arm circumference" = c("BMXARMC", "BMIARMC"),
                  "waist circumference" = c("BMXWAIST", "BMIWAIST"),
                  "hip circumference" = c("BMXHIP", "BMIHIP"))

#' Get names for the variables in the data frame
#'
#' @return a list of strings that represent the human-readable version of the variables in the data frame
#' @export
#' @examples
#' var_names()

var_names <- function(){
  names(variables)
}
#-------------------------------------------------------------------------------------------------------

#Now we can use variables to properly index the dataframe while still letting the user
#have human-readable and intuitive inputs

#Next up, we create a helper function to provide summaries of a single statistic:

#' Helper function to gather info about a single variable of a specified ID
#'
#' @param id the ID that we want to search information for (integer)
#' @param name the name of the variable we are gathering the data of (string)
#' @param df the data frame
#'
#' @return a string with the value of the chosen variable for the chosen ID and a comment if there is. Warning if no data is found or variable does not exist
#' @export
#'
#' @examples
#' df <- get_data()
#' get_info(130378, "recumbent length", df)

get_info <- function(id, name, df){
  if (!(name %in% var_names())){
    return(paste0("The variable ('", name, "') is not in the data set. Use the function var_names()
                 to look at all the possible variables."))
  }
  comment_str <- ""
  if (length(variables[[name]])==2){
    info <- df[df$SEQN == id, variables[[name]][1]]
    comment <- df[df$SEQN == id, variables[[name]][2], drop=TRUE]
    if(is.na(info)){
      return(paste0('No data recorded for variable (', name, ').'))
    }
    if (!(is.na(comment))){
      comment_str <- paste0(" Comment: ", comment, ".")
    }
    return(paste0(name, ": ",info, ".", comment_str))
  }
  else{
    return(paste0(name, ": ", df[df$SEQN == id, variables[[name]][1]]))
  }
}
#--------------------------------------------------------------------------------------------------

#Now that we have this helper function ready to go, we can create the function
#that will give us the summary of the results for the desired person:

#' Get info for all variables or chosen variables for a single ID
#'
#' @param id the ID that we want to search information for (integer)
#' @param df the data frame
#' @param requests the names of the variables we want to get info for. If null, get info for all variables (string vector)
#'
#' @return a string with the information for the chosen variables. Warning if ID not found
#' @export
#'
#' @examples
#' df <- get_data()
#' nhanes_results(130378, df)
#' nhanes_results(130378, df, c("recumbent length", "BMI"))

nhanes_results <- function(id, df, requests=NULL){
  if (!(id %in% df[["SEQN"]])){
    return("This ID is not registered in the data.")
  }
  result <- ""
  if (is.null(requests)){
    requests <- var_names()
  }
  for (name in requests){
    result<- paste0(result, get_info(id, name, df), "\n")
  }
  return(result)
}
#-------------------------------------------------------------------------------------------------

#Get the ratio between different statistics of a person

#' Get get ratio between two chosen variables for a single ID
#'
#' @param id the ID that we want to search information for (integer)
#' @param df the data frame
#' @param v1 name of one value for the ratio (string)
#' @param v2 name of another value for the ratio (string)
#'
#' @return a string showcasing the information about the desired ratio. Warning if ID or variables not in the data set
#' @export
#'
#' @examples
#' df <- get_data()
#' get_ratio(130378, df, "waist circumference", "hip circumference")

get_ratio <- function(id, df, v1, v2){
  if (!(id %in% df[["SEQN"]])){
    return("This ID is not registered in the data.")
  }
  if (!(v1 %in% var_names()) || !(v2 %in% var_names())){
    return(paste0("One or more of the variables you requested are not in the data set. Use the function var_names()
                 to look at all the possible variables."))
  }
  if (!(id %in% df[["SEQN"]])){
    return("This ID is not registered in the data.")
  }
  value1 <- df[df$SEQN == id, variables[[v1]][1]]
  value2 <- df[df$SEQN == id, variables[[v2]][1]]
  if (!(is.na(value1)) && !(is.na(value2))){
    ratio <- value1/value2
    return(paste0("Your ratio between ", v1, " and ", v2, " is ", ratio))
  }
  else{
    return("One or more values could not be found for this ID.")
  }
}

#------------------------------------------------------------------------------------------------

#Create another "dictionary" for the averages in the U.S. in cm (both genders averaged)

averages <- list("recumbent length" = 49.5,
                 "head circumference" = 55.5,
                 "height" = 169, "BMI" = 29.2,
                 "upper leg length" = 39.25,
                 "upper arm length" = 37.45,
                 "arm circumference" = 30.25,
                 "waist circumference" = 100,
                 "hip circumference" = 101)

#' Show national average values for the variables in the data frame
#'
#' @return a string showcasing the national averages for every variable in the data frame (male and female combined)
#' @export
#'
#' @examples
#' var_averages()

var_averages <- function(){
  print(averages)
}

#------------------------------------------------------------------------------------------------

#Compare any person's data with the averages

#' Compare a single ID's value for one variable to the national average (male and female combined)
#'
#' @param id the ID that we want to search information for (integer)
#' @param df the data frame
#' @param name name of the variable to be compared with its average (string)
#'
#' @return a string showcasing the comparison. Warning if ID or variable not in the data set
#' @export
#'
#' @examples
#' df <- get_data()
#' compare(130378, df, "BMI")

compare <- function(id, df, name){
  if (!(id %in% df[["SEQN"]])){
    return("This ID is not registered in the data.")
  }
  if (!(name %in% var_names())){
    return(paste0("The variable ('", name, "') is not in the data set. Use the function var_names()
                 to look at all the possible variables."))
  }
  value <- df[df$SEQN == id, variables[[name]][1]]
  if (is.na(value)){
    return(paste0("The value of ('", name, "') is not recorded for this ID."))
  }
  difference <- value - averages[[name]]
  if (difference > 0){
    return(paste0("Your ", name, " is above average by a difference of ", abs(difference), "."))
  }
  else if (difference < 0){
    return(paste0("Your ", name, " is below average by a difference of ", abs(difference), "."))
  }
  else{
    return(paste0("Your ", name, " is right on the average!"))
  }
}
