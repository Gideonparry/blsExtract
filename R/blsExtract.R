#' Returns and saves file for BLS data of current month for inds
#'
#' Takes year for BLS data and returns the values of the different groups for
#' the BLS employment series file. Requires blsAPI and rjson packages
#'
#' @param file the file to return winter_wind from
#' @param year a string for the year to obtain the value
#' @param var2 2nd wind variable to use
#'
#' @importFrom blsAPI blsAPI
#' @importFrom rjson fromJSON
#' @export

blsExtract <- function(year, file_path = "INVALID FILE PATH",
                       create_csv = TRUE) {
  ## One or More Series, Specifying Years
  if(create_csv){
    if(!(dir.exists(file.path(file_path)))){
      stop("Enter a valid file path or set create_csv = FALSE")
    }

  }
  payload <- list("seriesid"=c("CES3200000001","CES3231100001", "CES3232900001",
                               "CES3231300001", "CES3231400001", "CES3231500001",
                               "CES3232200001", "CES3232300001", "CES3232400001",
                               "CES3232500001", "CES3232600001", "CES3100000001",
                               "CES3132100001", "CES3132700001", "CES3133100001",
                               "CES3133200001", "CES3133300001", "CES3133400001",
                               "CES3133500001", "CES3133600001", "CES3133700001",
                               "CES3133900001"),
                  "startyear" = year, "endyear" = year)
  response <- blsAPI::blsAPI(payload)
  if(grepl("No Data Available for Series", response)){
    stop("Please enter a valid year containing data")
  }
  json <- rjson::fromJSON(response)


  values <- c()
  for (i in 1:22){
    values[i] <- as.numeric(json$Results$series[[i]]$data[[1]]$value)
  }
  vals_data <- as.data.frame(matrix(values, 2, 11, byrow = TRUE)[2:1,])
  rownames(vals_data) = c("dg", "ndg")
  if(create_csv){
    write.csv(vals_data, paste(file_path, "new_vals.csv", sep = "/"))
  }
  vals_data

}
