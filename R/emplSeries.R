#' Returns and saves file for BLS data of current month for inds
#'
#' Takes year for BLS data and returns the values of the different groups for
#' the BLS employment series file. Requires blsAPI and rjson packages
#'
#' @param year the year to obtain most recent monthly employment data from
#' @param file_path the file path to save the csv file to
#' @param num_months The number of months to go back
#' @param create_csv TRUE to create a CSV file, FALSE to not create one
#'
#' @importFrom blsAPI blsAPI
#' @importFrom rjson fromJSON
#' @export

emplSeries <- function(year, file_path = "INVALID FILE PATH",
                       num_months = 1,
                       create_csv = TRUE) {
  ## One or More Series, Specifying Years
  if(create_csv){
    if(!(dir.exists(file.path(file_path)))){
      stop("Enter a valid file path or set create_csv = FALSE")
    }

  }
  payload <- list("seriesid" = c("CES3200000001","CES3231100001",
                                 "CES3232900001", "CES3231300001",
                                 "CES3231400001", "CES3231500001",
                                 "CES3232200001", "CES3232300001",
                                 "CES3232400001", "CES3232500001",
                                 "CES3232600001", "CES3100000001",
                                 "CES3132100001", "CES3132700001",
                                 "CES3133100001", "CES3133200001",
                                 "CES3133300001", "CES3133400001",
                                 "CES3133500001", "CES3133600001",
                                 "CES3133700001", "CES3133900001"),
                  "startyear" = year, "endyear" = year)
  response <- blsAPI::blsAPI(payload)
  if(grepl("No Data Available for Series", response)){
    stop("Please enter a valid year containing data")
  }
  json <- rjson::fromJSON(response)

  ## empty matrix to store all the values
  values <- c()
  ## For each of the 22 columns, we grab the specified months of data
  for (i in 1:22){
    onevals <- c()
    for (j in 1:num_months){
      onevals[j] <- as.numeric(json$Results$series[[i]]$data[[j]]$value)
    }
    val1 <- num_months*(i-1) + 1
    valn <- num_months*i
    values[val1:valn] <- onevals[num_months:1]
  }

  ## Order was weird, reordering columns here
  vals_mat <- matrix(values, num_months, 22, byrow = FALSE)[,c(12:22, 1:11)]

  if(create_csv){
    if(num_months > 1){
      vals_dat1 = vals_mat[,1:11]
      write.csv(vals_dat1, paste(file_path, "empl_dg.csv",
                                 sep = "/"))

      vals_dat2 = vals_mat[,12:22]
      write.csv(vals_dat2, paste(file_path, "empl_ndg.csv",
                                 sep = "/"))

    }

    if(num_months == 1){
      vals_dat1 = vals_mat[1:11]
      write.csv(t(vals_dat1), paste(file_path, "empl_dg.csv",
                                 sep = "/"))

      vals_dat2 = vals_mat[12:22]
      write.csv(t(vals_dat2), paste(file_path, "empl_ndg.csv",
                                 sep = "/"))

    }


  }
  vals_mat

}
