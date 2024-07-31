# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

blsExtract <- function(year, file_path) {
  ## One or More Series, Specifying Years
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
  json <- rjson::fromJSON(response)


  values <- c()
  for (i in 1:22){
    values[i] <- as.numeric(json$Results$series[[i]]$data[[1]]$value)
  }
  vals_dat <- as.data.frame(matrix(values, 2, 11, byrow = TRUE)[2:1,])
  rownames(vals_dat) = c("dg", "ndg")
  write.csv(vals_dat, paste(file_path, "new_vals.csv", sep = "/"))
  vals_dat

}
