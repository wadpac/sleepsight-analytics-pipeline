#' check_personfolder
#'
#' @param personfolder personfolder
#' @return No output
#' @export
check_personfolder = function(personfolder) {
  folder = rep(0,3)
  folder[1] = paste0(personfolder,"/Phone_sensors")
  folder[2] = paste0(personfolder,"/Sleep_diary")
  folder[3] = paste0(personfolder,"/Withings-data")
  folder_nm = c("Phone_sensors","Sleep_diary","Withings-data")
  skip = 0; n = c()
  for (jj in 1:3) {
    if (!dir.exists(folder[jj])) {
      if (skip == 0) {
        n <- readline(prompt=paste0(folder_nm," folder does not exist. Create folder? (y)es | (n)o | yes to (a)ll:"))
      }
      if (n == "y" | n == "a") {
        dir.create(folder[jj])
        if (n == "a") skip = 1
      }
    }
    if (length(dir(folder[jj])) == 0) {
      if (jj == 1) {
        warning(paste0("\nNo data found inside ",folder[jj],". Move your Phone sensor data there, if it exists."))
      } else if (jj == 2) {
        warning(paste0("\nNo data found inside ",folder[jj],". Move your Survey data there, if it exists."))
      } else if (jj == 3) {
        warning(paste0("\nNo data found inside ",folder[jj],". Move your Withings data there, if it exists."))
      }
    }
  }
}