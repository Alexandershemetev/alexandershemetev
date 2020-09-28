#' alex_clean()
#' Keeping your environment free from unnecessary files is very important in your current work. Large files or large number of objects may distract the memory of your PC and your own attention from really important tasks. Sometimes, sinse R has no inbuilt memory controller, R may run out of memory. To avoid this – you need to keep your R environment clean. This function allow you not to memorise the ways for cleaning the R memory and writing long codes for that – it is short and effective. One object name is marked as undeletable with this function in case if you use this function often and would like to type short alex_clean() all the time without more complicated inputs – you may keep 1 object named  ca1122334455667788990011 in advance (before using this function) – your R environment should save this object for the future.
#'
#' alex_clean() cleans either entire R global environment from all objects,
#' Or saves specific objects aither by name or by pattern in name or both
#' Easy way to clean the R environment from unnecessary objects and free RAM. This function is first designed by Alexander Shemetev partially inspired by maaniB. .
#'
#' @param except - name(s) of objects to preserve in R
#' @param pattern - pattern in names of objects in R environment to be preserved
#' @return A cleaned R environment with everything removed and selected objects preserved only (if any were asked to be preserved).
#' @note Object having pattern in its name: "ca1122334455667788990011" cannot be removed with this function (if such object exists in your R environment at all). This name is reserved for undeletable object.
#' @importFrom R global environment
#' @export to R global environment
#' @examples
#' alex_clean() #cleans everything in R global environment
#' a = 5; b = 6; c1 = a + b; df1 = c(1, 2, 3, 4); gf1 = c(8,9,6,8); ls()
#' alex_clean(except = c("a", "b", "c", "d"), pattern = "df")
#' ls() #Your environment has only selected by user files preserved
#' alex_clean(pattern = "df")  #only object(s) with pattern df in name are preserved in R environment



alex_clean <- function(except = "ca1122334455667788990011", pattern = "ca1122334455667788990011")
{
  print(paste("The full name of object in your R environment before request for erasing was: ", ls(envir = .GlobalEnv), sep = ""))
  if(except == "ca1122334455667788990011" && pattern == "ca1122334455667788990011")
  {
    print(paste("Your Request to clear the entire R environment is received", sep = ""))
    rm(list = ls(envir = .GlobalEnv) , envir = .GlobalEnv )
  }
  else
  {
    except = except;
    pattern = pattern;
    formula = c(c(except), ls(pattern = pattern, envir = .GlobalEnv));
    rm(list = setdiff(ls(envir = .GlobalEnv), formula), envir = .GlobalEnv)
  }

  print(paste("The full name of preserved object in your R environment is: ", ls(envir = .GlobalEnv), sep = ""))

}

