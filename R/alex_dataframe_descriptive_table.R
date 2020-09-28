#' alex_dataframe_descriptive_table
#'
#' @description
#' Reads a data frame and reports descriptive statistics
#' (n, mean, standard deviation, minimum, first quartile, median,
#' third quartile, maximum) for all members of the data frame that are either
#' numeric or logical. The thinking engine is motivated by EpanDar package.
#' But this function is improved here in performance and functionality.
#' This function is created by Alexander Shemetev.
#' @param df Data frame containing at least one variable that is either numeric
#'   or logical and at least two observations.
#' @param digitsx Number of decimal digits that you want to be displayed for each
#'   column. If you provide NA, then the column is omitted from the output.
#'   By default these are 2 digits after the dot: digitsx = c(0, 2, 2, 2, 2, 2, 2, 2)
#' @param format character scalar that is handed over to \code{\link[knitr]{kable}}
#'   (e.g., "html" or "latex").
#' @param my_path Path where to save your data to in format like:
#' C:/Users/Alex/Documents/NewRPackage/alexandershemetev;
#' format C:/Users/Alex/Documents/NewRPackage/alexandershemetev/ should also be processed
#'  by default, this is your current working directory
#'  @param my_file is the name of the output .html file your want to save your
#'  descriptive table to
#'  by default, the name of the file is: my_html_descr_table.html.html
#'  (the first .html is for windows users whose OS doesn't provide extension obviously
#'  they could easily work with the file in their environment)
#' @param showhtmlinR TRUE/FALSE (by default it is FALSE); if TRUE it will show the HTML code
#' of this table for inputting in HTML page in some web-development environment.
#' This function is mostly designed for the web-developers they could easily put these tables
#' to their web-pages inside some more complicated design page.
#' @param showdf TRUE/FALSE (by default it is FALSE) - if it should save your
#' descriptive table data to dataframe to your global environment
#' @param save_excel TRUE/FALSE (by default it is TRUE) - if your descriptive table data
#' should be saved in excel format to work with it in the future.
#' @param open_in_browser TRUE/FALSE (by default it is TRUE); if TRUE it will open in browser
#' in your local environment the descriptive statistics of your dataframe for further work with.
#' @return A list containing two items in global environment.
#' \describe{
#'  \item{"df"}{A data frame containing the descriptive table}
#'  \item{"kable_ret"}{The return value provided by \code{\link[knitr]{kable}} containing the formatted table}
#' }
#' Also it returns a dataframe containing the descriptive table if requested by the user
#' It can also return html file and/or output containing the descriptive table if requested by the user
#' And it can also create Excel file output containing the descriptive table if requested by the user
#' The function will be saying what it is doing in your R environment - all returns will be said. 
#' @examples alex_dataframe_descriptive_table(mtcars)
#' @examples 
#' fcm <-c(14.0,14.1,13.0,14,2,14.7,13.8,14.0)
#' gk  <-c(12.1,12.5,12.2,12,0,11.5,12.0,11.4)
#' gg  <-c(14.0,14.1,13,3,12.8,12.0,12.2,12.0)
#' fcm1 <-c(14.0,14.1,13.0,14,2,14.7,13.8,14.0)
#' gk1  <-c(12.1,12.5,12.2,12,0,11.5,12.0,11.4)
#' gg1  <-c(14.0,14.1,13,3,12.8,12.0,12.2,12.0)
#' fcm2 <-c(14.0,14.1,13.0,14,2,14.7,13.8,14.0)
#' gk2  <-c(12.1,12.5,12.2,12,0,11.5,12.0,11.4)
#' gg2  <-c(14.0,14.1,13,3,12.8,12.0,12.2,12.0)
#' data1 <- rbind(fcm,gk,gg, fcm1,gk1,gg1,fcm2,gk2,gg2)
#' colnames(data1) <- c(6:13)
#' alex_dataframe_descriptive_table(data1)
#' @note The function uses "knitr", "plyr", "dplyr", "kableExtra", "htmltools", "utils", "pander", "writexl" libraries which will be installed and/or opened in your R environment 
#' even if they are not installed yet. This process is automatic (but user may be asked to select a mirror to download absent packages, if any). 
#' t1_12_28_90 - is the basic name of the returned list and t1_12_28_90_df is the basic name of the returned dataframe. 
#' Everything returned or created into the system is also said in R environment by this function. 



alex_dataframe_descriptive_table <- function(df, digitsx = c(0, 2, 2, 2, 2, 2, 2, 2), format = "html", my_path = getwd(), my_file = "my_html_descr_table.html", showhtmlinR = FALSE, open_in_browser = TRUE, showdf = FALSE, save_excel = TRUE)
{

#packages (might need to install some) - THIS FUNCTION INSTALLS ALL MAIN PACKAGES IF YOU DID NOT INSTALL SOME, and LOAD LIBRARIES THAT ARE INTALLED YET OR WILL BE INSTALLED AUTOMATICALLY BY THIS FUNCTION IN THIS CELL
install_and_load = function(name, char = T)
{
# Knittr library is needed to run this command - call the library or install it if there is no such library installed at all 
  if (!require(name, character.only = char)) 
  {
    install.packages(name)
  }
  require(name, character.only = char)
}

sapply(
  c("knitr", "plyr", "dplyr", "kableExtra", "htmltools", "utils", "pander", "writexl"),
  install_and_load
)
rm(install_and_load) #clean the R environment from the unnecessary anymore function 
digitsx <<- digitsx
print(paste("Library knitr is prepared; Starting to process your data: ", sep = ""))
### Thanks to ExpanDar whose function prepare_descriptive_table (here it is modified) motivated me to write this function :)
prepare_descriptive_table <- function(df, digits, format = "html") {
  digits = digitsx
  if(!is.data.frame(df)) stop("df needs to be a dataframe")
  df <- as.data.frame(df)
  df <- df[sapply(df, is.logical) | sapply(df, is.numeric)]
  if ((ncol(df) < 1) | (nrow(df) < 2)) stop("insuitable data frame (does it contain numerical data?)")
  if (!is.numeric(digits) | length(digits) != 8) stop("digits vector is not numeric or has wrong length (!= 8)")

  t <- cbind(apply(df,2,function(x) as.integer(sum(!is.na(x)))),
             apply(df,2,mean, na.rm=TRUE),
             apply(df,2,stats::sd, na.rm=TRUE),
             t(apply(df, 2, function(x) stats::quantile(x, probs=c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE))))
  colnames(t) <- c("Nobs", "Mean", "Std.dev.", "Min.", "25 %", "Median", "75 %", "Max.")
  t <- as.data.frame(t)
  t$Nobs <- as.integer(t$Nobs)
  t <- t[which(!is.na(digits))]
  digits <- digits[!is.na(digits)]
  list(df = t, kable_ret = knitr::kable(t, format, digits,
                                        format.args = list(big.mark = ","),
                                        caption = "Descriptive Statistics"))
										
										
										
}

df <- as.data.frame(df)
t1_12_28_90 <<- prepare_descriptive_table(df)

print(paste("Your final output knitr_kable object name is t1_12_28_90 in your global R environment - you may use it later: ", sep = ""))


# t1_12_28_90$kable_ret  %>%  kableExtra::kable_styling("condensed", full_width = F, position = "center")
my_path = my_path
my_file = my_file
# my_file1 = lapply(my_path, function(col_) {gsub( "/.$","",col_)} )
my_path <<- lapply(my_path, function(col_) {gsub( "\\/$","",col_)} ) #In case if user would input additional / in the file path - remove it

# htmltools::save_html((t1_12_28_90$kable_ret  %>%  kableExtra::kable_styling("condensed", full_width = F, position = "center")), paste(my_path, "/", my_file, sep = ""), background = "white", libdir = "lib")
# htmltools::save_html(paste("<!DOCTYPE html><html> <head> <META HTTP-EQUIV=\"REFRESH\" CONTENT=\"1; URL=www.finsoft.systems\"> <title>Descriptive Table</title> </head> <body>", (t1_12_28_90$kable_ret  %>%  kableExtra::kable_styling("condensed", full_width = F, position = "center")), "</body> </html>", sep = ""), paste(my_path, "/", my_file, sep = ""), background = "white", libdir = "lib")
write.table(paste("<!DOCTYPE html><html> <head> <META HTTP-EQUIV=\"REFRESH\" CONTENT=\"1; URL=www.finsoft.systems\"> <title>Descriptive Table</title> </head> <body>", (t1_12_28_90$kable_ret  %>%  kableExtra::kable_styling("condensed", full_width = F, position = "center")), "</body> </html>", sep = ""),file=paste0(my_path, "/", my_file,".html"),sep='')
print(paste("Your final .html file with descriptive statistics is saved to: ", my_path, "/", my_file,".html", sep = ""))

# openFileInOS(paste(my_path, "/", my_file,".html", sep = "")) #Buggy function from library  "pander" 

if (showhtmlinR > 0) #Logical in R TRUE = 1; FALSE = 0 
{
t1_12_28_90$kable_ret  %>%  kableExtra::kable_styling("condensed", full_width = F, position = "center")
}else 
{
# Do nothing if showhtmlinR is not set to TRUE
}

if (open_in_browser > 0) 
{
print(paste("Trying opening your output descriptive statistics data in your browser from the saved file to your system: ", sep = ""))

openFileInOS(paste(my_path, "/", my_file,".html", sep = ""))
}
else 
{
# Do nothing - User did not ask to open anything in the browser
}
if (showdf > 0)
{
t1_12_28_90_df <<- t1_12_28_90$df
print(paste("The new dataframe object t1_12_28_90_df with your descriptive table is set to your R global environment; it looks like this: ", sep = ""))
t1_12_28_90$df
}
else 
{
# Do nothing
}
if (save_excel > 0)
{
t1_12_28_90_df <- t1_12_28_90$df
# write_xlsx(list(t1_12_28_90_df), paste("C:/Users/Alex/Documents/CERGE-EI study/QUANTITATIVE METHODS 2/PROJECT TO PAVLA/PAVLE_WB_PWT_IMF_newest.xlsx"), sep = "")
my_file = my_file
my_file = lapply(my_file, function(col_) {gsub( "\\.","",col_)} )
#  file_test file_test("-f", input.good) # returns TRUE
# file_for_test =  file_test(paste0(my_path, "/", my_file,".xlsx"))
if(file.exists(paste0(my_path, "/", my_file,".xlsx")))
{

print(paste("As requested by the user (same is by default), Excel file would be created if no such file would exist at: ", my_path, "/", my_file,".xlsx", ". Please use the file you have or delete the previous version of the file manually (R suggests maybe some important info may be in your file - that is why it was not removed).", sep = ""))

}
else
{
t1_12_28_90_df <- t1_12_28_90$df

my_file = my_file
my_file = lapply(my_file, function(col_) {gsub( "\\.","",col_)} )

write_xlsx(list(t1_12_28_90_df), paste0(my_path, "/", my_file,".xlsx"))

print(paste("As requested by the user (same is by default), Excel file is created with descriptive table output at: ", my_path, "/", my_file,".xlsx", sep = ""))
}
}
else 
{
# Do nothing
}

rm(my_path, digitsx)

}

