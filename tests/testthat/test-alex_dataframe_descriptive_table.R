# alex_dataframe_descriptive_table
if (require(alexandershemetev)){
fcm <-c(14.0,14.1,13.0,14,2,14.7,13.8,14.0)
gk  <-c(12.1,12.5,12.2,12,0,11.5,12.0,11.4)
gg  <-c(14.0,14.1,13,3,12.8,12.0,12.2,12.0)
fcm1 <-c(14.0,14.1,13.0,14,2,14.7,13.8,14.0)
gk1  <-c(12.1,12.5,12.2,12,0,11.5,12.0,11.4)
gg1  <-c(14.0,14.1,13,3,12.8,12.0,12.2,12.0)
fcm2 <-c(14.0,14.1,13.0,14,2,14.7,13.8,14.0)
gk2  <-c(12.1,12.5,12.2,12,0,11.5,12.0,11.4)
gg2  <-c(14.0,14.1,13,3,12.8,12.0,12.2,12.0)
data1 <- rbind(fcm,gk,gg, fcm1,gk1,gg1,fcm2,gk2,gg2)
colnames(data1) <- c(6:13)
alex_dataframe_descriptive_table(data1, open_in_browser = FALSE, save_excel = FALSE, showdf = TRUE)
mrsk = mean(t1_12_28_90_df$Nobs)
expect_equal(mrsk, 9)
}



