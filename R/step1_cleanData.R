
# note on profileClean
# be careful with user profiles - some of them have just \n and dashes and spaces (especially those from non-AlphaBay --- make sure to get rid of these
# change times in profileClean
# profileClean$date <- as.Date(as.POSIXct(profileClean$date, origin = "1970-01-01"), "%m-%d-%Y")
#### CHECK: profileClean$profileClean: --- check how this was cleaned up before
