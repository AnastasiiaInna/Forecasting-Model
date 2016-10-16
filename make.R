source.dir <- "~/Documents/"
setwd(source.dir)

if(!require(plyr)) {
  install.packages("plyr"); require(plyr)}
if(!require(dplyr)) {
  install.packages("dplyr"); require(dplyr)} 
if(!require(ggplot2)) {
  install.packages("ggplot2"); require(ggplot2)}
if(!require(scales)) {
  install.packages("scales"); require(scales)}
if(!require(reshape)) {
  install.packages("reshape"); require(reshape)}
if(!require(reshape2)) {
  install.packages("reshape2"); require(reshape2)}
if(!require(lubridate)) {
  install.packages("lubridate"); require(lubridate)} 
if(!require(forecast)) {
  install.packages("forecast"); require(forecast)}
if(!require(zoo)) {
  install.packages("zoo"); require(zoo)}
if(!require(sqldf)) {
  install.packages("sqldf"); require(sqldf)}

source(paste0(source.dir, "ParametersInput.R"))
source(paste0(source.dir, "EvaluationOutput.R"))
source(paste0(source.dir, "FinalForecast.R"))