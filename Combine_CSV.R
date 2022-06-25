## Packages Used
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(dplyr)
library(cvequality)
library(snpar)
library(mixtools)
library(stringr)


## Functions
## CV
cv = function(x){
  return(100* sd(x)/mean(x))
}

## CVQ
cvq = function(x){
  qs = quantile(x, probs = c(0.25,0.75))
  return(100*(qs[2] - qs[1])/(qs[2] + qs[1]))
}

## Load data
grp = "<Insert Treatment Label Here>"
nt_data = NULL
file_path = paste0("<Insert Filepath Here>")
## List of CSV files
file_list = list.files(path = file_path, pattern="*.csv")

## Assemble full dataset
for(i in 1:length(file_list)){
  ## Check if CSV is empty
  if(length(count.fields(paste0(file_path, "/", file_list[i]))) > 1){
    
    temp_data = read.csv(paste0(file_path, "/", file_list[i]))
    temp_data$condition = grp
    temp_data$exp = substr(file_list[i], 18, 18)
    temp_data = data.frame(image = substr(file_list[i], 20, 20), temp_data)
    nt_data = rbind(nt_data, temp_data)
  }
}

## write to excel
library(writexl)
write_xlsx(nt_data, "<Insert Filepath To Empty Excel File>")


