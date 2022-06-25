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

## Load Non-Treatment data
grp = "NT"
nt_data = NULL
file_path = paste0(<Insert File Path Here>)

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


### Convert to microns
nt_data$area = (0.12581^2)*nt_data$area
nt_data$lengthEllipse = 0.12581*nt_data$lengthEllipse
nt_data$widthEllipse = 0.12581*nt_data$widthEllipse



## Load Treatment 1 data
grp = <Insert Treatment 1 Label Here>
hu_data = NULL
file_path = paste0(<Insert Treatment 1 CSV File Path Here>)
  
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
      hu_data = rbind(hu_data, temp_data)
    }
  }


### Convert to microns
hu_data$area = (0.12581^2)*hu_data$area
hu_data$lengthEllipse = 0.12581*hu_data$lengthEllipse
hu_data$widthEllipse = 0.12581*hu_data$widthEllipse


## Load Treatment 2 data
grp = <Insert Treatment 2 Label Here>
mms_data = NULL
file_path = paste0(<Insert Treatment 2 CSV File Path Here>)

## List of CSV files
file_list = list.files(path = file_path, pattern="*.csv")

## Assemble full dataset
for(i in 1:length(file_list)){
  ## Check if CSV is empty
  if(length(count.fields(paste0(file_path, "/", file_list[i]))) > 1){
    
    temp_data = read.csv(paste0(file_path, "/", file_list[i]))
    temp_data$condition = grp
    temp_data$exp = substr(file_list[i], 20, 20)
    temp_data = data.frame(image = substr(file_list[i], 22, 22), temp_data)
    mms_data = rbind(mms_data, temp_data)
  }
}


### Convert to microns
mms_data$area = (0.12581^2)*mms_data$area
mms_data$lengthEllipse = 0.12581*mms_data$lengthEllipse
mms_data$widthEllipse = 0.12581*mms_data$widthEllipse



## Load Treatment 3 data
grp = <Insert Treatment 3 Label Here>
uv_data = NULL
file_path = paste0(<Insert Treatment 3 CSV File Path Here>)

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
    uv_data = rbind(uv_data, temp_data)
  }
}


### Convert to microns
uv_data$area = (0.12581^2)*uv_data$area
uv_data$lengthEllipse = 0.12581*uv_data$lengthEllipse
uv_data$widthEllipse = 0.12581*uv_data$widthEllipse

## Length graph
full_all = rbind(nt_data, hu_data, uv_data, mms_data)
ap = ggplot(full_all, aes(x = area, color = condition)) + geom_density(lwd = 1.3, adjust = 1.4) + 
  theme_minimal() + labs(x = "Area", y = " ") + scale_x_continuous(limits = c(8,41))

lp = ggplot(full_all, aes(x = lengthEllipse, color = condition)) + geom_density(lwd = 1.3, adjust = 1.4) + 
  theme_minimal() + labs(x = "Length", y = " ") + scale_x_continuous(limits = c(3,18))

wp = ggplot(full_all, aes(x = widthEllipse, color = condition)) + geom_density(lwd = 1.3, adjust = 1.4) + 
  theme_minimal() + labs(x = "Width", y = " ") + scale_x_continuous(limits = c(1.5,6.75))

png(<Name of Image Here>, h=8, w=11, units = 'in', res = 300)
### Replace the wp for ap or lp when needed
grid.arrange(wp)
dev.off()
