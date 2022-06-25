## Instructions to use:
# 1) Adjust file_path to the correct directory where the csv files are located.
# 2) Under Assemble full dataset, adjust the two variables where the first two are 
# the experiment variables and the last one is the control variable name.
# 3) Under Sets up color scheme, adjust the variable to which one should be
# blue or red.


## Packages Used
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(dplyr)
library(cvequality)
library(snpar)
library(mixtools)
library(stringr)
library(moments)

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

## Path where the EXP csvs are located
file_path = <Insert File Path Here>

## List of CSV files
file_list = list.files(path = file_path, pattern="*.csv")

## Assemble full dataset
uv_nt = NULL
for(i in 1:length(file_list)){
  ## Check if CSV is empty
  if(length(count.fields(paste0(file_path, "/", file_list[i]))) > 1){
    
    temp_data = read.csv(paste0(file_path, "/", file_list[i]))
    temp_data$condition = ifelse(str_detect(file_list[i], pattern = <"Insert Label of First Treatment Group">), 
                                 <"Insert Label of First Treatment Group">, <"Insert Label of Second Treatment Group">)
    temp_data = data.frame(image = i, temp_data)
    uv_nt = rbind(uv_nt, temp_data)
  }
}

### Convert to microns
uv_nt$area = (0.12581^2)*uv_nt$area
uv_nt$lengthEllipse = 0.12581*uv_nt$lengthEllipse
uv_nt$widthEllipse = 0.12581*uv_nt$widthEllipse

## Summary table (area)
area_sum = uv_nt %>% group_by(condition) %>% summarize(N = n(),
                                                       Avg_area = mean(area),
                                                       Med_area = median(area),
                                                       Q3_area = quantile(area, probs = 0.75),
                                                       P90_area = quantile(area, probs = 0.90),
                                                       Skewness_area = skewness(area),
                                                       CV_area = cv(area))

## Summary table (length)
length_sum = uv_nt %>% group_by(condition) %>% summarize(N = n(),
                                                         Avg_length = mean(lengthEllipse),
                                                         Med_length = median(lengthEllipse),
                                                         Q3_length = quantile(lengthEllipse, probs = 0.75),
                                                         P90_length = quantile(lengthEllipse, probs = 0.90),
                                                         Skewness_length = skewness(lengthEllipse),
                                                         CV_length = cv(lengthEllipse))

## Summary table (width)
width_sum = uv_nt %>% group_by(condition) %>% summarize(N = n(),
                                                        Avg_width = mean(widthEllipse),
                                                        Med_width = median(widthEllipse),
                                                        Q3_width = quantile(widthEllipse, probs = 0.75),
                                                        P90_width = quantile(widthEllipse, probs = 0.90),
                                                        Skewness_width = skewness(widthEllipse),
                                                        CV_width = cv(widthEllipse))

# # Sets up color scheme
# if(unique(uv_nt$condition)[2] == "T"){
#   ## If "treatment" is the second condition, make "red" the second color
#   colScale = scale_colour_manual(name = "condtion",values = c("red","blue"))
# } else {
#   ## Otherwise "treatment" is first condition, so make "red" the first color
#   colScale = scale_colour_manual(name = "condtion",values = c("blue","red"))
# }

## Assemble one treatment dataset, comment out if not needed and change the group back to uv_nt
# in the ggplot graphs
# onetype_only = subset(uv_nt, condition == "YES")

## Density comps
p1 = ggplot(uv_nt, aes(x = area, color = condition)) + geom_density(lwd = 1.3, adjust = 1.4) + 
  theme_minimal() + scale_x_continuous(limits = c(0,45)) + labs(x = "Area", y = " ") +
  scale_color_manual(values=c("blue","red"))


p2 = ggplot(uv_nt, aes(x = lengthEllipse, color = condition)) + geom_density(lwd = 1.3, adjust = 1.4) + 
  theme_minimal() + scale_x_continuous(limits = c(1,22)) + labs(x = "Length", y = " ") +
  scale_color_manual(values=c("blue","red")) 

p3 = ggplot(uv_nt, aes(x = widthEllipse, color = condition)) + geom_density(lwd = 1.3, adjust = 1.4) + 
  theme_minimal() + scale_x_continuous(limits = c(1,5)) + labs(x = "Width", y = " ") +
  scale_color_manual(values=c("blue","red")) 

png("dens.png", width = 8, height = 2.5, unit = "in", res = 300)
ggarrange(p1, p2, p3, ncol=3, nrow=1, common.legend = TRUE, legend="right")
dev.off()
