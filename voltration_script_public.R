#############################################################################

#Enter PMT Increment VOltage here
voltage_step <-  50
#Enter -A, -H, -W parameter (default is -A)
parameter_specific <- "-A"

##############################################################################

#install and load libraries
if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")}
if("EnvStats" %in% rownames(installed.packages()) == FALSE) {install.packages("EnvStats")}
if("flowCore" %in% rownames(installed.packages()) == FALSE) {install.packages("flowCore")}
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")}
if("gtable" %in% rownames(installed.packages()) == FALSE) {install.packages("gtable")}
if("janitor" %in% rownames(installed.packages()) == FALSE) {install.packages("janitor")}

#load libraries
library(tidyverse)
library(EnvStats)
library(flowCore) #to read FCS files
library(ggplot2) #to plot
library(gtable) #to deal with table data 
library(janitor) #to cleanup names

# #set the working directory from which the files will be read from
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
#    

#make a list of all .fcs files in folder
filelist <- list.files(pattern = ".fcs")

#placeholder for information to go into out file.
output_list <- c("Parameter", "Voltage", "SD", "CV")

for (a in 1:length(filelist)){ #for all files in the list
  
  #read in FCS file
  fcs <- read.FCS(paste(filelist[a]))
  
  data = as.data.frame(exprs(fcs))
  
  parameter_names <- fcs@parameters@data[1] #extract CV & mean/GM parameter and add to master list for all heights for all groups (potentially variable)
  parameter_names_logic <- grepl(parameter_specific, parameter_names[,1]) #get parameters that are only the A/H/W here - this is chosen here
  
  #drop detectors not used here based on logic and get values
  for (b in 1:length(parameter_names_logic)){#
    if (parameter_names_logic[b] == "TRUE"){
      
      #get this stat and append to growing list
        stat_CV <-  cv(fcs@exprs[,b])#note CV is not percentage but fraction instead
        stat_SD <-  sd(fcs@exprs[,b])
        stat_GeoMean <- geoMean(fcs@exprs[,b])
        stat_median <- median(fcs@exprs[,b])
        stat_SD <- mad(fcs@exprs[,b])#most robust...Median Absolute Deviation
        row_entry <- c(parameter_names$name[b],(voltage_step*(a-1)),stat_SD, stat_CV)
        output_list <- rbind(output_list,row_entry)
  }
  }
}
  
write.csv(output_list,"completed.csv")
data_to_plot <- as.data.frame(output_list) #change to df
data_to_plot <- data_to_plot[-1,] #remove first title row
colnames(data_to_plot) <-c("Parameter", "PMT V", "SD", "CV") #update col names

indx <-  sapply(data_to_plot, is.factor) #step 1 factor to numeric
indx[1:1] <-  FALSE #step 2
data_to_plot[indx] <- lapply(data_to_plot[indx], function(x) (as.character(x)))#step 3 

df <- janitor::clean_names(data_to_plot)

df$pmt_v <- as.numeric(as.character(df$pmt_v))
df$sd <- as.numeric(as.character(df$sd))

#subset data (1 detector)
for (name_count in seq(1, 45, 1))
{
df_edit <- subset(df, parameter %in% c(parameter_names$name[name_count]))

df_edit1 <- df_edit

p = ggplot(df,aes(pmt_v, sd), size=1) +
  geom_line(data = df_edit1, aes(pmt_v, sd), size=1) +
  geom_point(data = df_edit1, aes(pmt_v, sd), size=3, shape= 20) +
  xlab('PMT Voltage') +
  ylab('Median Absolute Deviation') +
  scale_y_continuous(trans='log10') +
  annotate("text", label = paste("2.5xmad_EN = ",round(2.5*df_edit$sd[1]), sep=""), x = 100, y = 50, size = 4, colour = "red")+  
  geom_hline(yintercept=round(2.5*df_edit$sd[1]), linetype="dashed", color = "red")+
  #expand_limits(x=c(0,30), y=c(0, 150)) +# change the axis limits
  #expand_limits(x=0, y=0) +# set the intercept of x and y axis at (0,0)
  ggtitle(parameter_names$name[name_count], subtitle = 'Negative population SD')

  #This actually save the plot in a image
  ggsave(file=paste(parameter_names$name[name_count]," SD.jpg"))
}

