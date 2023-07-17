library(htmlwidgets)
library(webshot)
library(sankeyD3)
library(tidyverse)
library(ggalluvial)
library(reshape)
library(readxl)
library(dplyr)
library(scales)
library(ggsci)
library(here)

#The number of aggregated sectors and original sectors
num_sec <- 10
NN <- 65

#Sector information data of GTAP
sma_tb <- read_xlsx("./combine.xlsx", sheet = 2)
sma_tb_old <- sma_tb

#Function: Give a small department id to get a large department id or vice versa
id_trans <- function(id,type="up") {
  if (type=="up") {
    return(sma_tb_old[which(sma_tb_old$ID_old==id),]$ID_new[1])
  } else {
    return(sma_tb_old[which(sma_tb_old$ID_new==id),]$ID_old)
  }
}

#The first step，transform the cascading propagation data ("mydata.rds") into monthly data and annual data
mydata <- read_rds("mydata_sec_sum.rds")
#Annual cascading propagation data, mydata_new
for (i in 1:dim(mydata)[1]) {
  mydata$from_new[i] <- id_trans(as.numeric(mydata$from[i]))
  mydata$to_new[i] <- id_trans(as.numeric(mydata$to[i]))
}
#Monthly cascading propagation data, mydata_m[i]
for (i in 1:12) {
  assign(paste0("mydata_new",i),
         aggregate(data=mydata[which(mydata$step==i),],freq~from_new+to_new,sum))
  mydata_temp <- get(paste0("mydata_new",i))
  mydata_temp <- mydata_temp %>% mutate(freq_R=freq/500)
  assign(paste0("mydata_new",i),
         mydata_temp)
}


#The second step，redefine the node position and order, 
#that is, to use the order of the aggregated departments.
sma_tb <- sma_tb %>% arrange(ID_new)#按新部门序号排序
sma_tb <- sma_tb %>% mutate(new_ord = 1:NN, abbr = paste0(tcode,ID_new))
#"abbr" is a new abbreviation. The first two characters represent the code of the small industry, and the last letter represents the large industry to which new_ord belongs
sma_lev <- sma_tb$abbr
#This is a new abbreviation for NN small industries, as a standard for subsequent factorization (sorting so that the same aggregated sectors are grouped together).
sma_tb$ID_old <- factor(sma_tb$ID_old, 1:NN)
sma_tb$abbr <- factor(sma_tb$abbr, sma_lev)
big_tb <- data.frame(matrix(nrow = num_sec,ncol = 3))
names(big_tb) <- c("name","code","id")
big_tb$name <- unique(sma_tb$Description_new)
big_tb$code <- unique(sma_tb$Code_new)
big_tb$id <- unique(sma_tb$ID_new)

#The third step, sankey plot by the "sankeyNetwork" package. 
nodes_big <- rbind(big_tb,big_tb) #node define
#Node color setting.
colorJS <- 'd3.scaleOrdinal(["#4ENN7D", "#FAD8C3", "#E0B88B",
              "#A28D70","#8F6028","#699CD4","#E4966E","#D47D39",
              "#856A41","#4B2D07"]);'

#Annual "mydata" processing.
mydata_new <- aggregate(data=mydata,freq~from_new+to_new,sum)
m_temp <- match(mydata_new$from_new,big_tb$id)-1
m_temp_2 <- match(mydata_new$to_new,big_tb$id)+num_sec-1
mydata_new$source <- m_temp
mydata_new$target <- m_temp_2

#Monthly "mydata" processing
for (i in 1:12) {
  mydata_temp <- get(paste0("mydata_new",i))
  m_temp <- match(mydata_temp$from_new,big_tb$id)-1
  m_temp_2 <- match(mydata_temp$to_new,big_tb$id)+num_sec-1
  
  assign("mydata_temp",get(paste0("mydata_new",i)))
  mydata_temp$source <- m_temp
  mydata_temp$target <- m_temp_2
  assign(paste0("mydata_new",i),mydata_temp)
}

#plot test.
mydata_temp <- get(paste0("mydata_new"))
p <- sankeyNetwork(Links = mydata_temp, Nodes = nodes_big,
                   
                   Source = "source", Target = "target",
                   
                   Value = "freq", NodeID = "name",nodeWidth =18,units = 'cm',
                   
                   height=600,width=600,
                   
                   colourScale=colorJS, #zoom = T, dragX = T, dragY = T,
                   
                   numberFormat=".0f",fontSize = 16,showNodeValues = F,
                   
                   LinkGroup = "from_new",NodeGroup = "id")
p

#----------------------------------------------------------------------
#output folder
out_folder <- "./Output"
if (!dir.exists(out_folder)){
  dir.create(out_folder)
}

#save the annual sector-cascade plot
saveNetwork(p,"./Output/sankeyD3.html")
webshot("./Output/sankeyD3.html" , paste0("./Output/sankeyD3_p_all",".pdf"),cliprect = "viewport",zoom = 0.5,debug = F)

#The figures are saved as pdf for 12 months
for (i in 1:12) {
  mydata_temp <- get(paste0("mydata_new",i))
  nodes_temp <- nodes_big
  #nodes_temp$name <- ""
  
  mydata_temp$freq_per <- mydata_temp$freq
  for (j in 1:length(mydata_temp)) {
    mydata_temp$freq_per[j] <- mydata_temp$freq[j]/
      sum(mydata_temp$freq[which(mydata_temp$from_new==mydata_temp$from_new[j])])
  }
  p <- sankeyNetwork(Links = mydata_temp, Nodes = nodes_temp, 
                     Source = "source", Target = "target",
                     
                     Value = "freq", NodeID = "name",nodeWidth =18,units = 'cm',
                     
                     height=600,width=750,
                     
                     colourScale=colorJS,
                     
                     numberFormat=".0f",fontSize = 16,showNodeValues = F,
                     
                     LinkGroup = "from_new",NodeGroup = "id",
                     
                     highlightChildLinks = T,doubleclickTogglesChildren = T,zoom = T)
  saveNetwork(p,"./Output/sankeyD3.html")
  webshot("./Output/sankeyD3.html" , paste0("./Output/sankeyD3_m",i,".pdf"),cliprect = "viewport",zoom = 0.5,debug = F)
  print(i)
}
p
