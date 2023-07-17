library(assertthat)
library(dplyr)
library(purrr)
library(igraph)
library(ggplot2)
library(ggraph)
library(ggmap)
library(readxl)
library(ggrepel)
library(ggsci)
library(scales)

#country information
file <- "./combine.xlsx"
data_node <- read_xlsx(file,sheet = 1) %>%
  select(ID_old,Description_old,Code_old)
names(data_node) <- c("con_id","con_name","con_code")

#GTAP country lon&lat
file <- "./GTAP_141_info.xlsx"
data_GTAP <- read_xlsx(file) %>%
  select("FIRST_Code","LON","LAT")

#match each country with lon&lat
data_node <- merge(data_node,data_GTAP,
                   by.x = "con_code",by.y = "FIRST_Code",
                   all.x = T)

#Read data
file <- "./mydata_con_sum.csv"
data_edge <- read.csv(file,header = T)
data_edge <- subset(data_edge,from!=141)
data_edge <- subset(data_edge,to!=141)
#period definition
data_edge <- data_edge %>% mutate(period=0)
data_edge$period[data_edge$step<4] <- 1
data_edge$period[data_edge$step>3&data_edge$step<7] <- 2
data_edge$period[data_edge$step>6&data_edge$step<10] <- 3
data_edge$period[data_edge$step>9] <- 4
#Clustering removes duplicate node pairs.
data_edge_agg <- aggregate(data=data_edge,
                           freq~from+to+period,FUN=sum)
data_edge_agg$period <- as.factor(data_edge_agg$period)

#Artificial discrete classification in order to reduce unnecessary lines in the graph.
data_edge <- data_edge_agg
data_cate <- data_edge$freq
data_cate[data_cate<100] <- 1
data_cate[data_cate>=100&data_cate<500] <- 2
data_cate[data_cate>=500&data_cate<1000] <- 3
data_cate[data_cate>=1000&data_cate<2000] <- 4
data_cate[data_cate>=2000&data_cate<5000] <- 5
data_cate[data_cate>=5000] <- 6
data_edge$category <- data_cate
#New data. Natural logarithmic conversion of the frequency of the cascade.
data_edge <- data_edge %>% mutate(value_new=log(freq))

#g data for plot information, the columns of edges and nodes must
# meet the demand of i_graph, no spare column is needed
data_edge <- data_edge %>% select(from,to,freq,period,value_new,category)
data_node <- data_node %>% select(con_id,LON,LAT,con_code,con_name) #保留缩写或者全称
g_s <- graph_from_data_frame(data_edge, directed = F, vertices = data_node)

#edges for plot
edges_for_plot <- data_edge %>%
  left_join(data_node %>% select(con_id, LON, LAT), 
             by = c('from' = 'con_id'))
edges_for_plot = rename(edges_for_plot,c("LON"="x","LAT"="y"))
edges_for_plot <- edges_for_plot %>%
  left_join(data_node %>% select(con_id, LON, LAT), 
            by = c('to' = 'con_id'))
edges_for_plot = rename(edges_for_plot,c("LON"="xend","LAT"="yend"))
#check the row number
assert_that(nrow(edges_for_plot) == nrow(data_edge))

#weight nodes for point plot, size = degree
data_node$degree = degree(g_s)
data_node <- data_node[order(data_node$degree,decreasing = F),]
c_temp <- rep(1,15)
for (i in 2:10) {
  c_temp <- c(c_temp,rep(i,14))
}
data_node$category <- c_temp

#data_g: plot data
data_g <- data_node

#base world map background
maptheme <- theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))
country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = map_data('world'),
                               fill = "white", color = "#515151",
                               size = 0.1)
mapcoords <- coord_fixed(xlim = c(-150, 180), ylim = c(-52, 90))


#color settings
colorline <- c("#EE00007F","#008B457F","#1B19197F","#5F559B7F")
colorfill <- c("#EE0000FF","#008B45FF","#1B1919FF","#5F559BFF")

#test plot background
g_map <- ggplot(data_g) +country_shapes +
  mapcoords + maptheme +
  guides(size="none") +
  #points,由于难以同时控制动态线宽与节点大小，根据需求，动态线宽更为重要
  geom_point(aes(x = LON, y = LAT),
             shape = 21, fill = 'red',size=1.5,
             color = 'black', stroke = 1,show.legend = F)+
  geom_text(data=data_g,
            aes(x = LON, y = LAT, label = con_code), #跟保留的列一致
            hjust = 0, nudge_x = 1, nudge_y = 4,check_overlap = T,
            size = 3, color = "red", fontface = "bold",show.legend = F) +
  scale_size_continuous(range = 1)
g_map

#Organize and adjust drawing data
#---------------------------------------------------------------------------
F_p1 <- sum(filter(edges_for_plot,period==1)$freq)/1000/3
F_p2 <- sum(filter(edges_for_plot,period==2)$freq)/1000/3
F_p3 <- sum(filter(edges_for_plot,period==3)$freq)/1000/3
F_p4 <- sum(filter(edges_for_plot,period==4)$freq)/1000/3
to_country_1 <- unique(filter(edges_for_plot,period==1)$to)
to_country_2 <- unique(filter(edges_for_plot,period==2)$to)
to_country_3 <- unique(filter(edges_for_plot,period==3)$to)
to_country_4 <- unique(filter(edges_for_plot,period==4)$to)

#Only show the top 10 countries for each period.
top <- aggregate(data=edges_for_plot,freq~from+period,sum)
top_p1 <- top %>% 
  filter(period==1) %>%
  arrange(-freq) %>%
  head(10)
top_p2 <- top %>% 
  filter(period==2) %>%
  arrange(-freq) %>%
  head(10)
top_p3 <- top %>% 
  filter(period==3) %>%
  arrange(-freq) %>%
  head(10)
top_p4 <- top %>% 
  filter(period==4) %>%
  arrange(-freq) %>%
  head(10)

#Separate the plot data for the four periods
cate_thre <- 2
edges_for_plot_1 <- edges_for_plot %>%
  filter(period==1) %>%
  filter(category>cate_thre)
edges_for_plot_1 <- edges_for_plot_1[which(edges_for_plot_1$from%in%top_p1$from),]
edges_for_plot_2 <- edges_for_plot %>%
  filter(period==2) %>%
  filter(category>cate_thre)
edges_for_plot_2 <- edges_for_plot_2[which(edges_for_plot_2$from%in%top_p2$from),]
edges_for_plot_3 <- edges_for_plot %>%
  filter(period==3) %>%
  filter(category>cate_thre)
edges_for_plot_3 <- edges_for_plot_3[which(edges_for_plot_3$from%in%top_p3$from),]
edges_for_plot_4 <- edges_for_plot %>%
  filter(period==4) %>%
  filter(category>cate_thre)
edges_for_plot_4 <- edges_for_plot_4[which(edges_for_plot_4$from%in%top_p4$from),]

#Part of the curve parameters, unified scale and legend, using color to distinguish four periods
scale_range <- c(0,1)
curvature <- 0.3
alphas <- (edges_for_plot_1$value_new-min(edges_for_plot_1$value_new))/max(edges_for_plot_1$value_new)+0.1
lwd <- edges_for_plot_1$value_new/max(edges_for_plot_1$value_new)

#Partial label parameter
text_color <- "black"
text_size <- 4
legend_size <- 10

#Partial parameters of saving the graph.
size <- c(5264,3188)
size_unit <- "px"
point_lw <- 0

color4 <- pal_nejm()(4)
show_col(color4)

data_g <- data_node

#map plot separately for 4 periods (one period is equal to 3 months)
g_map_p1 <- ggplot(data_g) +country_shapes +
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend,
                 lwd=alphas,alpha=alphas),
             data = edges_for_plot_1, curvature = curvature, 
             angle = 90,show.legend = T,
             lineend = "square",color="#BC3C29FF",
             arrow = arrow(angle = 20,type = "closed",
                           length = unit(0.15,"cm"))) +
  scale_size_continuous(range = scale_range) +
  #labs(lwd="Frequency") +
  guides(alpha="none",lwd="none") +
  theme(legend.title = element_text(size = legend_size)) +
  mapcoords + maptheme +

  geom_point(aes(x = LON, y = LAT, size = 0.2),
             shape = 21, fill = 'red',
             color = 'black', stroke = point_lw,show.legend = F)+
  geom_point(data=filter(data_g,con_id%in%top_p1$from),
             aes(x = LON, y = LAT, size = 0.8),
             shape = 21, fill = 'red',
             color = 'black', stroke = point_lw,show.legend = F)+
  geom_text_repel(data=filter(data_g,con_id%in%top_p1$from),
            aes(x = LON, y = LAT, label = con_name),
            hjust = 0, nudge_x = 1, nudge_y = 4,max.overlaps = 12,
            size = text_size, color = text_color, fontface = "bold",
            show.legend = F)
g_map_p1
ggsave("./Output/g_map_p1_top.pdf",width = size[1],height = size[2],units = size_unit,dpi = 600)

#period 2
scale_range <- c(0,1)
curvature <- 0.3
alphas <- (edges_for_plot_2$value_new-min(edges_for_plot_2$value_new))/max(edges_for_plot_2$value_new)+0.1
lwd <- edges_for_plot_2$value_new/max(edges_for_plot_2$value_new)

g_map_p2 <- ggplot(data_g) +country_shapes +
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend,
                 lwd=alphas,alpha=alphas),
             data = edges_for_plot_2, curvature = curvature, 
             angle = 90,show.legend = T,
             lineend = "square",color=color4[2],
             arrow = arrow(angle = 20,type = "closed",
                           length = unit(0.15,"cm"))) +
  scale_size_continuous(range = scale_range,
                        labels = labels) +
  #labs(lwd="Frequency") +
  guides(color="none",alpha="none",lwd="none") +
  theme(legend.title = element_text(size = legend_size)) +
  mapcoords + maptheme +
  
  geom_point(aes(x = LON, y = LAT, size = 0.2),
             shape = 21, fill = 'red',
             color = 'black', stroke = point_lw,show.legend = F)+
  geom_point(data=filter(data_g,con_id%in%top_p2$from),
             aes(x = LON, y = LAT, size = 0.8),
             shape = 21, fill = 'red',
             color = 'black', stroke = point_lw,show.legend = F)+
  geom_text_repel(data=filter(data_g,con_id%in%top_p2$from),
            aes(x = LON, y = LAT, label = con_name),
            hjust = 0, nudge_x = 1, nudge_y = 4,max.overlaps = 12,
            size = text_size, color = text_color, fontface = "bold",
            show.legend = F)
g_map_p2
ggsave("./Output/g_map_p2_top.pdf",width = size[1],height = size[2],units = size_unit,dpi = 600)

#period 3
scale_range <- c(0,1)
curvature <- 0.3
alphas <- (edges_for_plot_3$value_new-min(edges_for_plot_3$value_new))/max(edges_for_plot_3$value_new)+0.1
lwd <- edges_for_plot_3$value_new/max(edges_for_plot_3$value_new)

g_map_p3 <- ggplot(data_g) +country_shapes +
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend,
                 lwd=alphas,alpha=alphas),
             data = edges_for_plot_3, curvature = curvature, 
             angle = 90,show.legend = T,
             lineend = "square",color=color4[3],
             arrow = arrow(angle = 20,type = "closed",
                           length = unit(0.15,"cm"))) +
  scale_size_continuous(range = scale_range) +
  #labs(lwd="Frequency") +
  guides(color="none",lwd="none",alpha="none") +
  theme(legend.title = element_text(size = legend_size)) +
  mapcoords + maptheme +
  
  geom_point(aes(x = LON, y = LAT, size = 0.2),
             shape = 21, fill = 'red',
             color = 'black', stroke = point_lw,show.legend = F)+
  geom_point(data=filter(data_g,con_id%in%top_p3$from),
             aes(x = LON, y = LAT, size = 0.8),
             shape = 21, fill = 'red',
             color = 'black', stroke = point_lw,show.legend = F)+
  geom_text_repel(data=filter(data_g,con_id%in%top_p3$from),
            aes(x = LON, y = LAT, label = con_name),
            hjust = 0, nudge_x = 1, nudge_y = 4,
            size = text_size, color = text_color, fontface = "bold",
            show.legend = F)
g_map_p3
ggsave("./Output/g_map_p3_top.pdf",width = size[1],height = size[2],units = size_unit,dpi = 600)

#period 4
scale_range <- c(0,1)
curvature <- 0.3
alphas <- (edges_for_plot_4$value_new-min(edges_for_plot_4$value_new))/max(edges_for_plot_4$value_new)+0.1
lwd <- edges_for_plot_4$value_new/max(edges_for_plot_4$value_new)

g_map_p4 <- ggplot(data_g) +country_shapes +
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend,
                 lwd=alphas,alpha=alphas),
             data = edges_for_plot_4, curvature = curvature, 
             angle = 90,show.legend = T,
             lineend = "square",color=color4[4],
             arrow = arrow(angle = 20,type = "closed",
                           length = unit(0.15,"cm"))) +
  scale_size_continuous(range = scale_range) +
  #labs(lwd="Frequency") +
  guides(color="none",lwd="none",alpha="none") +
  theme(legend.title = element_text(size = legend_size)) +
  mapcoords + maptheme +

  geom_point(aes(x = LON, y = LAT, size = 0.2),
             shape = 21, fill = 'red',
             color = 'black', stroke = point_lw,show.legend = F)+
  geom_point(data=filter(data_g,con_id%in%top_p4$from),
             aes(x = LON, y = LAT, size = 0.8),
             shape = 21, fill = 'red',
             color = 'black', stroke = point_lw,show.legend = F)+
  geom_text_repel(data=filter(data_g,con_id%in%top_p4$from),
            aes(x = LON, y = LAT, label = con_name),
            hjust = 0, nudge_x = 1, nudge_y = 4,max.overlaps = 12,
            size = text_size, color = text_color, fontface = "bold",
            show.legend = F)
g_map_p4
ggsave("./Output/g_map_p4_top.pdf",width = size[1],height = size[2],units = size_unit,dpi = 600)

