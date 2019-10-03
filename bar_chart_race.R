##bar chart race

#load packages
library(tidyverse)
library(gganimate)
library(reshape2)
library(gifski)

#load data
df <- read.csv("C:/workspaceR/avocado/avocado.csv")
df <- select(df, Small.Bags, Large.Bags, XLarge.Bags, year)
cols <- c("Small", "Large", "XLarge", "year")
colnames(df) <- cols

#group by year
sum_table <- df %>% group_by(year) %>% summarize_all(sum)

#wide to long
sum_table <- melt(sum_table, id.vars = "year")

#change $variable to $name
colnames(sum_table)[2] <- "name"

#set sum_table as data
data <- sum_table

###########################################################################################
#### forked source = https://github.com/stevejburr/Bar-Chart-Race/blob/master/Final.R #####

#inputs:
#data - the dataset, must contain a column called "year"
#x - the column which contains the numeric value to plot 
#y - the column which contains the labels of the plot

#optional: 
#title - title to show at the top of the graph, defaults to blank
#caption - text show in footer, defaults to blank
#number - filter to top "number" defaults to 10 for top 10 in each period
#parameters passed to "animate" function: defaults to nframes=300, fps=5, end_pause=20

make_barchart_race <- function(data,x,y,
                               number=10,
                               title="",
                               caption="",
                               nframes=250,
                               fps=5,
                               end_pause=10){
  #set up variables for use with tidy evaluation
  y <- rlang::enquo(y)
  x <- rlang::enquo(x)
  number <- rlang::enquo(number)
  
  #take the input dataset, compute ranks within each time period
  data %>%
    group_by(year) %>%
    arrange(-!!y) %>%
    mutate(rank=row_number()) %>%
    #filter to top "number"
    filter(rank<=!!number) -> data
  
  #plot the data
  data %>%
    ggplot(aes(x=-rank,y=!!y,fill=!!x, group=!!x)) +
    geom_tile(aes(y=!!y/2,height=!!y),width=0.9,show.legend = F)+
    geom_text(aes(label=!!x),
              hjust="right",
              colour="black",
              fontface="bold",
              nudge_y=-1000)+
    geom_text(aes(label=scales::comma(!!y)),
              hjust="left",
              nudge_y=2000,
              colour="grey30")+
    theme_minimal() +
    coord_flip(clip="off") +
    scale_x_discrete("") +
    scale_y_continuous("",labels=scales::comma)+
    theme(panel.grid.major.y=element_blank(),
          panel.grid.minor.x=element_blank(),
          plot.title= element_text(size=20,colour="grey50",face="bold"),
          plot.caption = element_text(colour="grey50"),
          plot.subtitle = element_text(size=20,colour="grey50",face="bold"),
          plot.margin = margin(1,1,1,2,"cm"),
          axis.text.y=element_blank())+
    #this bit does the animation by year
    transition_time(year) +
    labs(title=title,
         subtitle='{round(frame_time,0)}',
         caption=caption)-> p
  
  #animate the plot - this is returned by the function
  animate(p, nframes = nframes, fps = fps, end_pause = end_pause, width = 1000, height = 400, duration = 40)
}

#call the function to make the animation:
make_barchart_race(data,
                   name,
                   value,
                   title="Bag size sales by year",
                   caption="Avocado sales")

#save it:
anim_save("out.gif")




