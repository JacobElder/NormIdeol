library(groundhog)
library(shiny)
library(tidyverse)
library(arrow)
library(here)

longDf1 <- as.data.frame(read_parquet("main/data/PFfullDf.parquet"))
longDf2 <- as.data.frame(read_parquet("main/data/PFfullDf2.parquet"))

longDf1$partyN <- as.factor(longDf1$partyN)
longDf1$Rep <- as.factor(longDf1$Rep)
contrasts(longDf1$Rep) <- contr.sum(3)
longDf1$RepN <- as.factor(longDf1$RepN)
longDf1$RepN <- relevel(longDf1$RepN,"In")
longDf1$Info <- as.factor(longDf1$Info)
contrasts(longDf1$Info) <- contr.sum(2)
longDf1$partyN <- as.factor(longDf1$partyN)
contrasts(longDf1$partyN) <- contr.sum(2)

longDf2 <- longDf2 %>% rename(label = label.x)
longDf2$partyN <- as.factor(longDf2$partyN)
longDf2$Rep <- as.factor(longDf2$Rep)
longDf2$Rep <- factor(longDf2$Rep, c("Non","Rep","Dem"))
longDf2$Rep <- relevel(longDf2$Rep,"Non")
contrasts(longDf2$Rep) <- contr.sum(3)
longDf2$RepN <- as.factor(longDf2$RepN)
longDf2$RepN <- relevel(longDf2$RepN,"In")
longDf2$Info <- as.factor(longDf2$Info)
contrasts(longDf2$Info) <- contr.sum(2)
longDf2$partyN <- as.factor(longDf2$partyN)
contrasts(longDf2$partyN) <- contr.sum(2)

df1summsplit <- Rmisc::summarySE(longDf1, measurevar="eval", groupvars=c("label","partyN"),na.rm=T)
df1summ <- Rmisc::summarySE(longDf1, measurevar="eval", groupvars=c("label"),na.rm=T)
df1summsplit$partyN2 <- "Split"
df1summ$partyN2 <- "Combined"
df1summ$partyN <- "Both"
df1summ$study <- "Study 1"
df1summsplit$study <- "Study 1"
df2summsplit <- Rmisc::summarySE(longDf2, measurevar="eval", groupvars=c("label","partyN"),na.rm=T)
df2summ <- Rmisc::summarySE(longDf2, measurevar="eval", groupvars=c("label"),na.rm=T)
df2summsplit$partyN2 <- "Split"
df2summ$partyN2 <- "Combined"
df2summ$partyN <- "Both"
df2summ$study <- "Study 2"
df2summsplit$study <- "Study 2"

issuesOnly1 <- longDf1[!duplicated(longDf1$label),]
issuesOnly2 <- longDf2[!duplicated(longDf2$label),]

df1 <- issuesOnly1 %>% select(label, polarized, gap) %>% full_join(df1summsplit)
df2 <- issuesOnly2 %>% select(label, polarized, gap) %>% full_join(df2summsplit)

df1$eval <- round(df1$eval,2)
for(i in seq(from=1,to=nrow(df1)-1,by=2)){
  df1$max[i:(i+1)] <- max(df1$eval[i:(i+1)])
  df1$deval[i:(i+1)] <- df1$eval[i]
  df1$gap[i:(i+1)] <- (df1$eval[i] - df1$eval[(i+1)])
  df1$polarized[i:(i+1)] <- abs(df1$eval[i] - df1$eval[(i+1)])
}


# set a custom nudge value
nudge_value=.1

p_main=
  df1 %>% 
  
  # the following 3 lines of code are the same
  #ggplot(aes(x=round(eval,2),y= reorder(label,polarized) )) +
  ggplot(aes(x=round(eval,2),y= reorder(label,deval) )) +
  geom_line(aes(group=label), color="#E7E7E7", size=1.5) +
  geom_point(aes(color=partyN), size=2) +
  
  # data callout
  geom_text(aes(label=eval, color=partyN),
            size=3,
            nudge_x=if_else(
              df1$eval==df1$max, # if it's the larger value...
              nudge_value,   # move it to the right of the point
              -nudge_value), # otherwise, move it to the left of the point
            hjust=if_else(
              df1$eval==df1$max, #if it's the larger value
              0, # left justify
              1),# otherwise, right justify      
  )+
  
  # legend
  geom_text(aes(label=partyN, color=partyN), 
            #data=. %>% filter(gap==max(abs(gap)) ),
            data=. %>% filter(deval==max(deval) ),
            nudge_y =2, 
            fontface="bold",
            size=5)+  
  
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_text(color="black", size=9),
        axis.text.x = element_text(color="black", size =12),
        axis.title = element_blank()#,
        #panel.grid = element_blank()
  ) +
  #labs(x=",y=NULL) +
  scale_color_manual(values=c("#436685", "#BF2F24")) +
  
  #extend the y-axis otherwise the legend is cut off
  coord_cartesian(xlim=c(1.2, 7),ylim=c(-1,103)) +
  
  #display percentages with % appended
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7)) 

p_main

ggsave("~/Desktop/dumbbell.png",p_main,height=12,width=12,units="in")
