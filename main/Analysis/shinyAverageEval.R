library(groundhog)
library(shiny)
library(tidyverse)
library(arrow)
library(here)
# pkgs <-  c("shiny","tidyverse","arrow","here")
# groundhog.day <- '2022-07-25'
# groundhog.library(pkgs, groundhog.day)
#here::i_am("main/Analysis/supplemental_analyses.qmd")

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

alldfsumm <- rbind(df1summsplit, df2summsplit, df1summ, df2summ)

# Define the user interface for the app
ui <- fluidPage(
  
  # Add a title and sidebar
  titlePanel("Dynamic Bar Plot"),
  sidebarLayout(
    sidebarPanel(
      selectInput("study", "Study:",
                  c("Study 1" = "Study 1",
                    "Study 2" = "Study 2")),
      selectInput("partyN2", "Grouping:",
                  c("Separated" = "Split",
                    "Combined" = "Combined"))
    ),
    
    # Add a plot to the main panel
    mainPanel(plotOutput("barplot"))
  )
)

# Define the server logic for the app
server <- function(input, output) {
  
  # Create a reactive expression to generate the plot
  plot_data <- reactive({
  
    dataset<-{ 
      subset(alldfsumm, study == input$study)  
    }
    
    print(dataset)
    dataset2<-{ 
        subset(dataset, partyN2 == input$partyN2)  
    }
    
    if(input$partyN2=="Split"){
      print(dataset2)
      ggplot(dataset2, aes_string(x="label", y="eval",group="partyN", fill="partyN")) +
        geom_errorbar(aes(ymin=eval-se, ymax=eval+se),
                      color="black", 
                      width=.2,
                      position=position_dodge(.9))  +
        geom_bar(position="dodge", stat="identity",alpha=.2) + 
        coord_flip() +
        theme(axis.text=element_text(size=8)) +
        scale_color_manual(values=c("blue","red"),labels=c("Democrats","Republicans")) +
        scale_fill_manual(values=c("blue","red"),labels=c("Democrats","Republicans")) +
        labs(x="Issue Names", y="Average Rating") +
        jtools::theme_apa()  +
        geom_errorbar(aes(ymin=eval-se, ymax=eval+se), width=.2,
                      position=position_dodge(.9)) 
    }else if(input$partyN2=="Combined"){
      print(dataset2)
      ggplot(dataset2, aes_string(x="label", y="eval")) +
        geom_errorbar(aes(ymin=eval-se, ymax=eval+se), width=.2,
                      position=position_dodge(.9))  +
        geom_bar(stat="identity",fill="purple",alpha=.2) + 
        coord_flip() +
        theme(axis.text=element_text(size=8)) +
        scale_color_manual(values=c("blue","red"),labels=c("Democrats","Republicans")) +
        scale_fill_manual(values=c("blue","red"),labels=c("Democrats","Republicans")) +
        labs(x="Issue Names", y="Average Rating") +
        jtools::theme_apa()
    }

  })
  
  # Render the plot
  output$barplot <- renderPlot({
    plot_data()
  },
  height=1200)
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
