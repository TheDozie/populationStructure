library(shiny) 
library(ggplot2) # need this for plotting the pyramid and evolution
library(plyr) # need this for reformatting the data frame

popfile<-"http://ecastats.uneca.org/popVis/popCountries.csv"
popCountries<-read.csv(popfile, header=TRUE)
countries<-unique(popCountries[,1])
countries<-as.character(countries[order(countries)])

thisYear<-as.integer(format(Sys.Date(), "%Y"))

# Define server to draw pyramid and age grade evolution
shinyServer(function(input, output, session) {

      observe({
            updateSelectInput(session, "country", choices=as.list(countries))
      })

      readData <- reactive({
            popcountry=input$country
            
            ## check if country list has been set up, if not don't bother
            listUp <- grep("#", popcountry)
            if (length(listUp) == 0){      
                  popfile <-paste("http://ecastats.uneca.org/popVis/", popcountry, ".csv", sep="")
                  popdata<-read.csv(popfile, header=TRUE)
            }
      })
      
      output$popPyramid <- renderPlot({
            baseYear=input$years[1]
            compYear=input$years[2]
            
            popcountry=input$country
            listUp <- grep("#", popcountry)
            if(length(listUp) == 0){
                  popdata <- readData()
                  
                  g1<-DzPyramid2(popdata, popcountry, baseYear, compYear)
                  print(g1)
            }
      })
      
      output$popAges <- renderPlot({
            baseYear=input$years[1]
            compYear=input$years[2]
            
            popcountry=input$country
            listUp <- grep("#", popcountry)
            if(length(listUp) == 0){
                  popdata <- readData()
                  
                  g2<-ageGrades(popdata, popcountry, baseYear, compYear)
                  print(g2)
            }
      })      
      
      output$popSummary <- renderTable({
            baseYear=input$years[1]
            compYear=input$years[2]
            
            popcountry=input$country
            listUp <- grep("#", popcountry)
            if (length(listUp) == 0){
                  popdata <- readData()
                  
                  popTable<-popProfile(popdata, popcountry, baseYear, compYear)
                  popTable
            }
      })
})

DzPyramid2 = function(popdata, country, baseYear, compYear){
      
      names(popdata)<-c("Country", "Year", "Sex", "Age", "Population")
      popDf<-popdata[popdata$Country==country & (popdata$Year==baseYear | 
                                                popdata$Year == thisYear |
                                                popdata$Year == compYear),]
      popDf<-na.omit(popDf)
      
      popDf$Population<-as.numeric(popDf$Population)
      popDf$Age<-as.numeric(popDf$Age)
      
      popSums<-ddply(popDf, c("Year"), summarize, 
                     sum=sum(Population))

      baseSum<-popSums$sum[popSums$Year == baseYear]
      thisSum<-popSums$sum[popSums$Year == thisYear]
      compSum<-popSums$sum[popSums$Year == compYear]
      
      # add percentages over year sums
      # make male values negative in order to plot to the left
      popDf <- mutate(popDf, 
                      popPct = ifelse(Year == baseYear, 
                                      ifelse(Sex == "Male", 
                                             -100*Population/baseSum,
                                             100*Population/baseSum
                                      ),
                                      ifelse(Year == thisYear, 
                                             ifelse(Sex == "Male", 
                                                    -100*Population/thisSum,
                                                    100*Population/thisSum
                                             ),
                                             ifelse(Sex == "Male", 
                                                    -100*Population/compSum,
                                                    100*Population/compSum
                                             )
                                      )
                      )
      )
      
      ## calculate tic marks on the age axis to be every 10 years
      agetics<-data.frame(ageTics=seq(from=0, to=max(popDf$Age),by=5),
                          ageText=as.character(seq(from=0, to=max(popDf$Age), by=5)))
      
      femaleTics<- seq(from=0, to=max(popDf$popPct)+2, by = 5)
      maleTics<- seq(from=0, to=min(popDf$popPct)-2, by=-5)
      
      pctTics<-unique(sort(c(maleTics,femaleTics))) ## use unique to remove the duplicate 0 value
      pctLabels <- paste(as.character(abs(pctTics)),"%", sep="")
      
      
      title<-paste("Population structure of ", 
                   country, 
                   " in ", 
                   baseYear, ", ", thisYear, " and ", compYear, sep="")

      popDf$legendColours <- ifelse(popDf$Sex == "Male", popDf$Year,
                                    paste(popDf$Year, popDf$Sex))

      DzPyramid<-ggplot(data=popDf, aes(x=Age, y=popPct)) +
            scale_colour_manual(breaks=as.character(c(baseYear, thisYear, compYear)),
                  values=c("red", "red", "blue", "blue", "green", "green")) +
            geom_line(aes(x=Age, y=popPct, colour=legendColours), size=1.5) +
            geom_hline(aes(yintercept=0)) + 
            scale_x_continuous(breaks=agetics$ageTics, labels=agetics$ageText)+
            scale_y_continuous(breaks=pctTics, labels=pctLabels) +
            theme(legend.title=element_blank(), 
                  legend.background=element_blank(),
                  legend.position=c(1,1),
                  legend.justification=c(1,1),
                  axis.title.x=element_blank(),
                  panel.background=element_blank(),
                  axis.line = element_line(colour = "black")) +
            coord_flip() +
            ggtitle(title) +
            annotate("text", x=5, y=min(popDf$popPct)/2, label="Male", fontface="bold") +
            annotate("text", x=5, y=max(popDf$popPct)/2, label="Female", fontface="bold") 
      return(DzPyramid)
}

ageGrades <- function(popdata, country, baseYear, compYear){
      
      names(popdata)<-c("Country", "Year", "Sex", "Age", "Population")
      popDf<-popdata[popdata$Country==country & 
                           (popdata$Year >= baseYear & 
                                  popdata$Year <= compYear),]
      
      popDf<-na.omit(popDf)
      
      popDf$Population<-as.numeric(popDf$Population)
      popDf$Age<-as.numeric(popDf$Age)
      
      popDf<-mutate(popDf, age_cat=ifelse(Age<15, "Children", 
                                          ifelse(Age<65, "Adult", "Old")))
      popDf<-ddply(popDf, c("Year", "age_cat"), summarize,
                   age_grade_population=sum(Population))
      
      ageGrades<-ggplot(popDf, aes(Year, age_grade_population)) +
            scale_fill_manual(values=c("Children"="orange", 
                                       "Adult"="blue",
                                       "Old"="gray"),
                              labels=c("Children: Ages 0-14",
                                       "Adult: Ages 15-64",
                                       "Old: Ages 65+")) +
            geom_area(aes(fill=age_cat), position='stack') +
            theme(legend.title=element_blank(), 
                  legend.background=element_blank(),
                  legend.position=c(0,1),
                  legend.justification=c(0,1),
                  axis.line = element_line(colour = "black"),
                  panel.background=element_blank()) + 
            ggtitle(paste("Evolution of population age grades for ", country,
                          " from ", baseYear, " to ", compYear, sep="")) +
            geom_vline(xintercept=thisYear, colour="white", size=1.5) +
            annotate("text", x=thisYear, y=0, label=as.character(thisYear), fontface="italic")
      return (ageGrades)
}

popProfile <- function(popdata, country, baseYear, compYear){
      
      names(popdata)<-c("Country", "Year", "Sex", "Age", "Population")
      popDf<-popdata[popdata$Country==country & (popdata$Year==baseYear | 
                                                       popdata$Year == thisYear |
                                                       popdata$Year == compYear),]
      popDf<-na.omit(popDf)
      
      popDf$Population<-as.numeric(popDf$Population)
      popDf$Age<-as.numeric(popDf$Age)
      
      popSums<-ddply(popDf, c("Year"), summarize, 
                     sum=sum(Population),
                     Children=sum(Population[Age < 15]),
                     adult = sum(Population[Age >= 15 & Age < 65]),
                     old = sum(Population[Age >= 65]),
                     males = sum(Population[Sex=="Male"]),
                     females = sum(Population[Sex == "Female"]),
                     sexRatio=males/females*100,
                     totalDependency = (Children+old)/adult*100,
                     ChildrenDependency = Children/adult*100,
                     oldDependency = old/adult*100)

      baseSum<-popSums$sum[popSums$Year == baseYear]
      thisSum<-popSums$sum[popSums$Year == thisYear]
      compSum<-popSums$sum[popSums$Year == compYear]
      
      # initialize population profile
      popProfile <- data.frame(feature = "Years", baseYear = as.character(as.integer(baseYear)), 
                               thisYear =  as.character(as.integer(thisYear)),
                               compYear =  as.character(as.integer(compYear)))
      
      popProfile <- rbind(popProfile, 
                          data.frame(feature="Total Population (millions)", 
                                     baseYear=as.character(round(baseSum/1000,3)),
                                     thisYear=as.character(round(thisSum/1000,3)),
                                     compYear=as.character(round(compSum/1000,3))),
                          data.frame(feature="Sex Ratio (males per 100 females)",
                                     baseYear=as.character(as.integer(popSums$sexRatio[popSums$Year==baseYear])),
                                     thisYear=as.character(as.integer(popSums$sexRatio[popSums$Year==thisYear])),
                                     compYear=as.character(as.integer(popSums$sexRatio[popSums$Year==compYear]))),
                          data.frame(feature="Children: Ages 0-14 (% of total)", 
                                     baseYear=as.character(as.integer(100*popSums$Children[popSums$Year==baseYear]/baseSum)), 
                                     thisYear=as.character(as.integer(100*popSums$Children[popSums$Year==thisYear]/thisSum)), 
                                     compYear=as.character(as.integer(100*popSums$Children[popSums$Year == compYear]/compSum))),
                          data.frame(feature="Adult: Ages 15-64 (% of total)", 
                                     baseYear=as.character(as.integer(100*popSums$adult[popSums$Year==baseYear]/baseSum)), 
                                     thisYear=as.character(as.integer(100*popSums$adult[popSums$Year==thisYear]/thisSum)), 
                                     compYear=as.character(as.integer(100*popSums$adult[popSums$Year == compYear]/compSum))),
                          data.frame(feature="Old: Ages 65+ (% of total)", 
                                     baseYear=as.character(as.integer(100*popSums$old[popSums$Year==baseYear]/baseSum)), 
                                     thisYear=as.character(as.integer(100*popSums$old[popSums$Year==thisYear]/thisSum)), 
                                     compYear=as.character(as.integer(100*popSums$old[popSums$Year == compYear]/compSum))),
                          data.frame(feature="Total Dependency Ratio (%)", 
                                     baseYear=as.character(as.integer(popSums$totalDependency[popSums$Year==baseYear])), 
                                     thisYear=as.character(as.integer(popSums$totalDependency[popSums$Year==thisYear])), 
                                     compYear=as.character(as.integer(popSums$totalDependency[popSums$Year == compYear]))),
                          data.frame(feature="Children Dependency Ratio (%)", 
                                     baseYear=as.character(as.integer(popSums$ChildrenDependency[popSums$Year==baseYear])), 
                                     thisYear=as.character(as.integer(popSums$ChildrenDependency[popSums$Year==thisYear])), 
                                     compYear=as.character(as.integer(popSums$ChildrenDependency[popSums$Year == compYear]))),
                          data.frame(feature="Old Dependency Ratio (%)", 
                                     baseYear=as.character(as.integer(popSums$oldDependency[popSums$Year==baseYear])), 
                                     thisYear=as.character(as.integer(popSums$oldDependency[popSums$Year==thisYear])), 
                                     compYear=as.character(as.integer(popSums$oldDependency[popSums$Year == compYear])))
      )
      
      names(popProfile)<-c("Feature", "Base Year", "This Year", "Future Year")
      return(popProfile)
}
