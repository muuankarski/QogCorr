library(shiny)
library(ggplot2)
library(grid)
library(gridExtra)

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
    
    load("data/standardCont.RData")
    
    
    variableInputY <- reactive({
        switch(input$variableY)
    })
    variableInputX <- reactive({
        switch(input$variableX)
    })
    variableInputYear <- reactive({
        switch(input$year,
               "1946" = 1946,
               "1947" = 1947,
               "1948" = 1948,
               "1949" = 1949,
               "1950" = 1950,
               "1951" = 1951,
               "1952" = 1952,
               "1953" = 1953,
               "1954" = 1954,
               "1955" = 1955,
               "1956" = 1956,
               "1957" = 1957,
               "1958" = 1958,
               "1959" = 1959,
               "1960" = 1960,
               "1961" = 1961,
               "1962" = 1962,
               "1963" = 1963,
               "1964" = 1964,
               "1965" = 1965,
               "1966" = 1966,
               "1967" = 1967,
               "1968" = 1968,
               "1969" = 1969,
               "1970" = 1970,
               "1971" = 1971,
               "1972" = 1972,
               "1973" = 1973,
               "1974" = 1974,
               "1975" = 1975,
               "1976" = 1976,
               "1977" = 1977,
               "1978" = 1978,
               "1979" = 1979,
               "1980" = 1980,
               "1981" = 1981,
               "1982" = 1982,
               "1983" = 1983,
               "1984" = 1984,
               "1985" = 1985,
               "1986" = 1986,
               "1987" = 1987,
               "1988" = 1988,
               "1989" = 1989,
               "1990" = 1990,
               "1991" = 1991,
               "1992" = 1992,
               "1993" = 1993,
               "1994" = 1994,
               "1995" = 1995,
               "1996" = 1996,
               "1997" = 1997,
               "1998" = 1998,
               "1999" = 1999,
               "2000" = 2000,
               "2001" = 2001,
               "2002" = 2002,
               "2003" = 2003,
               "2004" = 2004,
               "2005" = 2005,
               "2006" = 2006,
               "2007" = 2007,
               "2008" = 2008,
               "2009" = 2009,
               "2010" = 2010,
               "2011" = 2011,
               "2012" = 2012)
    })
    variableInputCont <- reactive({
        switch(input$continent)
    })
    
    #******************************#
    #*** Scatterplot
    
    datasetInput <- reactive(function() {
        #varx <- datPlot$perGini
        vary <- dat[, input$variableY]
        varx <- dat[, input$variableX]
        cntry <- as.character(dat$cname)
        contName <- as.character(dat$contName)
        year <- dat$year
        datPlotX <- data.frame(vary,varx,cntry,year,contName)
        datPlot <- datPlotX[datPlotX$year == input$year,]
    })
    
    ## ****** ###
    # Plot
    
    plotInput <- reactive(function() {
        
        cbPalette <- c("#000000", "#D55E00", "#56B4E9",  "#CC79A7", "#0072B2", "#F0E442")
        datPlot <- datasetInput()
        ## Subset russia data
#         datRus <- datPlot[datPlot$cntry %in% "Russia",]
        ## subset the continent
        if (input$continent == "All") datPlot <- datPlot
        if (input$continent != "All") datPlot <- datPlot[datPlot$contName == input$continent,]
        
        
        # subset data for highlighiting Russia
        ggplot(datPlot, aes(x=varx, y=vary, 
                            label=cntry,group=1)) +
            geom_point(data=datPlot, aes(color=contName), size=4) +
            geom_smooth(method=lm, se=TRUE, alpha=.5, 
                        linetype="dashed", size=0.5) +  
            geom_text(size=4, vjust=-0.8, hjust=0.5) +
            labs(x = input$variableX,
                 y = input$variableY) + 
#             geom_point(data=datRus, aes(x=varx, y=vary,group=1),
#                        color="red", size=5) +
#             geom_text(data=datRus, aes(x=varx, y=vary,
#                                        label=cntry,group=1),
#                       size=5, color="red",vjust=1, hjust=-.2) +
            theme_minimal() +
            scale_colour_manual(values=cbPalette) +
            theme(legend.title=element_blank()) +
            theme(legend.text=element_text(size=16)) +
            theme(legend.position="top") +
            theme(axis.title = element_text(size=16)) +
            theme(axis.text = element_text(size=16)) +
            guides(color = guide_legend(nrow = 2)) + 
            labs(title=paste("Year",input$year))
    })
    
    output$correlation <- renderPrint({
        vary <- dat[, input$variableY]
        varx <- dat[, input$variableX]
        cntry <- as.character(dat$cname)
        contName <- as.character(dat$contName)
        year <- dat$year
        datPlotX <- data.frame(vary,varx,cntry,year,contName)
        datPlot <- datPlotX[datPlotX$year == input$year,]
        if (input$continent == "All") datPlot <- datPlot
        if (input$continent != "All") datPlot <- datPlot[datPlot$contName == input$continent,]
        datPlot <- datPlot[!is.na(datPlot$varx),]
        datPlot <- datPlot[!is.na(datPlot$vary),]
        datPlot <- datPlot[!is.na(datPlot$cntry),]
        paste("Correlation =",cor(datPlot$varx,datPlot$vary))
        
    })
    
    output$plot <- reactivePlot(function() {
        print(plotInput())
    })
    
    #******************************#
    #*** Time Scatterplot
    
    datasetInputT <- reactive(function() {
        #varx <- datPlot$perGini
        vary <- dat[, input$variableY]
        varx <- dat[, input$variableX]
        cntry <- as.character(dat$cname)
        year <- dat$year
        datPlot <- data.frame(vary,varx,cntry,year)
        datPlot <- datPlot[datPlot$cntry %in% c(input$Country1,
                                                input$Country2,
                                                input$Country3,
                                                input$Country4,
                                                input$Country5),]
        datPlot <- datPlot[datPlot$year >= input$yearStart,]
        datPlot <- datPlot[datPlot$year <= input$yearEnd,]
        datPlot <- datPlot[!is.na(datPlot$varx), ]
        datPlotT <- datPlot[!is.na(datPlot$vary), ]
        datPlotT <- datPlotT[with(datPlotT, order(year)), ]
    })
    
    ## ****** ###
    # Plot
    
    plotInputT <- reactive(function() {
        cbPalette <- c("#000000", "#D55E00", "#56B4E9",  "#CC79A7", "#0072B2", "#F0E442")
        datPlotT <- datasetInputT()
        
        ggplot(datPlotT, aes(x=varx, y=vary, 
                             group=cntry,color=cntry,
                             label=year)) +
            geom_point(alpha=.5) + 
            geom_path(alpha=.5)  +
            geom_text(size=5, hjust=0.0, vjust=-0.5,alpha=.7) +
            geom_text(data=merge(datPlotT, aggregate(year ~ cntry, datPlotT, max),
                                 by=c("year","cntry")),
                      aes(x=varx,y=vary,label=cntry),
                      hjust=1,vjust=-1,size=5) + 
            labs(x = input$variableX,
                 y = input$variableY) + 
            theme_minimal() +
            scale_colour_manual(values=cbPalette) +
            theme(legend.title=element_blank()) +
            theme(legend.text=element_text(size=16)) +
            theme(legend.position="top") +
            theme(axis.title = element_text(size=16)) +
            theme(axis.text = element_text(size=16))
        
    })
    
    #   output$data <- renderPrint({ Tää oli datan plottaamiseen time-skatteriin että tiesi missä vika
    #     datPlotT <- datasetInputT()
    #     datPlotT
    #     
    #   })
    
    output$timeplot <- reactivePlot(function() {
        print(plotInputT())
    })
    
    #******************************#
    #*** Downloads scatter
    
    output$downloadPlot <- downloadHandler(
        filename = function() { paste("varx_",input$variableX,"_vary_",input$variableY,Sys.time(),'.png', sep='') },
        content = function(file) {
            png(file, width=800, height=800,res=72)
            print(plotInput())
            dev.off()
        })
    
    ## preparing data for download
    
    datasetInput2 <- reactive(function() {
        datPlot <- datasetInput()
        ## subset the continent
        if (input$continent == "All") datPlot <- datPlot
        if (input$continent != "All") datPlot <- datPlot[datPlot$contName == input$continent,]
    })
    
    
    output$downloadData <- downloadHandler(
        filename = function() { paste("varx_",input$variableX,"_vary_",input$variableY,Sys.time(),'.csv', sep='') },
        content = function(file) {
            write.csv(datasetInput2(), file)
        }
    )
    
    #******************************#
    #*** Downloads scatter
    
    output$downloadPlotT <- downloadHandler(
        filename = function() { paste("varx_",input$variableX,"_vary_",input$variableY,Sys.time(),'.png', sep='') },
        content = function(file) {
            png(file, width=800, height=800,res=72)
            print(plotInputT())
            dev.off()
        })
    
    output$downloadDataT <- downloadHandler(
        filename = function() { paste("varx_",input$variableX,"_vary_",input$variableY,Sys.time(),'.csv', sep='') },
        content = function(file) {
            write.csv(datasetInputT(), file)
        }
    )
    
})