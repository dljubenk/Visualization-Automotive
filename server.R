library(shiny)
library(ggplot2)
library(mapdata)
library(gridExtra)
library(RColorBrewer)
# Loading the data
autos <- read.csv("autos_clean_new.csv")

shinyServer(function(input, output) {
  
  observe({

    toggle(id = "rangeDistYear", condition = (input$selectDist == "Year of registration"))
    toggle(id = "rangeDistPrice", condition = (input$selectDist == "Price"))
    toggle(id = "rangeDistKm", condition = (input$selectDist == "Kilometers"))
    toggle(id = "rangeDistPower", condition = (input$selectDist == "Power"))
  })
  
  output$resale <- renderPlot({
    
    autos <- autos[which(autos$yearOfRegistration > input$range[1] & autos$yearOfRegistration < input$range[2]),]
    
    if (input$selectBrand != 'All') {
      autos <- autos[which(autos$brand == input$selectBrand),]
      by_mean <- by(autos$price,autos$model,mean)
      by_sd <- by(autos$price,autos$model,sd)
      autos_mean <- cbind(mean=by_mean, model=rownames(by_mean))
      autos_sd <- cbind(sd=by_sd, model=rownames(by_sd))
      autos_ratio <- merge(autos_mean, autos_sd, by="model")
      
    } else {
      by_mean <- by(autos$price,autos$brand,mean)
      by_sd <- by(autos$price,autos$brand,sd)
      autos_mean <- cbind(mean=by_mean, brand=rownames(by_mean))
      autos_sd <- cbind(sd=by_sd, brand=rownames(by_sd))
      autos_ratio <- merge(autos_mean, autos_sd, by="brand")
    }
    
    autos_ratio$sd <- as.numeric(as.character(autos_ratio$sd))
    autos_ratio$mean <- as.numeric(as.character(autos_ratio$mean))
    autos_ratio$ratio <- autos_ratio$sd / autos_ratio$mean
    autos_ratio <- autos_ratio[which(autos_ratio$ratio != 0),]
    
    if (input$measureType == "Standard deviation") {
      autos_ratio <- autos_ratio[order(autos_ratio$sd),]
    } else {
      autos_ratio <- autos_ratio[order(autos_ratio$ratio),]
    }
    
    topN <- switch(input$topN, 
                   "All" = autos_ratio,
                   "Top 5" = tail(autos_ratio, 5),
                   "Top 10" = tail(autos_ratio, 10))
    par(mar=c(0, 8, 0, 0))
    if (input$selectBrand != 'All') {
      if(input$measureType == "Standard deviation") {
        barplot(topN$sd, names=topN$model, las="2", horiz=TRUE)
      } else {
        barplot(topN$ratio, names=topN$model, las="2", horiz=TRUE)
      }
      
    } else {
      if(input$measureType == "Standard deviation") {
        barplot(topN$sd, names=topN$brand, las="2",horiz=TRUE)
      } else {
        barplot(topN$ratio, names=topN$brand, las="2",horiz=TRUE)
      }
    }
    
  })
  
  getDataSecond <- reactive({
    
  })
  
  output$second <- renderPlot({
    
    autos <- autos[which(autos$yearOfRegistration > input$range2[1] & autos$yearOfRegistration < input$range2[2]),]
    
    autos <- autos[which(autos$sellTime < 15000),]
    
    if (input$selectBrand2 != "All") {
      autos <- autos[which(autos$brand==input$selectBrand2),]
    } 
    
    if (input$selectX == "Power") {
      autos$powerPS <- round(autos$powerPS / 10)
    }
    if (input$selectX == "Price") {
      autos$price <- round(autos$price / 1000)
    }
    
    sellTime_mean  <- switch(input$selectX, 
                   "Year of registration" = by(autos$sellTime,autos$yearOfRegistration,mean),
                   "Power" = by(autos$sellTime,autos$powerPS,mean),
                   "Kilometer" = by(autos$sellTime,autos$kilometer,mean),
                   "Price" = by(autos$sellTime,autos$price,mean))
    
    df_sellTime_mean <- cbind(X=rownames(sellTime_mean/1440),mean=sellTime_mean/1440)
    df_sellTime_mean <- as.data.frame(df_sellTime_mean)
    df_sellTime_mean$mean <- as.numeric(as.character(df_sellTime_mean$mean))
    df_sellTime_mean$X <- as.numeric(as.character(df_sellTime_mean$X))
    plot <- ggplot(df_sellTime_mean, aes(X, mean, group = 1)) +
      geom_point() +
      geom_line() +
      labs(y = "Mean of selling time in days", 
           title = "Average selling time dependency on some attribute")
    
    if (input$selectX == "Year of registration") {
      plot <- plot + scale_x_continuous(breaks=seq(1950, 2016, 5)) + 
        xlab(input$selectX)
    } else if (input$selectX == "Price") {
      plot <- plot + scale_x_continuous(breaks=seq(0, 1000, 10)) + 
        xlab("Price in thousands")
    } else if (input$selectX == "Power") {
      plot <- plot + xlab("Power in tens")
    } else {
      plot <- plot + xlab(input$selectX)
    }
    
    plot
  })
  
  output$geo <- renderPlot({
    
    autos <- autos[which(autos$yearOfRegistration > input$rangeMapYear[1]
                         & autos$yearOfRegistration < input$rangeMapYear[2]),]
    autos <- autos[which(autos$price > input$rangeMapPrice[1]
                         & autos$price < input$rangeMapPrice[2]),]
    autos <- autos[which(autos$powerPS > input$rangeMapPower[1]
                         & autos$powerPS < input$rangeMapPower[2]),]
    autos <- autos[which(autos$kilometer > input$rangeMapKm[1]
                         & autos$kilometer < input$rangeMapKm[2]),]
    
    if (input$selectMapGearbox != "All") {
      autos <- autos[which(autos$gearbox==input$selectMapGearbox),]
    } 
    if (input$selectMapVehicletype != "All") {
      autos <- autos[which(autos$vehicleType==input$selectMapVehicletype),]
    } 
    if (input$selectMapFueltype != "All") {
      autos <- autos[which(autos$fuelType==input$selectMapFueltype),]
    } 
    
    if (input$selectBrand3 != "All") {
      autos <- autos[which(autos$brand==input$selectBrand3),]
    }
    
    pts <- data.frame(lon = autos$long, lat = autos$lat)    
    map('worldHires','Germany')
    points(pts)
    
  })
  
  output$distribution <- renderPlot({
    if (input$selectBrand4 != "All") {
      autos <- autos[which(autos$brand==input$selectBrand4),]
    }

    if (input$selectDist == "Year of registration") {
      if (input$selectFill == "Gearbox") {
        plotMultiples <- ggplot(autos, aes(yearOfRegistration, fill = gearbox)) + facet_wrap(~gearbox) + scale_fill_brewer(palette="PuOr")
        plot <- ggplot(autos, aes(yearOfRegistration, fill = gearbox)) + scale_fill_brewer(palette="PuOr")
      } else if(input$selectFill == "Vehicle type") {
        plotMultiples <- ggplot(autos, aes(yearOfRegistration, fill = vehicleType)) + facet_wrap(~vehicleType) + scale_fill_brewer(palette="PuOr")
        plot <- ggplot(autos, aes(yearOfRegistration, fill = vehicleType)) + scale_fill_brewer(palette="PuOr")
      } else if(input$selectFill == "Damaged") {
        plotMultiples <- ggplot(autos, aes(yearOfRegistration, fill = notRepairedDamage)) + facet_wrap(~notRepairedDamage)  + scale_fill_brewer(palette="PuOr")
        plot <- ggplot(autos, aes(yearOfRegistration, fill = notRepairedDamage))  + scale_fill_brewer(palette="PuOr")
      } else if(input$selectFill == "Fuel type") {
        plotMultiples <- ggplot(autos, aes(yearOfRegistration, fill = fuelType)) + facet_wrap(~fuelType)  + scale_fill_brewer(palette="PuOr")
        plot <- ggplot(autos, aes(yearOfRegistration, fill = fuelType)) + scale_fill_brewer(palette="PuOr")
      }
    } else if(input$selectDist == "Price") {
      if (input$selectFill == "Gearbox") {
        plotMultiples <- ggplot(autos, aes(price, fill = gearbox)) + facet_wrap(~gearbox)  + scale_fill_brewer(palette="PuOr")
        plot <- ggplot(autos, aes(price, fill = gearbox)) + scale_fill_brewer(palette="PuOr")
      } else if(input$selectFill == "Vehicle type") {
        plotMultiples <- ggplot(autos, aes(price, fill = vehicleType)) + facet_wrap(~vehicleType) + scale_fill_brewer(palette="PuOr")
        plot <- ggplot(autos, aes(price, fill = vehicleType)) + scale_fill_brewer(palette="PuOr")
      } else if(input$selectFill == "Damaged") {
        plotMultiples <- ggplot(autos, aes(price, fill = notRepairedDamage)) + facet_wrap(~notRepairedDamage) + scale_fill_brewer(palette="PuOr")
        plot <- ggplot(autos, aes(price, fill = notRepairedDamage)) + scale_fill_brewer(palette="PuOr")
      }else if(input$selectFill == "Fuel type") {
        plotMultiples <- ggplot(autos, aes(price, fill = fuelType)) + facet_wrap(~fuelType) + scale_fill_brewer(palette="PuOr")
        plot <- ggplot(autos, aes(price, fill = fuelType)) + scale_fill_brewer(palette="PuOr")
      }
    } else if(input$selectDist == "Power") {
      if (input$selectFill == "Gearbox") {
        plotMultiples <- ggplot(autos, aes(powerPS, fill = gearbox)) + facet_wrap(~gearbox) + scale_fill_brewer(palette="PuOr")
        plot <- ggplot(autos, aes(powerPS, fill = gearbox)) + scale_fill_brewer(palette="PuOr")
      } else if(input$selectFill == "Vehicle type") {
        plotMultiples <- ggplot(autos, aes(powerPS, fill = vehicleType)) + facet_wrap(~vehicleType) + scale_fill_brewer(palette="PuOr")
        plot <- ggplot(autos, aes(powerPS, fill = vehicleType)) + scale_fill_brewer(palette="PuOr")
      } else if(input$selectFill == "Damaged") {
        plotMultiples <- ggplot(autos, aes(powerPS, fill = notRepairedDamage)) + facet_wrap(~notRepairedDamage) + scale_fill_brewer(palette="PuOr")
        plot <- ggplot(autos, aes(powerPS, fill = notRepairedDamage)) + scale_fill_brewer(palette="PuOr")
      }else if(input$selectFill == "Fuel type") {
        plotMultiples <- ggplot(autos, aes(fuelType, fill = fuelType)) + facet_wrap(~fuelType) + scale_fill_brewer(palette="PuOr")
        plot <- ggplot(autos, aes(fuelType, fill = fuelType)) + scale_fill_brewer(palette="PuOr")
      }
    } else if(input$selectDist == "Kilometers") {
      if (input$selectFill == "Gearbox") {
        plotMultiples <- ggplot(autos, aes(kilometer, fill = gearbox)) + facet_wrap(~gearbox) + scale_fill_brewer(palette="PuOr")
        plot <- ggplot(autos, aes(kilometer, fill = gearbox)) + scale_fill_brewer(palette="PuOr")
      } else if(input$selectFill == "Vehicle type") {
        plotMultiples <- ggplot(autos, aes(kilometer, fill = vehicleType)) + facet_wrap(~vehicleType) + scale_fill_brewer(palette="PuOr")
        plot <- ggplot(autos, aes(kilometer, fill = vehicleType)) + scale_fill_brewer(palette="PuOr")
      }  else if(input$selectFill == "Damaged") {
        plotMultiples <- ggplot(autos, aes(kilometer, fill = notRepairedDamage)) + facet_wrap(~notRepairedDamage) + scale_fill_brewer(palette="PuOr")
        plot <- ggplot(autos, aes(kilometer, fill = notRepairedDamage)) + scale_fill_brewer(palette="PuOr")
      }else if(input$selectFill == "Fuel type") {
        plotMultiples <- ggplot(autos, aes(fuelType, fill = fuelType)) + facet_wrap(~fuelType) + scale_fill_brewer(palette="PuOr")
        plot <- ggplot(autos, aes(fuelType, fill = fuelType)) + scale_fill_brewer(palette="PuOr")
      }
    }
    
    if (input$selectDist == "Year of registration") {
      plot <- plot + scale_x_continuous(limits = c(input$rangeDistYear[1], input$rangeDistYear[2])) + scale_fill_brewer(palette="PuOr")
      plotMultiples <- plotMultiples + scale_x_continuous(limits = c(input$rangeDistYear[1], input$rangeDistYear[2])) + scale_fill_brewer(palette="PuOr")
    } else if(input$selectDist == "Price") {
      plot <- plot + scale_x_continuous(limits = c(input$rangeDistPrice[1], input$rangeDistPrice[2])) + scale_fill_brewer(palette="PuOr")
      plotMultiples <- plotMultiples + scale_x_continuous(limits = c(input$rangeDistPrice[1], input$rangeDistPrice[2])) + scale_fill_brewer(palette="PuOr")
    } else if(input$selectDist == "Power") {
      plot <- plot + scale_x_continuous(limits = c(input$rangeDistPower[1], input$rangeDistPower[2])) + scale_fill_brewer(palette="PuOr")
      plotMultiples <- plotMultiples + scale_x_continuous(limits = c(input$rangeDistPower[1], input$rangeDistPower[2])) + scale_fill_brewer(palette="PuOr")
    } else if(input$selectDist == "Kilometers") {
      plot <- plot + scale_x_continuous(limits = c(input$rangeDistKm[1], input$rangeDistKm[2])) + scale_fill_brewer(palette="PuOr")
      plotMultiples <- plotMultiples + scale_x_continuous(limits = c(input$rangeDistKm[1], input$rangeDistKm[2])) + scale_fill_brewer(palette="PuOr")
    }
    
    plot<- plot + geom_density(position = "stack") + 
      labs(x = input$selectDist, y = "Density", 
           title = "Distribution of year of registration")
    dev.new(width = 3, height = 4)
    plotMultiples<- plotMultiples + geom_density(position = "stack") + 
      labs(x = input$selectDist, y = "Density", 
           title = "Distribution of year of registration")
    grid.arrange(plot, plotMultiples, nrow = 2)
    
  })
  
  output$power <- renderPlot({
    if (input$brand != "All") {
      autos = autos[which(autos$brand==input$brand),]
      if (input$vehicleType == "all") {
        agg = aggregate(powerPS ~ model, autos, mean)
      } else {
        agg = aggregate(powerPS ~ model, autos[which(autos$vehicleType==input$vehicleType),], mean)
      }
      graph = ggplot(data=agg, aes(x=reorder(agg$model, -agg$powerPS), y=agg$powerPS, group=1, las=2))+
        xlab("Car model")
    } else {
      if (input$vehicleType == "all") {
        agg = aggregate(powerPS ~ brand, autos, mean)
      } else {
        agg = aggregate(powerPS ~ brand, autos[which(autos$vehicleType==input$vehicleType),], mean)
      }
      graph = ggplot(data=agg, aes(x=reorder(agg$brand, -agg$powerPS), y=agg$powerPS, group=1, las=2))+
        xlab("Car brand")
    }
    
    graph +
      theme(axis.text.x = element_text(size=20, angle = 90, hjust = 1, vjust = 0.5))+
      ylab("Car horsepower")+
      geom_line(color="red")+
      ylim(0,500)+ #### what is the best limit??????
      geom_point()
  });
  
})