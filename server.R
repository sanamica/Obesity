

library(shiny)
library()

shinyServer(function(input, output) {
   labels <-
     list("PACCESS" ="Low access to grocery store %",
                      "PACCESS_I" = "Low access to grocery store & lowincome %",
                      "PLACCESS_HHNV" = "Household no car & low access to grocery store %",
                      "PLACCESS_SNAP" = "SNAP households, low access to grocery store %",
                      "PLACCESSWHITE" = "White, low access to grocery store %",
                      "PLACCESSBLACK" = "Black, low access to grocery store %",
                      "PLACCESSHISP" = "Hispanic, low access to grocery store %",
                      "PLACCESSNHAASIAN" = "Asian, low access to grocery store %",
                      "PLACESSNHA" ="American Indian or Alaska Native, low access to store %",
                      "PACCESSPNHI" = "Hawaiian or Pacific Islander, low access to store %",
                      "GROC14"= "Number of Grocery stores/1,000 pop" ,
                      "SUPERC14" = "Supercenters & club stores/1,000 pop" ,
                      "CONVS14" = "Convenience stores/1,000 pop",
                      "FFRPTH14" = "Fast-food restaurants/1,000 pop",
                      "FSRPTH14" = "Full-service restaurants/1,000 pop",
                      "PDIABETES" = "Adult diabetes rate" ,
                      "RECFACPTH14" = "Recreation & fitness facilities", 
                      "MEDHHINC15" = "Median household income" ,
                      "POVRATE15" = "Poverty rate" 
          )
   
   output$obesity_map<- renderLeaflet({
     # leaflet() %>% addTiles() %>% setView(42, 16, 4)
      var <- "POBESE"
      tm <- tm_shape(county_shape,projection = 2163) +

       tm_fill(var, midpoint = 0,
               n=9,
               palette = "Set1",
               border.col = "black",
               title='Obesity Rate',
               border.alpha = .5,
               id = "NAME",
               textNA = 'Unreliable',
               colorNA = "grey",
               alpha= 1,
               popup.vars = c(
                 "Low access to grocery store & lowincome %" = "PACCESS_I",
                 "SNAP households, low access to grocery store %" = "PLACCESS_SNAP",
                 "White, low access to grocery store %" = "PLACCESSWHITE",
                 "Black, low access to grocery store %" = "PLACCESSBLACK",
                 "Hispanic, low access to grocery store %" = "PLACCESSHISP",
                 "Asian, low access to grocery store %" = "PLACCESSNHAASIAN",
                 "Number of Grocery stores/1,000 pop" = "GROC14",
                 "Fast-food restaurants/1,000 pop" = "FFRPTH14",
                 "Adult diabetes rate" = "PDIABETES",
                 "Recreation & fitness facilities" = "RECFACPTH14",
                 "Median household income" = "MEDHHINC15",
                 "Poverty rate" ="POVRATE15"
               ))+

       tm_polygons(col = var,

                   style = "quantile",           #bin selecion
                   border.col = "white",
                   border.alpha = 0.5,

       ) +

       tm_legend(legend.position = c("left","bottom")) +
       tm_layout(title = "Obesity Rate per County in U.S.A",
                 title.size = 1.1,
                 title.position = c("center", "top"),
                 inner.margins = c(0.06, 0.10, 0.10, 0.08)) +
       tm_shape(states) +
       tm_style("natural")+
       tm_borders(lwd = 2,col = "black", alpha =.25)

     tmap_leaflet(tm) 
   })
   
   # %>% addTiles() %>% setView(42, 16, 4)

    output$obesity_county<- renderPlotly({

        o_c <- usda %>%
            filter(State==input$State) %>%
            ggplot(aes(x=fct_reorder(County,POBESE, .desc = FALSE), y=POBESE, fill =obesity_rate))+
          geom_bar(stat = "identity") +
          coord_flip()+ aes(text = paste("County:", County, "Obesity%:", POBESE) )+
            labs(x= "", y = "Obesity Rate", fill = "Obesity Rate") +
          #geom_hline(aes(yintercept = mean(POBESE))) (need to get median line)
          scale_y_continuous(limits = c(0,50), expand = c(0, 0)) +
          theme(axis.text.y =element_blank(),
                rect = element_blank())
            ggplotly(o_c, tooltip = "text") %>%
              config(displayModeBar = F)


    })   #obesity_county
    
    output$obesity_state <- renderPlotly({
      p <- ggplot(usda_state_w,aes(x=fct_reorder(State, w_POBESE,.desc = FALSE), y=w_POBESE, fill = region)) +
        geom_bar(stat = "identity") + 
         coord_flip()+
        labs(x= "", y = "Obesity Rate", colors = "Obesity Rate")  +
         
        scale_y_continuous(limits = c(0,40), expand = c(0, 0)) +
        theme(axis.text.y =element_blank(),
              rect = element_blank(),
              plot.title = element_text(size = 16), 
              axis.title = element_text(size = 14),
              axis.text = element_text(size = 14),
              legend.text = element_text(size = 14),
              legend.key.size = unit(1.5,"line"))
       
          
      ggplotly(p)
    })  #obesity_state
    
    output$coorelation_plot <- renderPlotly({
      
      # p_cor <- usda %>%
      #   filter(State== input$State_2) %>%
      #   ggscatter(x=input$Select, y = "POBESE",
      #             add = "reg.line", conf.int = TRUE, color="obesity_rate", pallette = "jco") +
      #   stat_cor(aes(color = obesity_rate), label.x = 3) +
      #   geom_point(alpha=.40)
      # 
      # ggplotly(p_cor)
      # 
      # p_obesity <- usda %>%
      #   filter(State==input$State_2) %>%
      # 
      # ggplot(aes(x=input$Select, y=POBESE, color = obesity_rate))+
      #     labs(x=  input$State, y = "Obesity Rate") +
      #     stat_smooth(method = "lm",
      #                 color = "red",
      #                 se = FALSE,
      #                 size = 1) +
      #     geom_point(alpha =0.8, position = "jitter") +
      #     theme(text = element_text(size = 14),
      #           rect = element_blank(),
      #           panel.grid =element_blank(),
      #           title = element_text (color = "#8b0000" ),
      #          )

       # ggplotly(p_obesity)
       
       p_obesity <- usda %>% 
      filter(State==input$State_2) %>% 
       
       ggplot(aes(x=get(input$Select), y=POBESE, color = obesity_rate ))+
         labs(x = as.character(labels[input$Select]), y = "Obesity Rate",color = "Obesity Rate") +
                 # xlab(input$labels)+
         geom_smooth(method = "lm",
                     color = "black",
                     se = TRUE,
                     size = 1)+
         geom_point(alpha =0.7, size = 3, position = "jitter",aes(text = paste("County:",County ))) +
         theme_minimal() 
        
       
       ggplotly(p_obesity)
    })
    

    
    # axis.line = element_line(color = "grey"
        
        
  
}
)


