require(shiny)
require(dplyr)
require(magrittr)
require(tidyr)
require(janitor)
require(ggthemes)
require(ggplot2)
theme_set(theme_bw())
require(stringr)
#################################################################################################################
ui <- fluidPage(
  titlePanel(h1('AirQo-Ugandan-Air-Quality-Forecast-Challenge', align = 'center')),
  #
  sidebarLayout(
    sidebarPanel(h3('Input Pane', align = 'center'),
                 selectInput(inputId = 'tempid', label = 'Reading', selected = 'temp_1',
                             selectize = TRUE,
                             choices = c("temp_1", "temp_2", "temp_3", "temp_4", "temp_5", "temp_6", "temp_7",
                                         "temp_8", "temp_9", "temp_10", "temp_11", "temp_12", "temp_13", 
                                         "temp_14", "temp_15", "temp_16", "temp_17", "temp_18", "temp_19", 
                                         "temp_20", "temp_21", "temp_22", "temp_23", "temp_24", "temp_25",
                                         "temp_26", "temp_27", "temp_28", "temp_29", "temp_30", "temp_31",
                                         "temp_32", "temp_33", "temp_34", "temp_35", "temp_36", "temp_37",
                                         "temp_38", "temp_39", "temp_40", "temp_41", "temp_42", "temp_43",
                                         "temp_44", "temp_45", "temp_46", "temp_47", "temp_48", "temp_49",         
                                         "temp_50", "temp_51", "temp_52", "temp_53", "temp_54", "temp_55",
                                         "temp_56", "temp_57", "temp_58", "temp_59", "temp_60", "temp_61",
                                         "temp_62", "temp_63", "temp_64", "temp_65", "temp_66", "temp_67",
                                         "temp_68", "temp_69", "temp_70", "temp_71", "temp_72", "temp_73",
                                         "temp_74", "temp_75", "temp_76", "temp_77", "temp_78", "temp_79",         
                                         "temp_80", "temp_81", "temp_82", "temp_83", "temp_84", "temp_85",
                                         "temp_86", "temp_87", "temp_88", "temp_89", "temp_90", "temp_91",
                                         "temp_92", "temp_93", "temp_94", "temp_95", "temp_96", "temp_97",
                                         "temp_98", "temp_99", "temp_100", "temp_101", "temp_102", "temp_103",
                                         "temp_104", "temp_105", "temp_106", "temp_107", "temp_108", "temp_109",
                                         "temp_110", "temp_111", "temp_112", "temp_113", "temp_114", "temp_115",
                                         "temp_116", "temp_117", "temp_118", "temp_119", "temp_120", "temp_121",
                                         ##################################################################
                                         "precip_1", "precip_2", "precip_3", "precip_4", "precip_5", 
                                         "precip_6", "precip_7", "precip_8", "precip_9", "precip_10", 
                                         "precip_11", "precip_12", "precip_13", "precip_14", "precip_15",
                                         "precip_16", "precip_17", "precip_18", "precip_19", "precip_20",
                                         "precip_21", "precip_22", "precip_23", "precip_24", "precip_25",
                                         "precip_26", "precip_27", "precip_28", "precip_29", "precip_30", 
                                         "precip_31", "precip_32", "precip_33", "precip_34", "precip_35",
                                         "precip_36", "precip_37", "precip_38", "precip_39", "precip_40", 
                                         "precip_41", "precip_42", "precip_43", "precip_44", "precip_45", 
                                         "precip_46", "precip_47", "precip_48", "precip_49", "precip_50",
                                         "precip_51", "precip_52", "precip_53", "precip_54", "precip_55",
                                         "precip_56", "precip_57", "precip_58", "precip_59", "precip_60", 
                                         "precip_61", "precip_62", "precip_63", "precip_64", "precip_65", 
                                         "precip_66", "precip_67", "precip_68", "precip_69", "precip_70",
                                         "precip_71", "precip_72", "precip_73", "precip_74", "precip_75",
                                         "precip_76", "precip_77", "precip_78", "precip_79", "precip_80", 
                                         "precip_81", "precip_82", "precip_83", "precip_84", "precip_85",
                                         "precip_86", "precip_87", "precip_88", "precip_89", "precip_90",
                                         "precip_91", "precip_92", "precip_93", "precip_94", "precip_95",
                                         "precip_96", "precip_97", "precip_98", "precip_99", "precip_100",
                                         "precip_101", "precip_102", "precip_103", "precip_104", "precip_105",
                                         "precip_106", "precip_107", "precip_108", "precip_109", "precip_110",
                                         "precip_111", "precip_112", "precip_113", "precip_114", "precip_115",
                                         "precip_116", "precip_117", "precip_118", "precip_119", "precip_120", 
                                         "precip_121",
                                         ####################################################################
                                         "rel_humidity_1", "rel_humidity_2", "rel_humidity_3", "rel_humidity_4",
                                         "rel_humidity_5", "rel_humidity_6", "rel_humidity_7", "rel_humidity_8",
                                         "rel_humidity_9", "rel_humidity_10", "rel_humidity_11", "rel_humidity_12", 
                                         "rel_humidity_13", "rel_humidity_14", "rel_humidity_15", "rel_humidity_16",
                                         "rel_humidity_17", "rel_humidity_18", "rel_humidity_19", "rel_humidity_20",
                                         "rel_humidity_21", "rel_humidity_22", "rel_humidity_23", "rel_humidity_24",
                                         "rel_humidity_25", "rel_humidity_26", "rel_humidity_27", "rel_humidity_28",
                                         "rel_humidity_29", "rel_humidity_30", "rel_humidity_31", "rel_humidity_32", 
                                         "rel_humidity_33", "rel_humidity_34", "rel_humidity_35", "rel_humidity_36", 
                                         "rel_humidity_37", "rel_humidity_38", "rel_humidity_39", "rel_humidity_40",
                                         "rel_humidity_41", "rel_humidity_42", "rel_humidity_43", "rel_humidity_44",
                                         "rel_humidity_45", "rel_humidity_46", "rel_humidity_47", "rel_humidity_48",
                                         "rel_humidity_49", "rel_humidity_50", "rel_humidity_51", "rel_humidity_52", 
                                         "rel_humidity_53", "rel_humidity_54", "rel_humidity_55", "rel_humidity_56",
                                         "rel_humidity_57", "rel_humidity_58", "rel_humidity_59", "rel_humidity_60", 
                                         "rel_humidity_61", "rel_humidity_62", "rel_humidity_63", "rel_humidity_64",
                                         "rel_humidity_65", "rel_humidity_66", "rel_humidity_67", "rel_humidity_68", 
                                         "rel_humidity_69", "rel_humidity_70", "rel_humidity_71", "rel_humidity_72", 
                                         "rel_humidity_73", "rel_humidity_74", "rel_humidity_75", "rel_humidity_76",
                                         "rel_humidity_77", "rel_humidity_78", "rel_humidity_79", "rel_humidity_80",
                                         "rel_humidity_81", "rel_humidity_82", "rel_humidity_83", "rel_humidity_84",
                                         "rel_humidity_85", "rel_humidity_86", "rel_humidity_87", "rel_humidity_88", 
                                         "rel_humidity_89", "rel_humidity_90", "rel_humidity_91", "rel_humidity_92", 
                                         "rel_humidity_93", "rel_humidity_94", "rel_humidity_95", "rel_humidity_96", 
                                         "rel_humidity_97", "rel_humidity_98", "rel_humidity_99", "rel_humidity_100",
                                         "rel_humidity_101", "rel_humidity_102", "rel_humidity_103", "rel_humidity_104",
                                         "rel_humidity_105", "rel_humidity_106", "rel_humidity_107", "rel_humidity_108",
                                         "rel_humidity_109", "rel_humidity_110", "rel_humidity_111", "rel_humidity_112",
                                         "rel_humidity_113", "rel_humidity_114", "rel_humidity_115", "rel_humidity_116",
                                         "rel_humidity_117", "rel_humidity_118", "rel_humidity_119", "rel_humidity_120",
                                         "rel_humidity_121",
                                         ##################################################################################
                                         "wind_dir_1", "wind_dir_2", "wind_dir_3", "wind_dir_4", "wind_dir_5", "wind_dir_6",      
                                         "wind_dir_7", "wind_dir_8", "wind_dir_9", "wind_dir_10", "wind_dir_11", "wind_dir_12",
                                         "wind_dir_13", "wind_dir_14", "wind_dir_15", "wind_dir_16", "wind_dir_17", "wind_dir_18",
                                         "wind_dir_19", "wind_dir_20", "wind_dir_21", "wind_dir_22", "wind_dir_23", "wind_dir_24",
                                         "wind_dir_25", "wind_dir_26", "wind_dir_27", "wind_dir_28", "wind_dir_29", "wind_dir_30",
                                         "wind_dir_31", "wind_dir_32", "wind_dir_33", "wind_dir_34", "wind_dir_35", "wind_dir_36",     
                                         "wind_dir_37", "wind_dir_38", "wind_dir_39", "wind_dir_40", "wind_dir_41", "wind_dir_42", 
                                         "wind_dir_43", "wind_dir_44", "wind_dir_45", "wind_dir_46", "wind_dir_47", "wind_dir_48", 
                                         "wind_dir_49", "wind_dir_50", "wind_dir_51", "wind_dir_52", "wind_dir_53", "wind_dir_54", 
                                         "wind_dir_55", "wind_dir_56", "wind_dir_57", "wind_dir_58", "wind_dir_59", "wind_dir_60", 
                                         "wind_dir_61", "wind_dir_62", "wind_dir_63", "wind_dir_64", "wind_dir_65", "wind_dir_66",     
                                         "wind_dir_67", "wind_dir_68", "wind_dir_69", "wind_dir_70", "wind_dir_71", "wind_dir_72", 
                                         "wind_dir_73", "wind_dir_74", "wind_dir_75", "wind_dir_76", "wind_dir_77", "wind_dir_78", 
                                         "wind_dir_79", "wind_dir_80", "wind_dir_81", "wind_dir_82", "wind_dir_83", "wind_dir_84", 
                                         "wind_dir_85", "wind_dir_86", "wind_dir_87", "wind_dir_88", "wind_dir_89", "wind_dir_90",
                                         "wind_dir_91", "wind_dir_92", "wind_dir_93", "wind_dir_94", "wind_dir_95", "wind_dir_96",     
                                         "wind_dir_97", "wind_dir_98", "wind_dir_99", "wind_dir_100", "wind_dir_101", "wind_dir_102",
                                         "wind_dir_103", "wind_dir_104", "wind_dir_105", "wind_dir_106", "wind_dir_107", "wind_dir_108",
                                         "wind_dir_109", "wind_dir_110", "wind_dir_111", "wind_dir_112", "wind_dir_113", "wind_dir_114",
                                         "wind_dir_115", "wind_dir_116", "wind_dir_117", "wind_dir_118", "wind_dir_119", "wind_dir_120",
                                         "wind_dir_121",
                                         ################################################################################################
                                         "wind_spd_1", "wind_spd_2", "wind_spd_3", "wind_spd_4", "wind_spd_5", "wind_spd_6", "wind_spd_7", 
                                         "wind_spd_8", "wind_spd_9", "wind_spd_10", "wind_spd_11", "wind_spd_12", "wind_spd_13", "wind_spd_14",
                                         "wind_spd_15", "wind_spd_16", "wind_spd_17", "wind_spd_18", "wind_spd_19", "wind_spd_20", "wind_spd_21",
                                         "wind_spd_22", "wind_spd_23", "wind_spd_24", "wind_spd_25", "wind_spd_26", "wind_spd_27", "wind_spd_28",
                                         "wind_spd_29", "wind_spd_30", "wind_spd_31", "wind_spd_32", "wind_spd_33", "wind_spd_34", "wind_spd_35",     
                                         "wind_spd_36", "wind_spd_37", "wind_spd_38", "wind_spd_39", "wind_spd_40", "wind_spd_41", "wind_spd_42", 
                                         "wind_spd_43", "wind_spd_44", "wind_spd_45", "wind_spd_46", "wind_spd_47", "wind_spd_48", "wind_spd_49",
                                         "wind_spd_50", "wind_spd_51", "wind_spd_52", "wind_spd_53", "wind_spd_54", "wind_spd_55", "wind_spd_56",
                                         "wind_spd_57", "wind_spd_58", "wind_spd_59", "wind_spd_60", "wind_spd_61", "wind_spd_62", "wind_spd_63",
                                         "wind_spd_64", "wind_spd_65", "wind_spd_66", "wind_spd_67", "wind_spd_68", "wind_spd_69", "wind_spd_70",    
                                         "wind_spd_71", "wind_spd_72", "wind_spd_73", "wind_spd_74", "wind_spd_75", "wind_spd_76", "wind_spd_77",
                                         "wind_spd_78", "wind_spd_79", "wind_spd_80", "wind_spd_81", "wind_spd_82", "wind_spd_83", "wind_spd_84",
                                         "wind_spd_85", "wind_spd_86", "wind_spd_87", "wind_spd_88", "wind_spd_89", "wind_spd_90", "wind_spd_91",
                                         "wind_spd_92", "wind_spd_93", "wind_spd_94", "wind_spd_95", "wind_spd_96", "wind_spd_97", "wind_spd_98",
                                         "wind_spd_99", "wind_spd_100", "wind_spd_101", "wind_spd_102", "wind_spd_103", "wind_spd_104", "wind_spd_105",    
                                         "wind_spd_106", "wind_spd_107", "wind_spd_108", "wind_spd_109", "wind_spd_110", "wind_spd_111", "wind_spd_112",
                                         "wind_spd_113", "wind_spd_114", "wind_spd_115", "wind_spd_116", "wind_spd_117", "wind_spd_118", "wind_spd_119",
                                         "wind_spd_120", "wind_spd_121",
                                         ##################################################################################################################
                                         "atmos_press_1", "atmos_press_2", "atmos_press_3", "atmos_press_4", "atmos_press_5", "atmos_press_6",
                                         "atmos_press_7", "atmos_press_8", "atmos_press_9", "atmos_press_10", "atmos_press_11", "atmos_press_12", 
                                         "atmos_press_13", "atmos_press_14", "atmos_press_15", "atmos_press_16", "atmos_press_17", "atmos_press_18",
                                         "atmos_press_19", "atmos_press_20", "atmos_press_21", "atmos_press_22", "atmos_press_23", "atmos_press_24",  
                                         "atmos_press_25", "atmos_press_26", "atmos_press_27", "atmos_press_28", "atmos_press_29", "atmos_press_30", 
                                         "atmos_press_31", "atmos_press_32", "atmos_press_33", "atmos_press_34", "atmos_press_35", "atmos_press_36",
                                         "atmos_press_37", "atmos_press_38", "atmos_press_39", "atmos_press_40", "atmos_press_41", "atmos_press_42",
                                         "atmos_press_43", "atmos_press_44", "atmos_press_45", "atmos_press_46", "atmos_press_47", "atmos_press_48",
                                         "atmos_press_49", "atmos_press_50", "atmos_press_51", "atmos_press_52", "atmos_press_53", "atmos_press_54",  
                                         "atmos_press_55", "atmos_press_56", "atmos_press_57", "atmos_press_58", "atmos_press_59", "atmos_press_60",
                                         "atmos_press_61", "atmos_press_62", "atmos_press_63", "atmos_press_64", "atmos_press_65", "atmos_press_66",
                                         "atmos_press_67", "atmos_press_68", "atmos_press_69", "atmos_press_70", "atmos_press_71", "atmos_press_72", 
                                         "atmos_press_73", "atmos_press_74", "atmos_press_75", "atmos_press_76", "atmos_press_77", "atmos_press_78",
                                         "atmos_press_79", "atmos_press_80", "atmos_press_81", "atmos_press_82", "atmos_press_83", "atmos_press_84",  
                                         "atmos_press_85", "atmos_press_86", "atmos_press_87", "atmos_press_88", "atmos_press_89", "atmos_press_90",
                                         "atmos_press_91", "atmos_press_92", "atmos_press_93", "atmos_press_94", "atmos_press_95", "atmos_press_96",
                                         "atmos_press_97", "atmos_press_98", "atmos_press_99", "atmos_press_100", "atmos_press_101", "atmos_press_102",
                                         "atmos_press_103", "atmos_press_104", "atmos_press_105", "atmos_press_106", "atmos_press_107", "atmos_press_108",
                                         "atmos_press_109", "atmos_press_110", "atmos_press_111", "atmos_press_112", "atmos_press_113", "atmos_press_114", 
                                         "atmos_press_115", "atmos_press_116", "atmos_press_117", "atmos_press_118", "atmos_press_119", "atmos_press_120",
                                         "atmos_press_121" )),
                 sliderInput(inputId = 'targetid', label = 'Target', min = 1, max = 500, value = c(30, 70))
                            ),
    #
    mainPanel(h2('Analyses of the Air Quality in Uganda', align = 'center'),
              h4('Climate Readings Analysis', align = 'center'),
              plotOutput('temperature'),
              h4('Air Quality per Loation for Diverse Climate Readings', align = 'center'),
              plotOutput('locplot'))
    ))
    
#################################################################################################################
server <- function(input, output) {
  temp_new <- paste("temp", seq(1, 121, 1))
  precip_new <- paste("precip", seq(1, 121, 1))
  humidity_new <- paste("rel_humidity", seq(1, 121, 1))
  wind_dir_new <- paste("wind_dir", seq(1, 121, 1))
  wind_spd_new <- paste("wind_spd", seq(1, 121, 1))
  atmos_new <- paste("atmos_press", seq(1, 121, 1))
  #
  trn <- Train %>% tidyr::separate(col = "temp", into = temp_new, sep = ",", convert = TRUE) %>% 
    tidyr::separate(col = "precip", into = precip_new, sep = ",", convert = TRUE) %>% 
    tidyr::separate(col = "rel_humidity", into = humidity_new, sep = ",", convert = TRUE) %>% 
    tidyr::separate(col = "wind_dir", into = wind_dir_new, sep = ",", convert = TRUE) %>% 
    tidyr::separate(col = "wind_spd", into = wind_spd_new, sep = ",", convert = TRUE) %>% 
    tidyr::separate(col = "atmos_press", into = atmos_new, sep = ",", convert = TRUE) %>%
    mutate(location = factor(location)) %>% select(-1) %>% 
    janitor::clean_names()
  #
  trn[] <- lapply(trn, function(x) { 
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x
    })
  ###################################################
  target_filter <- reactive({
    trn %>% filter(target >= input$targetid[1],
                   target <= input$targetid[2])
  })
  #
  #
  hist_plot <- function(col_id) {
    target_filter() %>% ggplot(aes(target_filter()[[col_id]]))+
      geom_bar(color = 'white', fill = 'steelblue')+
      scale_x_binned()+
      xlab(col_id)+
      ggtitle(col_id)+
      theme(axis.title = element_text(face = 'bold', size = 14),
            axis.title.x = element_text(face = 'bold', size = 12),
            axis.title.y = element_text(face = 'bold', size = 12),
            axis.text = element_text(size = 12))
  }
  #
  # histogram distributuions of the readings
  output$temperature <- renderPlot({
      hist_plot(input$tempid)
  })
  #
  output$locplot <- renderPlot({target_filter() %>% ggplot(aes(location, target_filter()[[input$tempid]]))+
      geom_bar(stat = 'identity', fill = 'steelblue')+
      ylab(input$tempid)+
      xlab('Location')+
      geom_text(aes(label = target_filter()[[input$tempid]]), vjust=1.6, color="white", size=3.5)+
      theme(axis.title = element_text(size = 14, face = 'bold'),
            axis.title.x = element_text(face = 'bold', size = 12),
            axis.title.y = element_text(face = 'bold', size = 12),
            axis.text = element_text(size = 12))
    })
  #
}
#################################################################################################################
shinyApp(ui, server)