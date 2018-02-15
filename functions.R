# Custom R Functions
# Written by James Harper, PE, ENV SP of the University of Colorado Boulder
# Started October 1, 2017
# Last updated February 13, 2018

categorical_analysis_1way = function(data, metric1) {
  
  # Create temporary vector from data
  A = data[metric1][[1]]
  
  # Perform categorical analyses and store results for printing below
  freqs = table(A)                                                       # Create frequency table
  freqs_prop = prop.table(freqs)                                         # Create proportions table
  freqs_prop_df = data.frame(Response = names(freqs_prop), 
                             Proportion = as.numeric(freqs_prop))
  freqs_prop_df = freqs_prop_df[order(-freqs_prop_df$Proportion),]
  
  # Create file name variable
  name = paste("freqs_1way_", names(data)[[metric1]], sep = "")
  
  # Start sending text output to text file
  file1 = file(paste(getwd(),"/output/", name, ".txt", sep = ""))
  sink(file1, append = TRUE)
  sink(file1, append = TRUE, type = "message")

  # Add title to text file
  print(paste("A = ", names(data)[[metric1]], sep = ""))

  # Print results of analyses to text file
  print(summary(freqs))
  print(freqs)
  print(freqs_prop_df)

  # Stop sending text output to file
  sink()
  sink(type = "message")
  closeAllConnections()
  
}

categorical_analysis_2way = function(data, metric1, metric2) {
  # Also used for stratified one-way categorical analysis
  
  # Create temporary vectors from data
  A = data[metric1][[1]]
  B = data[metric2][[1]]
  
  # Start sending text output to dump file
  file1 = file(paste(getwd(),"/Output/dump.txt", sep = ""))
  sink(file1, append = TRUE)
  sink(file1, append = TRUE, type = "message")
  
  # Perform categorical analyses
  freqs = table(A, B)                            # Create frequency table for stats
  # print(freqs)
  # freqs = freqs[order(-freqs[,1]),]              # Sort table by frequency in first column
  # print(freqs)
  # print(fisher.test(freqs))                      # Fisher Exact test
  chisq_cramv = assocstats(freqs)                # Calculate chi squared and Cramer's V
  
  # Stop sending text output to dump file
  sink()
  sink(type = "message")
  
  # Create file name and plot name variables that includes p-value and Cramer's V
  p_value = round(chisq_cramv$chisq_tests[2,3], digits = 3)
  chisqd = round(chisq_cramv$chisq_tests[2,1], digits = 3)
  cramer_v = round(chisq_cramv$cramer, digits = 3)
  name = paste("freqs_2way_", p_value, "_", chisqd, "_", cramer_v, "_", names(data)[[metric1]], "_", names(data)[[metric2]], sep = "")
  plot_name = paste(names(data)[[metric1]], "_", names(data)[[metric2]], "_", p_value, "_", chisqd, "_", cramer_v, sep = "")
  
  # Start sending text output to text file in a given folder based on p_values
  if (is.nan(p_value)) {
    
    # Create output folder
    folder = create_folder(subfolder = "p is NaN")
    
    # Start sending text output to text file in folder
    file1 = file(paste(folder, "/", name, ".txt", sep = ""))
    sink(file1, append = TRUE)
    sink(file1, append = TRUE, type = "message")
    
  } else if (p_value > 0.05) {
    
    # Create output folder
    folder = create_folder(subfolder = "p above 0.05")
    
    # Start sending text output to text file in folder
    file1 = file(paste(folder, "/", name, ".txt", sep = ""))
    sink(file1, append = TRUE)
    sink(file1, append = TRUE, type = "message")
    
  } else {
    
    # Create output folder
    folder = create_folder(subfolder = "")
    
    # Start sending text output to text file in folder
    file1 = file(paste(folder, "/", name, ".txt", sep = ""))
    sink(file1, append = TRUE)
    sink(file1, append = TRUE, type = "message")
    
  }
  
  # Add title to text file
  print(paste("A = ", names(data)[[metric1]], sep = ""))
  print(paste("B = ", names(data)[[metric2]], sep = ""))
  
  # Print results of analyses to text file
  print(CrossTable(A, B))
  # print(ftable(freqs))
  print(summary(freqs))
  print(chisq_cramv)
  
  # Stop sending text output to file
  sink()
  sink(type = "message")
  
  # Start saving plot to PDF in a given folder based on p_values
  if (is.nan(p_value)) {
    folder = create_folder(subfolder = "p is NaN")       # Create output folder
    pdf(paste(folder, "/", name, ".pdf", sep = ""))      # Start saving plot to PDF in folder
  } else if (p_value > 0.05) {
    folder = create_folder(subfolder = "p above 0.05")   # Create output folder
    pdf(paste(folder, "/", name, ".pdf", sep = ""))      # Start saving plot to PDF in folder
  } else {
    folder = create_folder(subfolder = "")               # Create output folder
    pdf(paste(folder, "/", name, ".pdf", sep = ""))      # Start saving plot to PDF in folder
  }
  
  # Generate categorical analysis plots
  if (length(freqs[,1]) < 50) {
    mosaic(freqs, shade = TRUE, legend = TRUE, main = plot_name)          # Mosaic plot
  } else {
    mosaic(freqs, shade = TRUE, legend = TRUE, main = plot_name)          # Mosaic plot
    # barplot(table(B,A), main=name, legend = colnames(freqs))       # Stacked bar plot with legend
  }
  
  # Stop sending plot output to file
  dev.off()
  closeAllConnections()
  
}

categorical_analysis_3way = function(data, metric1, metric2, metric3) {
  
  # Create temporary vectors from data
  A = data[metric1][[1]]
  B = data[metric2][[1]]
  C = data[metric3][[1]]
  
  # Start sending text output to dump file
  file1 = file(paste(getwd(),"/Output/dump.txt", sep = ""))
  sink(file1, append = TRUE)
  sink(file1, append = TRUE, type = "message")
  
  # Perform categorical analyses
  freqs = table(A, B, C)                            # Create frequency table for stats
  freqs_prop = prop.table(freqs, 3)
  # freqs = freqs[order(-freqs[,1]),]              # Sort table by frequency in first column
  # print(fisher.test(freqs))                      # Fisher Exact test
  chisq_cramv = assocstats(freqs)                # Calculate chi squared and Cramer's V
  
  # Stop sending text output to dump file
  sink()
  sink(type = "message")
  
  # Create file name and plot name variables that includes first 2 p-values
  p_value1 = round(chisq_cramv[[1]]$chisq_tests[2,3], digits = 3)
  p_value2 = round(chisq_cramv[[2]]$chisq_tests[2,3], digits = 3)
  name = paste("freqs_3way_", p_value1, "_", p_value2, "_", names(data)[[metric3]], "_", names(data)[[metric1]], 
               "_", names(data)[[metric2]], sep = "")
  plot_name = paste(names(data)[[metric1]], "_", names(data)[[metric2]], "_", names(data)[[metric3]], sep = "")
  
  # Start sending text output to text file in a given folder based on p_values
  if (is.nan(p_value1) || is.nan(p_value2)) {
    
    # Create output folder
    folder = create_folder(subfolder = "p is NaN")
    
    # Start sending text output to text file in folder
    file1 = file(paste(folder, "/", name, ".txt", sep = ""))
    sink(file1, append = TRUE)
    sink(file1, append = TRUE, type = "message")
    
  } else if (p_value1 > 0.05 || p_value2 > 0.05) {
    
    # Create output folder
    folder = create_folder(subfolder = "p above 0.05")
    
    # Start sending text output to text file in folder
    file1 = file(paste(folder, "/", name, ".txt", sep = ""))
    sink(file1, append = TRUE)
    sink(file1, append = TRUE, type = "message")
    
  } else {
    
    # Create output folder
    folder = create_folder(subfolder = "")
    
    # Start sending text output to text file in folder
    file1 = file(paste(folder, "/", name, ".txt", sep = ""))
    sink(file1, append = TRUE)
    sink(file1, append = TRUE, type = "message")
    
  }
  
  # Add title to text file
  print(paste("A = ", names(data)[[metric1]], sep = ""))
  print(paste("B = ", names(data)[[metric2]], sep = ""))
  print(paste("C = ", names(data)[[metric3]], sep = ""))
  
  # Print results of analyses to text file
  print(ftable(freqs_prop))
  print(ftable(freqs))
  print(summary(freqs))
  print(chisq_cramv)
  
  # Stop saving text output to file
  sink()
  sink(type = "message")
  
  # Start saving plot to PDF in a given folder based on p_values
  if (is.nan(p_value1) || is.nan(p_value2)) {
    folder = create_folder(subfolder = "p is NaN")       # Create output folder
    pdf(paste(folder, "/", name, ".pdf", sep = ""))      # Start saving plot to PDF in folder
  } else if (p_value1 > 0.05 || p_value2 > 0.05) {
    folder = create_folder(subfolder = "p above 0.05")   # Create output folder
    pdf(paste(folder, "/", name, ".pdf", sep = ""))      # Start saving plot to PDF in folder
  } else {
    folder = create_folder(subfolder = "")               # Create output folder
    pdf(paste(folder, "/", name, ".pdf", sep = ""))      # Start saving plot to PDF in folder
  }
  
  # Create dataframe from data
  data_frame = data.frame(A, B, C)
  names(data_frame) = c(names(data)[[metric1]], names(data)[[metric2]], names(data)[[metric3]])
  
  # Generate categorical analysis plots
  scpcp(data_frame, sel = "data[,3]")
  mosaic(freqs, shade = TRUE, legend = TRUE, main = plot_name)          # Mosaic plot
  rmb(freqs)                                                            # RMB plot
  rmbmat(freqs, tv = 1)                                                 # RMB plot matrix
  fluctile(freqs)                                                       # Fluctuation diagram
  barplot(table(B,A), main = name, legend = colnames(freqs))            # Stacked bar plot with legend
  
  # Stop saving plot to PDF
  dev.off()
  closeAllConnections()
  
}

correspondence_analysis_2way = function(data, metric1, metric2) {

  # Create temporary vectors and name variable from data
  A = data[metric1][[1]]
  B = data[metric2][[1]]
  name = paste(names(data)[[metric1]], "___", names(data)[[metric2]], sep = "")
  
  # Start sending text output to text file
  # file1 = file(paste(getwd(),"/Output/", name, ".txt", sep = ""))
  # sink(file1, append=TRUE)
  # sink(file1, append=TRUE, type="message")
  
  # Add title to text file
  print(paste("A = ", names(data)[[metric1]], sep = ""))
  print(paste("B = ", names(data)[[metric2]], sep = ""))
  
  # Perform correspondence analyses
  CrossTable(A, B)                  # 2-way contingency table
  freqs = table(A, B)              # Generate frequency table for stats
  print(freqs)
  prop.table(freqs, 1) # row percentages
  prop.table(freqs, 2) # column percentages
  fit = ca(freqs)
  print(fit) # basic results 
  summary(fit) # extended results
  
  # Stop sending text output to file
  # sink()
  # sink(type="message")
  
  # Start sending plot output to file
  # pdf(paste(getwd(),"/Output/", name, ".pdf", sep = ""))
  
  # Generate correspondence analysis plots
  plot(fit) # symmetric map
  plot(fit, mass = TRUE, contrib = "absolute", map =
         "rowgreen", arrows = c(FALSE, TRUE)) # asymmetric map
  
  # Stop sending plot output to file
  # dev.off()
  # 
  # closeAllConnections()
}

multiple_correspondence_analysis = function(data, quali_sup, quanti_sup) {
  library(FactoMineR)
  results = MCA(data, quali.sup = quali_sup, quanti.sup = quanti_sup)
  # summary(results, ncp=3, nbelements = Inf)
  # dimdesc(results)
  pdf(paste(getwd(),"/Output/plot.pdf", sep = ""))
  # plot(results, label=c("var","quali.sup"), cex=0.7)
  # plot(results, invisible=c("var","quali.sup"), cex=0.7)
  plot(results, invisible = c("ind","quali.sup"), autoLab = "y", cex = 0.7, title = "Active Categories")
  # plot(results, invisible=c("ind","quali.sup"), autoLab="y", cex=0.7, title="Active Categories", selectMod = "contrib 20")
  # plot(results, invisible=c("ind","quali.sup"), cex=0.7, title="Active Categories")
  # plot(results, invisible=c("ind","var"), autoLab="y", cex=0.7, title="Supplementary Categories")
  # plot(results, invisible="ind", autoLab = "y", cex=0.7, selectMod = "cos2 10")
  # plot(results, invisible="ind", autoLab = "y", cex=0.7, selectMod = "contrib 20")
  # plot(results, invisible=c("var","quali.sup"), autoLab = "y", cex=0.7, select = "cos2 10")
  # plot(results, autoLab = "y", cex=0.7, selectMod = "cos2 20", select = "cos2 10")
  # plot(results, choix = "var", xlim=c(0,0.6), ylim=c(0,0.6))
  # plot(results, choix = "var", xlim=c(0,0.6), ylim=c(0,0.6), invisible = c("ind","quali.sup"))
  # plot(results, invisible = c("var","quali.sup"), cex=0.7, select="contrib 20", axes=3:4)
  # plot(results, invisible = c("ind"), cex=0.7, select="contrib 20", axes=3:4)
  # plotellipses(results, keepvar=c(1:4))
  dev.off()
  closeAllConnections()
}

save_text_output_to_file = function(subfolder, name) {

  # Name folder to store output
  folder = paste(getwd(),"/Output/", subfolder, sep = "")
  
  # Create folder if it does not exist
  if (!dir.exists(folder)) {
    dir.create(folder)
  }
  
  
  
}

create_folder = function(subfolder) {
  
  # Name folder to store output
  folder = paste(getwd(),"/Output/", subfolder, sep = "")
  
  # Create folder if it does not exist
  if (!dir.exists(folder)) {
    dir.create(folder)
  }
  
  return(folder)
  
}

sleep_time = function(seconds) {
  p1 = proc.time()
  Sys.sleep(seconds)
  proc.time() - p1 # The cpu usage should be negligible
}

load_libraries = function(libs) {
  for (p in libs) {
    if (!require(p, character.only = TRUE)) {
      install.packages(p)
      require(p, character.only = TRUE)
    }
  }
}
