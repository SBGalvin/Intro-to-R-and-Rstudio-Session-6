## Seafoam Green Standard Score Plot

standardScorePlot1     <- function(lb,  # scale lower bound 
                                   ub,  # scale upper bound 
                                   lb1, # Lower bound for s region 1
                                   ub1, # Upper bound for s region 1
                                   lb2, # Lower bound for s region 2
                                   ub2, # Upper bound for s region 2
                                   lb3, # Lower bound for s region 3
                                   ub3, # Upper bound for s region 3
                                   lb4, # Lower value for mean line
                                   ub4, #  upper value for mean line
                                   mean = 100, 
                                   sd = 15, 
                                   limits = c(mean - 4 * sd, mean + 4 * sd)) {
  # generates sequence of data points between limits using inputs
  x <- seq(limits[1], # vector position call
           limits[2], # vector position call
           length.out = 100)
  # Scale limits
  # Area 0 Average region
  x0min  <- max(lb, limits[1])   # defines xmin
  x0max  <- min(ub, limits[2])   # defines xmax
  areax0 <- seq(x0min, x0max, length.out = 100)
  area0  <- data.frame(x = areax0, ymin = 0, 
                       ymax = dnorm(areax0, mean = mean, sd = sd))
  
  # Area 1 High/Low Average regions
  x1min  <- max(lb1, limits[1])
  x1max  <- min(ub1, limits[2])
  areax1 <- seq(x1min, x1max, length.out = 100)
  area1  <- data.frame(x = areax1, ymin = 0, 
                       ymax = dnorm(areax1, mean = mean, sd = sd))
  
  # Area 2 Borderline/ Superior regions
  x2min  <- max(lb2, limits[1])
  x2max  <- min(ub2, limits[2])
  areax2 <- seq(x2min, x2max, length.out = 100)
  area2  <- data.frame(x = areax2, ymin = 0, 
                       ymax = dnorm(areax2, mean = mean, sd = sd))
  
  # Area 3 H
  x3min  <- max(lb3, limits[1])
  x3max  <- min(ub3, limits[2])
  areax3 <- seq(x3min, x3max, length.out = 100)
  area3  <- data.frame(x = areax3, ymin = 0, 
                       ymax = dnorm(areax3, mean = mean, sd = sd))
  
  # Area 4 Mean line
  x4min  <- max(lb4, limits[1])
  x4max  <- min(ub4, limits[2])
  areax4 <- seq(x4min, x4max, length.out = 100)
  area4  <- data.frame(x = areax4, ymin = 0, 
                       ymax = dnorm(areax4, mean = mean, sd = sd))
  
  ### Labels for X axis
  # 20 break points 
  # First and last points are left empty: No Score in this region
  
  # Percentile labels
  p <- c(" ", 
         "0.1%", "0.4%", "1%", "2.3%", "5%",
         "9%", "16%", "25%", "37%", "50%", 
         "63%","75%","84%", "91%", "95%", 
         "97.7%", "99%", "99.6%", "99.9%",
         " ")
  
  # Standard Scores
  s <- c(" ", 
         "55", "60", "65", "70", "75", 
         "80", "85", "90", "95", "100", 
         "105", "110", "115", "120", "125", 
         "130", "135", "140", "145", 
         " ")
  
  # Raw Scores
  r  <- c(" ", 
          "1-22", "23-25", "26-27", "28", "29", 
          "30-31", "32-33", "34-35", "36-37", "38",
          "39", "40-42", "43-45", "46-47", "48",
          "49", "50-51", "52-54", "55-60",
          " ")
  
  # Paste the Raw Score, Standard Score and Percentile together
  s1 <- paste(r, s, p, sep = "\n")
  
  # Call the plot function
  (ggplot() + 
      geom_line(data.frame(x = x, y = dnorm(x, mean = mean, sd = sd)),
                mapping = aes(x = x, y = y), 
                colour = "grey22") + 
      # Area 0
      geom_ribbon(data = area0, 
                  mapping = aes(x = x, ymin = ymin, ymax = ymax),
                  fill = "#49796b", 
                  colour = "grey22")+
      # Area 1
      geom_ribbon(data = area1, 
                  mapping = aes(x = x, ymin = ymin, ymax = ymax),
                  fill = "#5f9ea0", 
                  colour = "grey22")+
      # Area 2
      geom_ribbon(data = area2, 
                  mapping = aes(x = x, ymin = ymin, ymax = ymax),
                  fill = "#a0d6b4", 
                  colour = "grey22")+
      # Area 3
      geom_ribbon(data = area3, 
                  mapping = aes(x = x, ymin = ymin, ymax = ymax),
                  fill = "white", 
                  colour = "grey22") +
      # Area 4
      geom_ribbon(data = area4, 
                  mapping = aes(x = x, ymin = ymin, ymax = ymax),
                  fill = "grey22", 
                  colour = "grey22") +
      # Norm curve outline
      geom_line(data.frame(x = x, y = dnorm(x, mean = mean, sd = sd)),
                mapping = aes(x = x, y = y), 
                colour = "grey22")+
      # remove legend
      theme(legend.position="none")+
      # Scale limits
      scale_x_continuous(limits = limits,
                         breaks = c(45, 55, 60, 65, 70, 
                                    75, 80, 85, 90, 95,
                                    100, 105, 110, 115, 120, 
                                    125, 130, 135, 140, 145, 
                                    155),
                         labels = s1)+
      xlab("Raw Score\nStandard Score\nPercentile"))
}


## Seafoam Green Standard Score Plot with Qual Markings

standardScorePlot2     <- function(lb,  # scale lower bound 
                                   ub,  # scale upper bound 
                                   lb1, # Lower bound for s region 1
                                   ub1, # Upper bound for s region 1
                                   lb2, # Lower bound for s region 2
                                   ub2, # Upper bound for s region 2
                                   lb3, # Lower bound for s region 3
                                   ub3, # Upper bound for s region 3
                                   lb4, # Lower value for mean line
                                   ub4, #  upper value for mean line
                                   mean = 100, 
                                   sd = 15, 
                                   limits = c(mean - 4 * sd, mean + 4 * sd)) {
  # generates sequence of data points between limits using inputs
  x <- seq(limits[1], # vector position call
           limits[2], # vector position call
           length.out = 100)
  # Scale limits
  # Area 0 Average region
  x0min  <- max(lb, limits[1])   # defines xmin
  x0max  <- min(ub, limits[2])   # defines xmax
  areax0 <- seq(x0min, x0max, length.out = 100)
  area0  <- data.frame(x = areax0, ymin = 0, 
                       ymax = dnorm(areax0, mean = mean, sd = sd))
  
  # Area 1 High/Low Average regions
  x1min  <- max(lb1, limits[1])
  x1max  <- min(ub1, limits[2])
  areax1 <- seq(x1min, x1max, length.out = 100)
  area1  <- data.frame(x = areax1, ymin = 0, 
                       ymax = dnorm(areax1, mean = mean, sd = sd))
  
  # Area 2 Borderline/ Superior regions
  x2min  <- max(lb2, limits[1])
  x2max  <- min(ub2, limits[2])
  areax2 <- seq(x2min, x2max, length.out = 100)
  area2  <- data.frame(x = areax2, ymin = 0, 
                       ymax = dnorm(areax2, mean = mean, sd = sd))
  
  # Area 3 H
  x3min  <- max(lb3, limits[1])
  x3max  <- min(ub3, limits[2])
  areax3 <- seq(x3min, x3max, length.out = 100)
  area3  <- data.frame(x = areax3, ymin = 0, 
                       ymax = dnorm(areax3, mean = mean, sd = sd))
  
  # Area 4 Mean line
  x4min  <- max(lb4, limits[1])
  x4max  <- min(ub4, limits[2])
  areax4 <- seq(x4min, x4max, length.out = 100)
  area4  <- data.frame(x = areax4, ymin = 0, 
                       ymax = dnorm(areax4, mean = mean, sd = sd))
  
  ### Labels for X axis
  # 20 break points 
  # First and last points are left empty: No Score in this region
  
  # Percentile labels
  p <- c(" ", 
         "0.1%", "0.4%", "1%", "2.3%", "5%",
         "9%", "16%", "25%", "37%", "50%", 
         "63%","75%","84%", "91%", "95%", 
         "97.7%", "99%", "99.6%", "99.9%",
         " ")
  
  # Standard Scores
  s <- c(" ", 
         "55", "60", "65", "70", "75", 
         "80", "85", "90", "95", "100", 
         "105", "110", "115", "120", "125", 
         "130", "135", "140", "145", 
         " ")
  
  # Raw Scores
  r  <- c(" ", 
          "1-22", "23-25", "26-27", "28", "29", 
          "30-31", "32-33", "34-35", "36-37", "38",
          "39", "40-42", "43-45", "46-47", "48",
          "49", "50-51", "52-54", "55-60",
          " ")
  
  # Paste the Raw Score, Standard Score and Percentile together
  s1 <- paste(r, s, p, sep = "\n")
  
  # Qual Label Data
  SPM_Q  <- data.frame( x1 = c(55, 70, 80, 90, 110, 120, 130),
                        x2 = c(70, 80, 90, 110, 120, 130, 145),
                        y1 = c(-0.006, -0.006, -0.006, -0.006, -0.006, -0.006, -0.006),
                        y2 = c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00),
                        Qual = c("Extremely Low", "Borderline", "Low Average", 
                                 "Average",
                                 "High Average", "Superior", "Very Superior"),
                        type = c("a", "a", "a",
                                 "b",
                                 "a", "a", "a"))
  
  SPM_Q$Qual  <- factor(SPM_Q$Qual, 
                        levels = c("Extremely Low", "Borderline", "Low Average",
                                   "Average",
                                   "High Average", "Superior", "Very Superior"))
  
  SPM_Q$type  <- factor(SPM_Q$type,
                        levels = c("a", "b"))
  
  # Call the plot function
  (ggplot() + 
      geom_line(data.frame(x = x, y = dnorm(x, mean = mean, sd = sd)),
                mapping = aes(x = x, y = y), 
                colour = "grey22") + 
      # Area 0
      geom_ribbon(data = area0, 
                  mapping = aes(x = x, ymin = ymin, ymax = ymax),
                  fill = "#49796b", 
                  colour = "grey22")+
      # Area 1
      geom_ribbon(data = area1, 
                  mapping = aes(x = x, ymin = ymin, ymax = ymax),
                  fill = "#5f9ea0", 
                  colour = "grey22")+
      # Area 2
      geom_ribbon(data = area2, 
                  mapping = aes(x = x, ymin = ymin, ymax = ymax),
                  fill = "#a0d6b4", 
                  colour = "grey22")+
      # Area 3
      geom_ribbon(data = area3, 
                  mapping = aes(x = x, ymin = ymin, ymax = ymax),
                  fill = "white", 
                  colour = "grey22") +
      # Area 4
      geom_ribbon(data = area4, 
                  mapping = aes(x = x, ymin = ymin, ymax = ymax),
                  fill = "grey22", 
                  colour = "grey22") +
      # Norm curve outline
      geom_line(data.frame(x = x, y = dnorm(x, mean = mean, sd = sd)),
                mapping = aes(x = x, y = y), 
                colour = "grey22")+
      # Qualitative layer
      geom_rect(data = SPM_Q, 
                mapping = aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=Qual),
                colour = "grey22")+
      scale_fill_manual(values =  c("#49796b", "#5f9ea0", "#a0d6b4", 
                                    "white",
                                    "#a0d6b4", "#5f9ea0", "#49796b"))+
      geom_text(data = SPM_Q, 
                aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, 
                    label=Qual, colour = type), 
                size=2) +
      scale_colour_manual(values = c("white", "black"))+
      theme(legend.position="none")+
      # Scale limits
      scale_x_continuous(limits = limits,
                         breaks = c(45, 55, 60, 65, 70, 
                                    75, 80, 85, 90, 95,
                                    100, 105, 110, 115, 120, 
                                    125, 130, 135, 140, 145, 
                                    155),
                         labels = s1)+
      xlab("Raw Score\nStandard Score\nPercentile"))
}

## GreyScale  Standard Score Plot

standardScorePlot_Grey1<- function(lb,  # scale lower bound 
                                   ub,  # scale upper bound 
                                   lb1, # Lower bound for s region 1
                                   ub1, # Upper bound for s region 1
                                   lb2, # Lower bound for s region 2
                                   ub2, # Upper bound for s region 2
                                   lb3, # Lower bound for s region 3
                                   ub3, # Upper bound for s region 3
                                   lb4, # Lower value for mean line
                                   ub4, #  upper value for mean line
                                   mean = 100, 
                                   sd = 15, 
                                   limits = c(mean - 4 * sd, mean + 4 * sd)) {
  # generates sequence of data points between limits using inputs
  x <- seq(limits[1], # vector position call
           limits[2], # vector position call
           length.out = 100)
  # Scale limits
  # Area 0 Average region
  x0min  <- max(lb, limits[1])   # defines xmin
  x0max  <- min(ub, limits[2])   # defines xmax
  areax0 <- seq(x0min, x0max, length.out = 100)
  area0  <- data.frame(x = areax0, ymin = 0, 
                       ymax = dnorm(areax0, mean = mean, sd = sd))
  
  # Area 1 High/Low Average regions
  x1min  <- max(lb1, limits[1])
  x1max  <- min(ub1, limits[2])
  areax1 <- seq(x1min, x1max, length.out = 100)
  area1  <- data.frame(x = areax1, ymin = 0, 
                       ymax = dnorm(areax1, mean = mean, sd = sd))
  
  # Area 2 Borderline/ Superior regions
  x2min  <- max(lb2, limits[1])
  x2max  <- min(ub2, limits[2])
  areax2 <- seq(x2min, x2max, length.out = 100)
  area2  <- data.frame(x = areax2, ymin = 0, 
                       ymax = dnorm(areax2, mean = mean, sd = sd))
  
  # Area 3 H
  x3min  <- max(lb3, limits[1])
  x3max  <- min(ub3, limits[2])
  areax3 <- seq(x3min, x3max, length.out = 100)
  area3  <- data.frame(x = areax3, ymin = 0, 
                       ymax = dnorm(areax3, mean = mean, sd = sd))
  
  # Area 4 Mean line
  x4min  <- max(lb4, limits[1])
  x4max  <- min(ub4, limits[2])
  areax4 <- seq(x4min, x4max, length.out = 100)
  area4  <- data.frame(x = areax4, ymin = 0, 
                       ymax = dnorm(areax4, mean = mean, sd = sd))
  
  ### Labels for X axis
  # 20 break points 
  # First and last points are left empty: No Score in this region
  
  # Percentile labels
  p <- c(" ", 
         "0.1%", "0.4%", "1%", "2.3%", "5%",
         "9%", "16%", "25%", "37%", "50%", 
         "63%","75%","84%", "91%", "95%", 
         "97.7%", "99%", "99.6%", "99.9%",
         " ")
  
  # Standard Scores
  s <- c(" ", 
         "55", "60", "65", "70", "75", 
         "80", "85", "90", "95", "100", 
         "105", "110", "115", "120", "125", 
         "130", "135", "140", "145", 
         " ")
  
  # Raw Scores
  r  <- c(" ", 
          "1-22", "23-25", "26-27", "28", "29", 
          "30-31", "32-33", "34-35", "36-37", "38",
          "39", "40-42", "43-45", "46-47", "48",
          "49", "50-51", "52-54", "55-60",
          " ")
  
  # Paste the Raw Score, Standard Score and Percentile together
  s1 <- paste(r, s, p, sep = "\n")
  
  # Call the plot function
  (ggplot() + 
      geom_line(data.frame(x = x, y = dnorm(x, mean = mean, sd = sd)),
                mapping = aes(x = x, y = y), 
                colour = "grey22") + 
      # Area 0
      geom_ribbon(data = area0, 
                  mapping = aes(x = x, ymin = ymin, ymax = ymax),
                  fill = "grey28", 
                  colour = "grey22")+
      # Area 1
      geom_ribbon(data = area1, 
                  mapping = aes(x = x, ymin = ymin, ymax = ymax),
                  fill = "grey42", 
                  colour = "grey22")+
      # Area 2
      geom_ribbon(data = area2, 
                  mapping = aes(x = x, ymin = ymin, ymax = ymax),
                  fill = "grey66", 
                  colour = "grey22")+
      # Area 3
      geom_ribbon(data = area3, 
                  mapping = aes(x = x, ymin = ymin, ymax = ymax),
                  fill = "white", 
                  colour = "grey22") +
      # Area 4
      geom_ribbon(data = area4, 
                  mapping = aes(x = x, ymin = ymin, ymax = ymax),
                  fill = "grey22", 
                  colour = "grey22") +
      # Norm curve outline
      geom_line(data.frame(x = x, y = dnorm(x, mean = mean, sd = sd)),
                mapping = aes(x = x, y = y), 
                colour = "grey22")+
      # Scale limits
      scale_x_continuous(limits = limits,
                         breaks = c(45, 55, 60, 65, 70, 
                                    75, 80, 85, 90, 95,
                                    100, 105, 110, 115, 120, 
                                    125, 130, 135, 140, 145, 
                                    155),
                         labels = s1)+
      xlab("Raw Score\nStandard Score\nPercentile"))
}


## GreyScale Standard Score Plot with Qual rectangles

standardScorePlot_Grey2<- function(lb,  # scale lower bound 
                                   ub,  # scale upper bound 
                                   lb1, # Lower bound for s region 1
                                   ub1, # Upper bound for s region 1
                                   lb2, # Lower bound for s region 2
                                   ub2, # Upper bound for s region 2
                                   lb3, # Lower bound for s region 3
                                   ub3, # Upper bound for s region 3
                                   lb4, # Lower value for mean line
                                   ub4, #  upper value for mean line
                                   mean = 100, 
                                   sd = 15, 
                                   limits = c(mean - 4 * sd, mean + 4 * sd)) {
  # generates sequence of data points between limits using inputs
  x <- seq(limits[1], # vector position call
           limits[2], # vector position call
           length.out = 100)
  # Scale limits
  # Area 0 Average region
  x0min  <- max(lb, limits[1])   # defines xmin
  x0max  <- min(ub, limits[2])   # defines xmax
  areax0 <- seq(x0min, x0max, length.out = 100)
  area0  <- data.frame(x = areax0, ymin = 0, 
                       ymax = dnorm(areax0, mean = mean, sd = sd))
  
  # Area 1 High/Low Average regions
  x1min  <- max(lb1, limits[1])
  x1max  <- min(ub1, limits[2])
  areax1 <- seq(x1min, x1max, length.out = 100)
  area1  <- data.frame(x = areax1, ymin = 0, 
                       ymax = dnorm(areax1, mean = mean, sd = sd))
  
  # Area 2 Borderline/ Superior regions
  x2min  <- max(lb2, limits[1])
  x2max  <- min(ub2, limits[2])
  areax2 <- seq(x2min, x2max, length.out = 100)
  area2  <- data.frame(x = areax2, ymin = 0, 
                       ymax = dnorm(areax2, mean = mean, sd = sd))
  
  # Area 3 H
  x3min  <- max(lb3, limits[1])
  x3max  <- min(ub3, limits[2])
  areax3 <- seq(x3min, x3max, length.out = 100)
  area3  <- data.frame(x = areax3, ymin = 0, 
                       ymax = dnorm(areax3, mean = mean, sd = sd))
  
  # Area 4 Mean line
  x4min  <- max(lb4, limits[1])
  x4max  <- min(ub4, limits[2])
  areax4 <- seq(x4min, x4max, length.out = 100)
  area4  <- data.frame(x = areax4, ymin = 0, 
                       ymax = dnorm(areax4, mean = mean, sd = sd))
  
  ### Labels for X axis
  # 20 break points 
  # First and last points are left empty: No Score in this region
  
  # Percentile labels
  p <- c(" ", 
         "0.1%", "0.4%", "1%", "2.3%", "5%",
         "9%", "16%", "25%", "37%", "50%", 
         "63%","75%","84%", "91%", "95%", 
         "97.7%", "99%", "99.6%", "99.9%",
         " ")
  
  # Standard Scores
  s <- c(" ", 
         "55", "60", "65", "70", "75", 
         "80", "85", "90", "95", "100", 
         "105", "110", "115", "120", "125", 
         "130", "135", "140", "145", 
         " ")
  
  # Raw Scores
  r  <- c(" ", 
          "1-22", "23-25", "26-27", "28", "29", 
          "30-31", "32-33", "34-35", "36-37", "38",
          "39", "40-42", "43-45", "46-47", "48",
          "49", "50-51", "52-54", "55-60",
          " ")
  
  # Paste the Raw Score, Standard Score and Percentile together
  s1 <- paste(r, s, p, sep = "\n")
  
  # Qual Label Data
  SPM_Q  <- data.frame( x1 = c(55, 70, 80, 90, 110, 120, 130),
                        x2 = c(70, 80, 90, 110, 120, 130, 145),
                        y1 = c(-0.006, -0.006, -0.006, -0.006, -0.006, -0.006, -0.006),
                        y2 = c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00),
                        Qual = c("Extremely Low", "Borderline", "Low Average", 
                                 "Average",
                                 "High Average", "Superior", "Very Superior"),
                        type = c("a", "a", "a",
                                 "b",
                                 "a", "a", "a"))
  
  SPM_Q$Qual  <- factor(SPM_Q$Qual, 
                        levels = c("Extremely Low", "Borderline", "Low Average",
                                   "Average",
                                   "High Average", "Superior", "Very Superior"))
  
  SPM_Q$type  <- factor(SPM_Q$type,
                        levels = c("a", "b"))
  
  # Call the plot function
  (ggplot() + 
      geom_line(data.frame(x = x, y = dnorm(x, mean = mean, sd = sd)),
                mapping = aes(x = x, y = y), 
                colour = "grey22") + 
      # Area 0
      geom_ribbon(data = area0, 
                  mapping = aes(x = x, ymin = ymin, ymax = ymax),
                  fill = "grey28", 
                  colour = "grey22")+
      # Area 1
      geom_ribbon(data = area1, 
                  mapping = aes(x = x, ymin = ymin, ymax = ymax),
                  fill = "grey42", 
                  colour = "grey22")+
      # Area 2
      geom_ribbon(data = area2, 
                  mapping = aes(x = x, ymin = ymin, ymax = ymax),
                  fill = "grey66", 
                  colour = "grey22")+
      # Area 3
      geom_ribbon(data = area3, 
                  mapping = aes(x = x, ymin = ymin, ymax = ymax),
                  fill = "white", 
                  colour = "grey22") +
      # Area 4
      geom_ribbon(data = area4, 
                  mapping = aes(x = x, ymin = ymin, ymax = ymax),
                  fill = "grey22", 
                  colour = "grey22")+
      # Norm curve outline
      geom_line(data.frame(x = x, y = dnorm(x, mean = mean, sd = sd)),
                mapping = aes(x = x, y = y), 
                colour = "grey22")+
      # Qualitative layer
      geom_rect(data = SPM_Q, 
                mapping = aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=Qual),
                colour = "grey22")+
      scale_fill_manual(values =  c("grey28", "grey42", "grey66", 
                                    "white",
                                    "grey66", "grey42", "grey28"))+
      geom_text(data = SPM_Q, 
                aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, 
                    label=Qual, colour = type), 
                size = 2) +
      scale_colour_manual(values = c("white", "black"))+
      theme(legend.position="none")+
      # Scale limits
      scale_x_continuous(limits = limits,
                         breaks = c(45, 55, 60, 65, 70, 
                                    75, 80, 85, 90, 95,
                                    100, 105, 110, 115, 120, 
                                    125, 130, 135, 140, 145, 
                                    155),
                         labels = s1)+
      xlab("Raw Score\nStandard Score\nPercentile"))
}

