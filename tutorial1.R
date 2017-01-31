library(readr)
library(ggplot2)
library(saccades)
library(jpeg)
library(grid)

X1_gaze <- read_delim("/uploadData/1_gaze.txt", "\t", escape_double = FALSE, trim_ws = TRUE, skip = 37)

msg <- X1_gaze[which(X1_gaze$Type == "MSG"),]
start_row <- msg[which(msg$'L Raw X [px]' == "# Message: Questionnaire15.jpg")-1,]
end_row <- msg[which(msg$'L Raw X [px]' == "# Message: Questionnaire15.jpg"),]
stimuliRowData <- X1_gaze[which(X1_gaze$Time > start_row$Time & X1_gaze$Time < end_row$Time),]

DFforFDetection <- data.frame(time=stimuliRowData$Time, x=stimuliRowData$`R POR X [px]`, y=stimuliRowData$`R POR Y [px]`, trial=stimuliRowData$Trial)
DFforFDetection$x <- as.numeric(as.character(DFforFDetection$x))
DFforFDetection$y <- as.numeric(as.character(DFforFDetection$y))
DFforFDetection$y <- 1080 - DFforFDetection$y
DFforFDetection <- DFforFDetection[which(DFforFDetection$x > 0),]

fixationsSC <- detect.fixations(DFforFDetection)
fixationsSC <- fixationsSC[which(fixationsSC$dur > 0),]

summary(fixationsSC$dur)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3994   72000  228000  223900  324000  867900 

ggplot(fixationsSC, aes(x = dur/1000)) + 
  geom_histogram(aes(y = ..count..), binwidth = 20) +
  scale_x_continuous(name = "Fixation duration, msec.") +
  scale_y_continuous(name = "Count")

# start_row$'L Raw X [px]'

img <- readJPEG("/uploadData/pics/8a3893d3fab072a0a78db1788cc3afdc_1920x1080.jpg")
g <- rasterGrob(img, interpolate=TRUE)
ggplot(fixationsSC, aes(x, y)) +
  xlim(0, 1920) + ylim(0, 1080) +
  annotation_custom(g, xmin=0, xmax=Inf, ymin=0, ymax=Inf) +
  geom_point(aes(colour = "red", alpha = 1/10, size = dur/1000))

