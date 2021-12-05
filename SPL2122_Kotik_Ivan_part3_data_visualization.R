################################################################################
################## Statistical Programming Languages 2021/22 ###################
##################               Take-home Exam              ###################
##################                                           ###################
##################   	  Sigbert Klinke, Eva-Maria Maier,   ###################
##################       		  Alexander Volkmann         ###################
################################################################################

#-------------------------------------------------------------------------------
# Surname: Kotik
# Name: Ivan
#-------------------------------------------------------------------------------

### Exercise 4 -----------------------------------------------------------------


#   a) Read the data file mortgage.csv into R.

df <- as.data.frame(read.csv("C://Users//qwerty//Downloads//mortgage.csv"))

#   b) Construct a graphic similar to the following graphic below, which includes everything inside the thick
#      black frame (except the frame itself).
#      Use for the scatterplot for the x-axis the variable interest and the y-axis school.
#      As categorical variable use the variable coborrower.
#   c) Save the graphic as pdf file in your working directory.


df$coborrower <- factor(df$coborrower)

pcolor <- c("#9ACD32", "#FF0000", "#8B008B", "#FF4FFF", "#FFA500", "#00FF00", "#35FFFF", "#0000FF") # recreating the colors from the example

df$Colour[df$coborrower=="yes"] <- "#00FF00" # giving the categories colours
df$Colour[df$coborrower=="no"] <- "#FF0000"
df$Shape[df$coborrower=="yes"] <- 2  # giving the categories shapes
df$Shape[df$coborrower=="no"] <- 6

pdf("SPL2122_Kotik_Ivan_4_plot.pdf") # saving the file

layout(matrix(c(1, 2, 1, 2, 1, 2, 3, 3, 3, 3), 5, 2, byrow = TRUE))  # creating the layout
#layout.show(n=3)

# Scatterplot
plot(x = df$interest,
     y = df$school,
     col = df$Colour,
     pch = df$Shape,
     cex = 1.75,
     ylab = "Years of schooling for the borrower",
     xlab = "Fixed interest rate",
     cex.axis = 1.5,
     cex.lab = 1.7)

# Boxplot
boxplot(df$school ~ df$coborrower,
        ylab = "Years of schooling for the borrower", xlab = "Is there a co-borrower",
        xaxt = "n",
        cex.axis = 1.5,
        cex.lab = 1.7,
        col = c(unique(df$Colour)))

# Legend
plot(0,type='n',axes=FALSE,ann=FALSE)
legend("center",
       legend = c("There is no co-borrower", "There is a co-borrower"),
       pch = c(unique(df$Shape)),
       cex = 1.5,
       col = c(unique(df$Colour)))
dev.off()
