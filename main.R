getwd() # check working directory

w.dir <- getwd() # link working directory to a manipulable object

library(ggplot2)

output.folder.names <- c( # handy dandy replicable file names
  "raw data",
  "clean data",
  "cleaners",
  "analyzers",
  "figures"
)

# double check length and strucure of folder names vector
length(output.folder.names)
str(output.folder.names)

# for-loop to build folders saved in output.folder.names, will run until all folders are built
for(i in 1:length(output.folder.names)) {
  if(file.exists(output.folder.names[i]) == FALSE)
    dir.create(output.folder.names[i])
}

# check that the new files are where they should be
list.files()

# build file paths for new files
# paste pulls all elements in () together; 
#     working directory + "/" + assigned folder name[number of folder in sequence] (DO NOT FORGET THE SLASHES)
p.rawdata <- paste(w.dir, (output.folder.names[1]), sep = "/")
p.cleandata <- paste(w.dir, (output.folder.names[2]), sep ="/")
p.cleaners <- paste(w.dir, (output.folder.names[3]), sep ="/")
p.analyzers <- paste(w.dir, (output.folder.names[4]), sep ="/")
p.figures <- paste(w.dir, (output.folder.names[5]), sep ="/")

# Build a test graph to check linear regression
test.data.linear <- function(a, b, sd.noise, x){
  
  y <- a * x + b # your basic bare-bones "y = mx + b" slope-intercept formula
  
  noise <- rnorm(length(x), 0, sd.noise)
  y.n <- y + noise
}

# simulate the datapoints: 50 x values and Y values to match using the function
x.sim <- c(1:50)
y.sim <- test.data.linear(1, 1, 10, x.sim)

# turn x and y from linear function into dataframe
d.1 <- data.frame("Projected Cost ($millions)" = x.sim, RoI = y.sim)

# visualize the data
plot(d.1)

# Build logarithmic function
test.data.log <- function(b, sd.noise, x){
  
  y <- log(x, base = exp(1)) + b # base log graph, the anti-parabola
  
  noise <- rnorm(length(x), 0, sd.noise)
  y.n <- y + noise
}

y.sim.2 <- test.data.log(1, 1, x.sim) # save the values to a specific graph

d.2 <- data.frame("Projected Cost ($millions)" = x.sim, RoI = y.sim.2)

plot(d.2) # SUCCESS

# plot explonential function
test.data.exp <- function(b, sd.noise, x){
  
  y <- x ^ 2 + b # base formula for a parbolic graph
  
  noise <- rnorm(length(x), 0, sd.noise)
  y.n <- y + noise
}

y.sim.3 <- test.data.exp(1, 150, x.sim)

d.3 <- data.frame("Projected Cost ($millions)" = x.sim, RoI = y.sim.3)

plot(d.3) # Success, come to find at least 100 noise is needed to be visualized

# come to the realization that plots d.2. and d.3 give back y values faaaaaaaar outside of the expected range (log is too low, exp is too high)

# load in the real data
t.1 <- read.csv("C:/Users/Christopher Walton/Documents/Quest/IND 3157 (Data Analysis Using R)/code book/economic.impact.megaprojects/raw data/subsidies.teams.csv")
str(t.1)

class(t.1)

# examine the columns to make sure they loaded in properly
str(t.1$Cost)
str(t.1$Subsidy)
str(t.1$Net.Revenue)

# remove rows with missing values from dataset
t.2 <- t.1[complete.cases(t.1), ]
str(t.2) # check the structure, make sure nothing fell out in transit

# visualize initial dataset
plot(x = t.2$Subsidy, y = t.2$Net.Revenue, xlab = "Subsidy", ylab = "Net Return")

t.lm <- lm(t.2$Net.Revenue~t.2$Subsidy, data = t.2) # assign new 
plot(t.lm)

summary(t.lm) # get a rundown of what I just made
coef(t.lm) # check the default correlation coefficients
anova(t.lm) # and run an ANOVA test because why not, we can

ggplot(t.2, aes(y = t.2$Net.Revenue, x = t.2$Subsidy)) + # get on that GGplot train boiiii
  geom_point(size = 2, col = "red") + # color the datapoints
  geom_smooth(method = "lm", se = TRUE) + 
  theme(aspect.ratio = 0.80)

confint(t.lm, level = 0.95) # exmine the lm plot's confidence intervals

anova(t.lm)

??pearson
cor.test(x = t.2$Subsidy, y = t.2$Net.Revenue,
         alternative = c("two.sided"),
         method = c("pearson"),
         exact = NULL, conf.level = 0.95, continuity = FALSE)
