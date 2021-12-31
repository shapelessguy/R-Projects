dataset <- read.csv("games.csv")   #Import dataset

  ##Definitions of discreet and categorical variables of interest
  #discrete_variables <- c('turns', 'white_rating', 'black_rating', 'opening_ply')
  #categorical_variables <- c('rated', 'victory_status', 'winner')


dataset_BR <- preparation(dataset) #Dataset before Removal
# into this method there are few changes of dataset, useful
# for the continuation of the analysis

for (ch in discrete_variables) {
  #Shows frequency histograms and boxplots of all the discreet variables
  sub_set <- dataset[[ch]];
  plot <- ggplot(data = dataset[ch], mapping = aes(x=sub_set)) + 
    geom_histogram(color = "black", fill = "blue") + 
    xlab(ch) + ylab('abs frequency')+
    geom_vline(xintercept = mean(sub_set), linetype = "solid", color = "red", size = 0.3)+
    geom_vline(xintercept = median(sub_set), linetype = "dashed", color = "red", size = 0.3);
  print(plot)
  boxplot <- ggplot(data = dataset[ch], mapping = aes(y=sub_set)) + geom_boxplot() + ylab(ch)
  print(boxplot)
  print(summary(dataset[ch]))
} #prints the discrete variables histograms
for (ch in categorical_variables) {
  # Shows frequency histograms and boxplots of all the categorical variables
  sub_set <- dataset[[ch]];
  plot <- barplot(table(sub_set), col = 'orange', ylab = 'abs frequency', xlab = ch, horiz = FALSE)
  print(plot)
  print(table(sub_set))
} #prints the discrete variables histograms

printHisto_byBins(mydataset, 'white_rating', 50) #   |
printHisto_byBins(mydataset, 'white_rating', 100)#   |  progressively incresing
printHisto_byBins(mydataset, 'white_rating', 200)#   |  the number of bins
printHisto_byBins(mydataset, 'white_rating', 300)#   v

dataset_newUsersR <- filter_newUsers(dataset_BR, max_difference = 60,
                           excluded_values = c(1500))

#from now on will work with this datset
mydataset <- filter_shortgames(dataset_newUsersR, min_turns = 4) 

ggqqplot(mydataset, x= "white_rating")  #Distribution is normally distribuited?
ggqqplot(mydataset, x= "black_rating")
shapiroTest_3groups(mydataset)

print_whitevsblackHistogram(mydataset)

print(binomialTest(mydataset, null_hyp_prob = 0.516, alt_hyp_prob = 0.527, conf_level = 0.95))
print(binomialTest(mydataset, null_hyp_prob = 0.513, alt_hyp_prob = 0.527, conf_level = 0.99))
print(binomialTest(mydataset, null_hyp_prob = 0.51, alt_hyp_prob = 0.527, conf_level = 0.999))

num_dataset <- Numerical_Dataset(mydataset)
rcorr(as.matrix(num_dataset))
corrplot(cor(num_dataset), type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

print_coloredScatterplot(mydataset, color = '', x = 'white_rating', y = 'black_rating', 
                         xlab = 'White rating', ylab = 'Black rating');

print(make_classes(mydataset, 'winner_int', 'rating_difference_div100'));

print_coloredHistogram(mydataset, color = 'winner', x = 'rating_difference_div100', 
                       xlab = 'Classes: rating difference', palette = c("gray", "orange", "green"));

print_coloredScatterplot(mydataset, color = 'group', x = 'rating_difference', y = 'game_result', 
                         xlab = 'rating difference', ylab = 'game result');



group_by(mydataset, winner) %>% summarise( count = n(),mean = mean(rating_difference), 
                                           sd = sd(rating_difference))


print(binomialTest(vect = c(5051, 1701), null_hyp_prob = 0.7392, alt_hyp_prob = 0.753, conf_level = 0.95))
print(binomialTest(vect = c(5051, 1701), null_hyp_prob = 0.7356, alt_hyp_prob = 0.753, conf_level = 0.99))
print(binomialTest(vect = c(5051, 1701), null_hyp_prob = 0.7314, alt_hyp_prob = 0.753, conf_level = 0.999))






p1 <- 0.516
p2 <- 0.513
p3 <- 0.510
pa <- 0.527
i = 0
n = 13082
plot (seq(i,n,1), dbinom(seq(i,n,1), n, p1), xlim=c(6400, 7000), pch = '.')
lines(seq(i,n,.1), dnorm(seq(i,n,.1), mean= n*p1, sd = sqrt(n*p1*(1-p1))), col = "red")
lines(seq(i,n,.1), dnorm(seq(i,n,.1), mean= n*p2, sd = sqrt(n*p2*(1-p2))), col = "red")
lines(seq(i,n,.1), dnorm(seq(i,n,.1), mean= n*p3, sd = sqrt(n*p3*(1-p3))), col = "red")
lines(seq(i,n,.1), dnorm(seq(i,n,.1), mean= n*pa, sd = sqrt(n*pa*(1-p3))), col = "blue")

za2 = qnorm(0.05, lower.tail = FALSE)
mi1 = n*p1
mi2 = n*p2
mi3 = n*p3
za2a = qnorm(0.05, lower.tail = TRUE)
mia = n*pa
sda = sqrt(n*pa*(1-pa))
xa <- mia - sda * c(-za2a, seq(-za2a, 3.5, length=150))
xa
mia
ya <- dnorm(xa, mia, sda)
ya[1] <- 0
#polygon(c(xa), c(ya), col="blue")

sd1 = sqrt(n*p1*(1-p1))
sd2 = sqrt(n*p2*(1-p2))
sd3 = sqrt(n*p3*(1-p3))
x1 <- mi1 + sd1 * c(za2, seq(za2, 3.5, length=150))
y1 <- dnorm(x, mi1, sd)
y1[1] <- 0
polygon(c(x1,max(x1)), c(0,y1), col="red")


x1 <- mi1 + sd * c(za2, seq(za2, 3.5, length=150))
y1 <- dnorm(x, mi1, sd)
y1[1] <- 0
polygon(c(x1,max(x1)), c(0,y1), col="red")
#polygon(c(mi1, mi1+1), c(0,dbinom(as.integer(mi1), n, p1)), col="red")
#polygon(c(mi2, mi2+1), c(0,dbinom(as.integer(mi2), n, p2)), col="red")
#polygon(c(mi3, mi3+1), c(0,dbinom(as.integer(mi3), n, p3)), col="red")
#polygon(c(mi+sd*za2, mi+sd*za2+1), c(0,0.006), col="red")

print(binomialTest(vect = c(234, 212), null_hyp_prob = 0.5, conf_level = 0.9))


binom.test(c(234,212), p=0.5, alternative = c("less"))
