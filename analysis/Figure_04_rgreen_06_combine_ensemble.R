# In this script, we explore the impact of increasing numbers of training data on the parameters used to 
# calculate the tree mortality fraction (crown fraction threshold and relative greenness threshold)

# The naming of the script comes the script's original location within the HPC-based scripts under
# hpc/sierra/align_trees

# We randomly arranged the training data into 30 unique sequences of indices to represent 30 different random
# ways of selecting from the training data

# For each of the 30 random sequences of indices, we selected training data 10 at a time in the order
# of the random sequence starting with the first 10 and adding by 10 through 5300 training samples. For each
# number of training samples, we computed the parameters of best fit and accuracy on both the training data
# and the validation data set. In this script, we plot the accuracy against the number of labeled training data 
# to see if the accuracy converges before reaching our total number of training data (>5300 trees). 

# We want to know if the accuracy and parameter values will converge for such a small number of 
# hand-labeled training data (order of thousands), since our data set is very large (more than one million trees).
# Since the accuracy and parameters converge within a few thousand training labels, we conclude that we likely 
# have enough training data to fit our parameters.

library(dplyr)
library(ggplot2)
library(cowplot)
library(stringr)
theme_set(theme_bw(base_size = 12))

# Read in the training data outcomes for each of the 30 ensembles
optim_params <- read.csv("data/intermediate/index_parameters/rgreen_optim_params_ensemble_train.csv") %>% arrange(total, ensemble_id)

# Read in the validation data outcomes for each of the 30 ensembles
optim_params2 <- read.csv("data/intermediate/index_parameters/rgreen_optim_params_ensemble_valid.csv") %>% arrange(total, ensemble_id)

# Group by the total number of training data by total number of trees. Note that each of the 30 ensembles 
# includes the outcome for 10 training labels)
optim_params_groups <- optim_params %>% group_by(total) %>% summarize(min_acc = min(acc), max_acc = max(acc), mean_acc = mean(acc))

# Group the total number of validation data by total number of trees
optim_params_groups2 <- optim_params2 %>% group_by(total) %>% summarize(min_acc = min(acc), max_acc = max(acc), mean_acc = mean(acc))

# Plot the training label accuracy range over the 30 ensembles
p1 <- ggplot(data = optim_params_groups) + 
  geom_ribbon(mapping = aes(ymin = min_acc, ymax = max_acc, x= total), fill = 'steelblue') + 
  geom_line(mapping = aes(x = total, y = min_acc), size = 0.1, color = 'steelblue') + 
  geom_line(mapping = aes(x = total, y = max_acc), size = 0.1, color = 'steelblue') + 
  geom_line(mapping = aes(x=total, y=mean_acc)) + 
  xlab("Number of labeled training data") + ylab("Training Set Accuracy") + ylim(c(0.70, 1.01)) 

# Plot the validation accuracy range over the 30 ensembles
p2 <- ggplot(data = optim_params_groups2) + 
  geom_ribbon(mapping = aes(ymin = min_acc, ymax = max_acc, x= total), fill = 'steelblue') + 
  geom_line(mapping = aes(x = total, y = min_acc), size = 0.1, color = 'steelblue') + 
  geom_line(mapping = aes(x = total, y = max_acc), size = 0.1, color = 'steelblue') + 
  geom_line(mapping = aes(x=total, y=mean_acc)) +
  xlab("Number of labeled training data") + ylab("Validation Set Accuracy") + ylim(c(0.70, 1.01)) 

# Plot the two figures in a panel for Figure 4 in the main text
plot_grid(p1, p2, labels = c('a', 'b'), label_size = 12, label_fontface = "plain")
ggsave("figures/Figure_4_accuracy_panel.eps", width = 6, height = 3, units = "in", dpi = 600)

