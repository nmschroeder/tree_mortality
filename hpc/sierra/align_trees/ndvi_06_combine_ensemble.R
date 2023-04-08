library(dplyr)
library(ggplot2)
library(cowplot)
library(stringr)
theme_set(theme_bw(base_size = 12))

data_dir <- "/path/to/your/data/directory/"
home_dir <- paste0(data_dir, "sierra")

setwd(home_dir)

# Make a function to read.csv and append the ensemble_id and m value to
# the data.frame (a way to avoid running ndvi_threshold_labels_idx.sub
# again to fix this the easier way)

read_ensemble <- function(fname_csv){
  df_csv <- read.csv(fname_csv)
  ensemble_id <- str_extract(fname_csv, "train_[0-9]+|test_[0-9]+")
  ensemble_id <- gsub('train_|test_', '', ensemble_id)
  ensemble_id <- ensemble_id %>% as.numeric()
  df_csv <- df_csv %>% mutate(ensemble_id = ensemble_id)
  return(df_csv)
}

fnames <- list.files(path = home_dir, pattern = glob2rx("optim_params_train_*.csv"), full.names = TRUE)

optim_params <- lapply(fnames, read_ensemble)
optim_params <- do.call(rbind.data.frame, optim_params)
optim_params <- arrange(optim_params, ensemble_id)
str(optim_params)
tail(optim_params)
write.csv(optim_params, "optim_params_ensemble_train.csv")

fnames2 <- list.files(path = home_dir, pattern = glob2rx("optim_params_test_*.csv"), full.names = TRUE)
optim_params2 <- lapply(fnames2, read_ensemble)
optim_params2 <- do.call(rbind.data.frame, optim_params2)
optim_params2 <- arrange(optim_params2, ensemble_id)
write.csv(optim_params2, "optim_params_ensemble_test.csv")

# Group by m (total)

optim_params_groups <- optim_params %>% group_by(total) %>% summarize(min_acc = min(acc), max_acc = max(acc), mean_acc = mean(acc))
optim_params_groups2 <- optim_params2 %>% group_by(total) %>% summarize(min_acc = min(acc), max_acc = max(acc), mean_acc = mean(acc))

p1 <- ggplot(data = optim_params_groups) + 
  geom_ribbon(mapping = aes(ymin = min_acc, ymax = max_acc, x= total), fill = 'lightblue') + 
  geom_line(mapping = aes(x=total, y=mean_acc)) + 
  xlab("Number of labeled training data") + ylab("Training Set Accuracy") + ylim(c(0.70, 1.01)) 
p1
ggsave("accuracy_training_labels_v2.png", dpi = 600)

p2 <- ggplot(data = optim_params_groups2) + 
  geom_ribbon(mapping = aes(ymin = min_acc, ymax = max_acc, x= total), fill = 'lightblue') + 
  geom_line(mapping = aes(x=total, y=mean_acc)) + 
  xlab("Number of labeled training data") + ylab("Testing Set Accuracy") + ylim(c(0.70, 1.01)) 
p2
ggsave("accuracy_testing_labels_v2.png", dpi = 600)

p1 <- ggplot(data = optim_params_groups) + 
  geom_ribbon(mapping = aes(ymin = min_acc, ymax = max_acc, x= total), fill = 'steelblue', alpha=0.5) + 
  geom_line(mapping = aes(x = total, y = min_acc), size = 0.1, color = 'steelblue') + 
  geom_line(mapping = aes(x = total, y = max_acc), size = 0.1, color = 'steelblue') + 
  geom_line(mapping = aes(x=total, y=mean_acc)) + 
  xlab("Number of labeled training data") + ylab("Training Set Accuracy") + ylim(c(0.70, 1.01)) 
p1
ggsave("accuracy_training_labels_v3.png", dpi = 600)

p2 <- ggplot(data = optim_params_groups2) + 
  geom_ribbon(mapping = aes(ymin = min_acc, ymax = max_acc, x= total), fill = 'steelblue', alpha=0.5) + 
  geom_line(mapping = aes(x = total, y = min_acc), size = 0.1, color = 'steelblue') + 
  geom_line(mapping = aes(x = total, y = max_acc), size = 0.1, color = 'steelblue') + 
  geom_line(mapping = aes(x=total, y=mean_acc)) +
  xlab("Number of labeled training data") + ylab("Testing Set Accuracy") + ylim(c(0.70, 1.01)) 
p2
ggsave("accuracy_testing_labels_v3.png", dpi = 600)

plot_grid(p1, p2, labels = c('A', 'B'), label_size = 12)
ggsave("accuracy_panel_v3.png", width = 6, height = 3, units = "in", dpi = 600)

ggplot(data = optim_params) + geom_point(mapping = aes(x=total, y=rdead)) + 
  xlab("Number of labeled training data") + ylab("Ratio of labeled dead trees (2017)") 
ggsave("rdead_training_labels.png", dpi = 600)

ggplot(data = optim_params) + geom_point(mapping = aes(x=total, y=ndvi), color = 'darkgreen') + geom_point(mapping = aes(x=total, y=prop), color = 'darkblue') +
  xlab("Number of labeled training data") + ylab("Parameter values") 
ggsave("parameter_values_labels.png", dpi = 600)
