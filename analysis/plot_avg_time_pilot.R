# Basic Plot for showing average time needed in load one pilot
#data time_avg_pilot was used here

boxplot(Min ~ load, data = Data) 

ggplot(time_avg_pilot, aes(x = load, y = Min, color = Min)) + 
  
  geom_point() + 
  
  geom_smooth(method = "lm", fill = NA) 
