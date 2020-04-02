# Import necessary libraries
library(tidyverse)
library(ggthemes)

# Get MTA Customer Feedback Data
# All data are from the MTA Right Now app (https://mta-nyc.custhelp.com/app/ask)
mta_cust <- read.csv('mta-customer-feedback-data-beginning-2014.csv')
mta_cust <- as_tibble(mta_cust)
head(mta_cust)

# Check structure of mta
str(mta_cust)

# Check for NA values
is.na(mta_cust)

# Define theme for vizualizations
thm <- theme(plot.title = element_text(size = 16, face = 'bold'),
             plot.subtitle = element_text(size = 8),
             legend.title = element_text(size = 12, face = 'bold'),
             axis.title.x = element_text(size = 12, face = 'bold'),
             axis.title.y = element_text(size = 12, face = 'bold'))

# Complaints due to lateness/delay by train
mta_cust %>%
  filter(Subject.Matter == 'Travel Disruption / Trip Problem'
         & Agency == 'Subways' & Commendation.or.Complaint == 'Complaint'
         & Issue.Detail == 'Late / Delay Current') %>%
  ggplot(aes(Branch.Line.Route)) +
  labs(title = 'MTA Dispruption by Train Line', x = 'Train Line', y = 'Count', 
       subtitle = 'Data from MTA Right Now app (https://mta-nyc.custhelp.com/app/ask)') +
  geom_histogram(stat = 'count') +
  thm

# Filter further to include only 5 borough trains (exl LIRR/Metro North, etc., as well as 'No Value')
mta_complaint <- mta_cust %>%
  filter(Subject.Matter == 'Travel Disruption / Trip Problem'
         & Agency == 'Subways' 
         & Commendation.or.Complaint == 'Complaint'
         & Branch.Line.Route %in% (c('1','2','3','4','5','6','7','A','B','C',
                                     'D','E','F','G','J','L','M','N','Q','R','Z')))

# Plot all train Travel Disruption complaints by train
mta_complaint %>%
  ggplot(aes(Branch.Line.Route)) +
  geom_histogram(aes(fill = Issue.Detail), stat = 'count', position = 'dodge', alpha = 0.8) +
  labs(title = 'MTA Dispruption by Train Line', x = 'Train Line', subtitle = 'Data from MTA Right Now app (https://mta-nyc.custhelp.com/app/ask)',
       fill = 'Reported Issue') +
  theme_bw()

# Filter by most complaints received
mta_top_trains <- mta_complaint %>%
  filter(Branch.Line.Route %in% c('F','A','7','R','E','4','D'))

# Complaints by Issue.Detail
mta_top_trains %>%
  ggplot(aes(Branch.Line.Route)) +
  geom_histogram(aes(fill = Issue.Detail), stat = 'count', position = 'dodge', alpha = 0.6) +
  labs(title = 'MTA Dispruption by Train Line', x = 'Train Line', y = 'Complaints',
       subtitle = 'Data from MTA Right Now app (https://mta-nyc.custhelp.com/app/ask)',
       fill = 'Reported Issue') +
  thm

# Complaints by Year
mta_top_trains %>%
  ggplot(aes(Branch.Line.Route)) +
  geom_histogram(aes(fill = factor(Year)), stat = 'count', position = 'dodge', alpha = 0.6) +
  labs(title = 'MTA Dispruption by Train Line', x = 'Train Line', y = 'Complaints',
       subtitle = 'Data from MTA Right Now app (https://mta-nyc.custhelp.com/app/ask)',
       fill = 'Year') +
  thm

# The highest number of complaints were received in 2016 and 2017
# What were the highesty complaints for these 2 years?
# Separate 2016 - 2017 data
mta_16_17 <- mta_top_trains %>%
  filter(Year %in% c('2016','2017'))

# Top complaints during 2016 - 2017
mta_16_17 %>%
  ggplot(aes(Branch.Line.Route)) +
  geom_histogram(aes(fill = Issue.Detail), stat = 'count', position = 'dodge', alpha = 0.6) +
  labs(title = 'MTA Dispruption by Train Line', x = 'Train Line', y = 'Complaints',
       subtitle = 'Data from MTA Right Now app (https://mta-nyc.custhelp.com/app/ask)',
       fill = 'Reported Issue') +
  thm

# Filter for top 10 complaints
mta_16_17 <- mta_16_17 %>%
  add_count(Issue.Detail) %>%
  filter(n >= 154)
  
# Density plot of all complaints per train line
mta_16_17 %>%
  ggplot(aes(Branch.Line.Route)) +
  geom_density(aes(fill = Issue.Detail), alpha = 0.3) +
  facet_grid(rows = vars(Year)) +
  labs(title = 'MTA Dispruption by Train Line', x = 'Train Line', 
       subtitle = 'Data from MTA Right Now app (https://mta-nyc.custhelp.com/app/ask)',
       fill = 'Reported Issue') +
  thm

# Late / Delay is by far the most common complaint made on all lines. The density for this particular complaint is highest amongst all 
# lines, though the 4, 7, and A are much higher. However, it is clear that delays are the most common complaint.

# Facet grid divided by year to see real complaint distribution per line
mta_16_17 %>%
  ggplot(aes(Branch.Line.Route)) +
  geom_histogram(aes(fill = Issue.Detail), stat = 'count', alpha = 0.8, position = 'dodge') +
  scale_fill_brewer(palette = 'RdYlGn') +
  facet_grid(rows = vars(Year)) +
  labs(title = 'MTA Dispruption by Train Line', x = 'Train Line', y = 'Complaints',
       subtitle = 'Data from MTA Right Now app (https://mta-nyc.custhelp.com/app/ask)',
       fill = 'Reported Issue') +
  thm

# By quarter
mta_16_17 %>%
  filter(Issue.Detail == 'Late / Delay Past') %>%
  ggplot(aes(Branch.Line.Route)) +
  geom_histogram(aes(fill = Branch.Line.Route), stat = 'count') +
  scale_fill_brewer(palette = 'RdYlGn') +
  facet_grid(cols = vars(Year), rows = vars(Quarter)) +
  labs(title = 'MTA Dispruption by Train Line', x = 'Train Line', y = 'Complaints',
       subtitle = 'Data from MTA Right Now app (https://mta-nyc.custhelp.com/app/ask)',
       fill = 'Train Line') +
  thm

# This indicates that, though there are minor fluctuations throughout the year, in terms of complaints, The biggest spike in 
# complaints due to delays (past delays according to the email response form) occured in the 4th Quarter of 2017 for both the 
# A and F lines. This could be due to many factors. Fistly, both of these lines are heavily trafficked lines. Secondly, both
# are cross borough lines, meaning that people likely rely on these lines for both work and neighborhood transit. The F also 
# runs through Rockefeller Station, which has one of oldest and most unreliable switches in the whole MTA system. This is also likely
# due to upticks in people moving further out in the outer-boroughs (in this case Brooklyn and Queens) due to rising rental costs.
# This only accounts for general traffic though. Another reason that it might be more heavily trafficked during the fourth quarter is
# due to holiday traffic, both from resident New Yorkers and tourists.

# Import MTA Eye on the Future data to see if any 2018 projects are perhaps in response to train delays caused in 2016 and 2017
