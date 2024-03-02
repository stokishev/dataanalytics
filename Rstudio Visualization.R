###### STATUS = ACCEPTED
library(ggplot2)
library(scales)


data2$created_at = as.POSIXct(data2$created_at, format="%Y-%m-%d %H:%M:%S")
data2$created_at_date = as.Date(data2$created_at, format = "%Y-%m-%d %H:%M:%S")
data2$created_at_time = format(as.POSIXct(strptime(data2$created_at, "%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M")
data2$created_at_time = as.POSIXct(data2$created_at_time, format = "%H:%M")
data2$created_at_time = as.POSIXct(data2$created_at_time, format = "%Y-%m-%d %H:%M:%S")

ggplot(data2, aes(created_at_date)) + geom_bar() + labs(x="Created At Time (Month)", y="Number of Orders") + scale_x_date(breaks=date_breaks("1 month"))
ggplot(data2, aes(created_at_time)) + geom_bar() + labs(x="Created At Time (Time)", y="Number of Orders") +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M")

data2$picked_up_at = as.POSIXct(data2$picked_up_at, format="%Y-%m-%d %H:%M:%S")
data2$picked_up_at_date = as.Date(data2$picked_up_at, format = "%Y-%m-%d %H:%M:%S")
data2$picked_up_at_time = format(as.POSIXct(strptime(data2$picked_up_at, "%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M")
data2$picked_up_at_time = as.POSIXct(data2$picked_up_at_time, format = "%H:%M")

ggplot(data2, aes(picked_up_at_date)) + geom_bar() + labs(x="Picked Up At Time (Month)", y="Number of Orders") + 
  scale_x_date(breaks=date_breaks("1 month"))
ggplot(data2, aes(picked_up_at_time)) + geom_bar() + labs(x="Picked Up At Time (Time)", y="Number of Orders") +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M")

data2$desired_dropoff_time = as.POSIXct(data2$desired_dropoff_time, format="%Y-%m-%d %H:%M:%S")
data2$desired_dropoff_time_date = as.Date(data2$desired_dropoff_time, format = "%Y-%m-%d %H:%M:%S")
data2$desired_dropoff_time_time = format(as.POSIXct(strptime(data2$desired_dropoff_time, "%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M")
data2$desired_dropoff_time_time = as.POSIXct(data2$picked_up_at_time, format = "%H:%M")

ggplot(data2, aes(desired_dropoff_time_date)) + geom_bar() + labs(x="Desired Dropoff At Time (Month)", y="Number of Orders") + 
  scale_x_date(breaks=date_breaks("1 month"))
ggplot(data2, aes(desired_dropoff_time_time)) + geom_bar() + labs(x="Desired Dropoff At Time (Time)", y="Number of Orders") +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M")

data2$last_assigned_at = as.POSIXct(data2$last_assigned_at, format="%Y-%m-%d %H:%M:%S")
data2$last_assigned_at_date = as.Date(data2$last_assigned_at, format = "%Y-%m-%d %H:%M:%S")
data2$last_assigned_at_time = format(as.POSIXct(strptime(data2$last_assigned_at, "%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M")
data2$last_assigned_at_time = as.POSIXct(data2$last_assigned_at_time, format = "%H:%M")
data2$last_assigned_at_time = as.POSIXct(data2$last_assigned_at_time, format = "%Y-%m-%d %H:%M:%S")

ggplot(data2, aes(last_assigned_at_date)) + geom_bar() + labs(x="Last Assigned At Time (Month)", y="Number of Orders") + 
  scale_x_date(breaks=date_breaks("1 month"))
ggplot(data2, aes(last_assigned_at_time)) + geom_bar() + labs(x="Last Assigned At Time (Time)", y="Number of Orders") +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M")


# Created at VS Picked up at
df_new <- as.data.frame(data2)
df_new$diff_time <- difftime(data2$created_at_time, data2$picked_up_at_time, units = "mins")
df_new$time_diff <- as.numeric(df_new$diff_time, units = "mins")

intervals <- c(-Inf, 30, 60, 120, 180, Inf)
labels_1 <- c("Under 30 mins", "30 mins to 1 hour", "1 to 2 hours", "2 to 3 hours", "3+ hours")
df_new$name_diff = cut(abs(df_new$time_diff), intervals, labels = labels_1)

ggplot(df_new, aes(name_diff))+geom_bar()+labs(x = "Number of Orders", y = "Intervals", title = "Created at - Picked up at")
# THERE IS TENDECY OF ORDERS' MAJORITY TO BE UNDER 30 MINS GAP BETWEEN CREATED AT AND PICKED UP TIME
# HOWEVER, IT IS NEEDED TO LOOK UP AFTER 30 mins to 2 hours SECTORS AS THEY ARE TOGETHER MIGHT CONTRIBUTE TO 50% OF ALL ORDERS



# Created at VS Last assigned at
df_new$diff_time_2 <- difftime(data2$created_at_time, data2$last_assigned_at_time, units = "mins")
df_new$time_diff_2 <- as.numeric(df_new$diff_time_2, units = "mins")
df_new$name_diff_2 = cut(abs(df_new$time_diff_2), intervals_2, labels = labels_2)

intervals_2 <- c(-Inf, 1, 2, 3, 4, 5, 10, 20, 30, 60, 120, 180, Inf)
labels_2 <- c("Under 1 min", "1 to 2 mins", "2 to 3 mins", "3 to 4 mins", "4 to 5 mins", "5 mins to 10", "10 mins to 20", "20 mins to 30", "30 mins to 1 hour", "1 to 2 hours", "2 to 3 hours", "3+ hours")

ggplot(df_new, aes(name_diff_2))+geom_bar()+labs(x = "Number of Orders", y = "Intervals", title = "Created at - Last assigned at")


###### STATUS = EVERYTHING

data$created_at = as.POSIXct(data$created_at, format="%Y-%m-%d %H:%M:%S")
data$created_at_date = as.Date(data$created_at, format = "%Y-%m-%d %H:%M:%S")
data$created_at_time = format(as.POSIXct(strptime(data$created_at, "%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M")
data$created_at_time = as.POSIXct(data$created_at_time, format = "%H:%M")
data$created_at_time = as.POSIXct(data$created_at_time, format = "%Y-%m-%d %H:%M:%S")

ggplot(data, aes(created_at_date)) + geom_bar() + labs(x="Created At Time (Month)", y="Number of Orders") + scale_x_date(breaks=date_breaks("1 month"))
ggplot(data, aes(created_at_time)) + geom_bar() + labs(x="Created At Time (Time)", y="Number of Orders") +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M")


data$picked_up_at = as.POSIXct(data$picked_up_at, format="%Y-%m-%d %H:%M:%S")
data$picked_up_at_date = as.Date(data$picked_up_at, format = "%Y-%m-%d %H:%M:%S")
data$picked_up_at_time = format(as.POSIXct(strptime(data$picked_up_at, "%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M")
data$picked_up_at_time = as.POSIXct(data$picked_up_at_time, format = "%H:%M")

ggplot(data, aes(picked_up_at_date)) + geom_bar() + labs(x="Picked Up At Time (Month)", y="Number of Orders") + 
  scale_x_date(breaks=date_breaks("1 month"))
ggplot(data, aes(picked_up_at_time)) + geom_bar() + labs(x="Picked Up At Time (Time)", y="Number of Orders") +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M")


data$last_assigned_at = as.POSIXct(data$last_assigned_at, format="%Y-%m-%d %H:%M:%S")
data$last_assigned_at_date = as.Date(data$last_assigned_at, format = "%Y-%m-%d %H:%M:%S")
data$last_assigned_at_time = format(as.POSIXct(strptime(data$last_assigned_at, "%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M")
data$last_assigned_at_time = as.POSIXct(data$last_assigned_at_time, format = "%H:%M")
data$last_assigned_at_time = as.POSIXct(data$last_assigned_at_time, format = "%Y-%m-%d %H:%M:%S")

ggplot(data, aes(last_assigned_at_date)) + geom_bar() + labs(x="Last Assigned At Time (Month)", y="Number of Orders") + 
  scale_x_date(breaks=date_breaks("1 month"))
ggplot(data, aes(last_assigned_at_time)) + geom_bar() + labs(x="Last Assigned At Time (Time)", y="Number of Orders") +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M")


data$first_assigned_at = as.POSIXct(data$first_assigned_at, format="%Y-%m-%d %H:%M:%S")
data$first_assigned_at_date = as.Date(data$first_assigned_at, format = "%Y-%m-%d %H:%M:%S")
data$first_assigned_at_time = format(as.POSIXct(strptime(data$first_assigned_at, "%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M")
data$first_assigned_at_time = as.POSIXct(data$first_assigned_at_time, format = "%H:%M")
data$first_assigned_at_time = as.POSIXct(data$first_assigned_at_time, format = "%Y-%m-%d %H:%M:%S")

ggplot(data, aes(first_assigned_at_date)) + geom_bar() + labs(x="First Assigned At Time (Month)", y="Number of Orders") + 
  scale_x_date(breaks=date_breaks("1 month"))
ggplot(data, aes(first_assigned_at_time)) + geom_bar() + labs(x="First Assigned At Time (Time)", y="Number of Orders") +
  scale_x_datetime(breaks = scales::date_breaks("1 hour"), date_labels = "%H:%M")

# Created at VS Picked up at
df_new_2 <- as.data.frame(data)

df_new_2$diff_time <- difftime(data$created_at_time, data$picked_up_at_time, units = "mins")

df_new_2$time_diff <- as.numeric(df_new_2$diff_time, units = "mins")

df_new_2$name_diff = cut(abs(df_new_2$time_diff), intervals, labels = labels_1)

ggplot(df_new_2, aes(name_diff))+geom_bar()+labs(x = "Number of Orders", y = "Intervals", title = "Created at - Picked up at")


# Created at VS First assigned at
df_new_2$diff_time_2 <- difftime(data$created_at_time, data$first_assigned_at_time, units = "mins")
df_new_2$time_diff_2 <- as.numeric(df_new_2$diff_time_2, units = "mins")
df_new_2$name_diff_2 = cut(abs(df_new_2$time_diff_2), intervals_2, labels = labels_2)

ggplot(df_new_2, aes(name_diff_2))+geom_bar()+labs(x = "Number of Orders", y = "Intervals", title = "Created at - First assigned at")


