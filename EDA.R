
#' ---
#' title: "EDA on Instacart Dataset"
#' author: "Gaurang Swarge"
#' date: "May 6th, 2018"
#' ---
#' 

#```{r setup, include=FALSE}
library(knitr)
opts_chunk$set(fig.width=12, fig.height=8, fig.path='Figs/', echo=FALSE, warning=FALSE, message=FALSE)

options(width = 180)


#EDA on the data

library(plyr)
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(scales)

#Use fread always, read the tables much faster

orders <- fread("/Dataset/orders.csv")
products <- fread("/Dataset/products.csv")
departments <- fread("/Dataset/departments.csv")
aisles <- fread("/Dataset/aisles.csv")
opp <- fread("/Dataset/order_products__prior.csv")
opt <- fread("/Dataset/order_products__train.csv")


#converting to factor variables
orders$order_hour_of_day <- as.numeric(orders$order_hour_of_day)
orders$order_dow <- as.factor(orders$order_dow)
orders$eval_set <- as.factor(orders$eval_set)

products$product_name <- as.factor(products$product_name)

departments$department <- as.factor(departments$department)

aisles$aisle <-  as.factor(aisles$aisle)

#opp$reordered <- as.factor(opp$reordered)
#opt$reordered <- as.factor(opt$reordered)
#````

#glimpse(opt)
# %>% is read as 'and then' , n=n() means that a variable named n will be 
# assigned number of rows

#' ### 1. Starting with Orders Table
#' Order Hour of the day vs no. of Orders: Checking What is the time when maximum people order:<br/>
#' It seems like people order the most between 9:00 AM to 4:PM on Day 0 of the week presumably Saturday <br/>

#```{r orders-table}

p <- orders
kable(head(p))
  p %>% ggplot(aes(x=order_hour_of_day,fill=order_dow))+
  geom_histogram(stat = "count")+
  theme(axis.text.x=element_text(angle=60, hjust=1),panel.grid.major = element_line(colour = "grey"))+
    scale_x_continuous(breaks=seq(0, 24, 1))+
  labs(x="Order Hour of the Day",y="Count of Orders")
  
  #' ```
  #' <br/>
    
  #'```{r }
#' ### 2. Checking for  Order Day of Week: When do people order the most:
#' It seems like Day 0 & Day 1 presumable Saturday and Sunday is when people order the most<br/>

  p %>% ggplot(aes(x=order_dow,fill=order_dow))+
    geom_histogram(stat = "count")+
    theme(axis.text.x=element_text(angle=60, hjust=1),panel.grid.major = element_line(colour = "grey"))+
    labs(x="Order Day of the Week",y="Count of Orders")
  
  
#' ### 3. Checking how many days it takes to people for ordering again.
#' Days since prior Order vs Count of Order numbers.
#' So seems like people order again maximum on 7th Day and 30th day<br/>

  p %>% ggplot(aes(x=days_since_prior_order,fill=order_dow))+
    geom_histogram(stat = "count")+
    theme(axis.text.x=element_text(angle=60, hjust=1),panel.grid.major = element_line(colour = "grey"))+
    scale_x_continuous(breaks=seq(0, 30, 1))+
    labs(x="No. of Days since last Order",y="Count of Orders")
  
  #' #### Figure 3
  #' 
  #' <br/>
  
  #Days since last order vs count of order
  #p <- orders %>%
  #  inner_join(opt,by="order_id")%>%
  #  inner_join(products,by="product_id") %>%
  #  group_by(order_id,order_number,days_since_prior_order)%>%
  #  summarise(count=n())
  #p <- orders %>%
  #  group_by(order_id,days_since_prior_order)%>%
  #  summarise(count=n())
    
  #kable(head(p,10))
  #dim(p)
  
  #p %>% ggplot(aes(x=days_since_prior_order,y=count,fill="lightcoral")) +
  #  geom_bar(stat = "identity")+
  #  #geom_jitter(stat="identity")+
  #  theme(axis.text.x=element_text(angle=60, hjust=1),panel.grid.major = element_line(colour = "grey"))+
  #  scale_x_continuous(breaks=seq(0, 30, 1))+
  #  labs(title="Days Since Last Order Vs Count of Order numbers")
  
#' ### 4. prior orders of the customer vs Order Number
#' We can see that more than 50% of the orders tend to have ordered previously upto 10 times
#' <br/>
#' 
  p<-orders %>% 
    filter(eval_set=="prior") %>%
    count(order_number)
    #summarise(count=n())
  kable(head(p,10))
  #dim(p)
  
  p %>% ggplot(aes(x=order_number,y=n)) + 
    geom_line(size=1,color="lightgreen")+
    geom_point(size=1, color="purple")+
    theme(axis.text.x=element_text(angle=60, hjust=1),panel.grid.major = element_line(colour = "lightgrey"))+
    scale_x_continuous(breaks=seq(0, 100, 5))+
    scale_y_continuous(breaks=seq(0, 225000, 25000))+
    labs(title="Prior Orders of the customer Vs Count", x="Prior no. of Orders", y="Count of orders")
  
  #' #### Figure 4
  
  #' ### 5. For all the customesrs who has prior order history 
  #' This graph shows customer vs No of orders<br/>
  #' We can see the average no of orders changing as the no of users increase<br/>

  p<-orders %>% 
    filter(eval_set=="prior") %>%
    count(user_id)
  #summarise(count=n())
  kable(head(p,10))
  #dim(p)
  
  p %>% ggplot(aes(x=user_id,y=n)) + 
    #geom_line(size=1,color="lightgreen")+
    #geom_point(size=0.5, color="purple")+
    geom_smooth(span=0.3,size=1, color="lightcoral")+
    theme(axis.text.x=element_text(angle=60, hjust=1),panel.grid.major = element_line(colour = "lightgrey"))+
    scale_x_continuous(breaks=seq(0, 200000, 10000))+
    #scale_y_continuous(breaks=seq(0, 225000, 25000))+
    labs(title="Prior Orders of the customers Vs Users", x="Users", y="No of Orders")
  
  #' #### Figure 5

  #' ### 6. Order_hour_of the day vs mean(number of orders of customers) 
  #' Trying to check how many orders are placed at what time of the day, 
  
  p<-orders %>% 
    group_by(order_hour_of_day,order_number)%>%
    summarise(count = n()) %>%
    arrange(desc(count))
  
  kable(head(p,10))
  #dim(p)
  
  p %>% ggplot(aes(x=order_hour_of_day,y=order_number,fill=order_number))+
    geom_tile(aes(fill=count),colour = "white") + 
    scale_fill_gradient(low = "aquamarine",high = "red")+
    theme(axis.text.x=element_text(angle=60, hjust=1),panel.grid.major = element_line(colour = "lightgrey"))+
    scale_x_continuous(breaks=seq(0, 24, 1))+
    scale_y_continuous(breaks=seq(0, 100, 5))+
    labs(title="Order Hour of Day Vs Prior Order Numbers", x="Order Hour of Day", y="Count of order numbers")

#' #### Figure 6

#' ### 7. Trying to find out Highest reordered Product
#' Organic LowFat Milk as expected is the highest reordered product in the dataset
#' and the second highest is the Banana<br/>

temp <- opt  %>%
  group_by(product_id) %>%
  summarise(percent_reordered = mean(reordered),n=n()) %>%
  filter(n>100) %>%
  top_n(10,wt=percent_reordered) %>% 
  arrange(desc(percent_reordered)) %>% 
  left_join(products,by="product_id")

kable(temp)

  temp %>% ggplot(aes(x=reorder(product_name,-percent_reordered),y=percent_reordered, fill=product_name)) +
    geom_bar(stat="identity")+
    theme(axis.text.x=element_text(angle=60, hjust=1),axis.title.x = element_blank(),panel.grid.major = element_line(colour = "grey"))+
    coord_cartesian(ylim=c(0.75,0.95))+
    labs(title="Highest Re-Ordered Product")

  
#### testing
  
# temp2 <- opt  %>%
#    group_by(product_id)%>%
#    summarize(p_reordered = mean(reordered), n=n()) %>%
#    filter(n>40)
    #top_n(10, wt=p_reordered)
  
#  kable(head(temp2))  
  ####
  #' #### Figure 7

  #' ### 8. Trying to check, which is the Highest Re-Ordered Department
  #' As you can see products from Produce and Dairy Eggs are reordered more than 95% of the time<br/>
  #' 
  temp1 <- opt  %>%
    group_by(product_id) %>%
    summarize(percent_reordered = mean(reordered),n=n()) %>%
    filter(n>100) %>%
    top_n(30,wt=percent_reordered) %>% 
    arrange(desc(percent_reordered)) %>% 
    left_join(products,by="product_id")%>%
    left_join(departments,by="department_id")
    
  kable(temp1)
  
  temp1 %>% ggplot(aes(x=reorder(department,-percent_reordered),y=percent_reordered,fill=department)) +
    geom_bar(stat="identity")+
    theme(axis.text.x=element_text(angle=60, hjust=1),axis.title.x = element_blank(),panel.grid.major = element_line(colour = "grey"))+
    coord_cartesian(ylim=c(0.80,0.95))+
    labs(title="Top 30 Items Re-Ordered from following Departments", y="%ge of Reodered")
  
  #' #### Figure 8
  #' 
  #' ### 9. Finding out total %ge of products which are reordered
  #' As you can se abour 60% products are generally reordered<br/> 
  
  temp2 <- opt %>%
    group_by(reordered) %>%
    summarise(count = n()) %>%
    mutate(reordered = as.factor(reordered)) %>%
    mutate(percentage = count/sum(count))
  
  kable(head(temp2,10))
  
  temp2 %>% ggplot(aes(x=desc(reordered),y=percentage, fill=reordered))+
    geom_bar(stat="identity")+
    theme(axis.text.x=element_text(angle=60, hjust=1),panel.grid.major = element_line(colour = "grey"))+
    scale_x_continuous(breaks=seq(0, 1, 1))+
    scale_y_continuous(breaks=seq(0, 1.0, .15))+
    labs(title="%ge of products Reordered", x="Reordered or Not")
  
  #' #### Figure 9
  #' 
  #' ---------------------------
  #'### 10. Trying to find out Max number of product Variety in the Aisle
  #'As we can see Vitamis SUpplements and yogurt aisles has the maximum product variets<br/>
  #'
  temp3 <- products %>%
    group_by(aisle_id) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>% 
    left_join(aisles,by="aisle_id")%>%
    top_n(20)
  
  kable(temp3)
  
  temp3 %>% ggplot(aes(x=reorder(aisle,-count),y=count, fill=aisle))+
    geom_bar(stat="identity")+
    theme(axis.text.x=element_text(angle=60, hjust=1),axis.title.x = element_blank(),panel.grid.major = element_line(colour = "grey"))+
    labs(title="Max number of product variety in which Aisle? (Top 20)",y="Count of Products")
  
  
  #' #### Figure 10
  #'
  #' ### 11. Trying to find out Max number of product Variety in the Departments
  #'As we can see Personal Care and Snacks has the maximum product varieties<br/>
  #'
  temp3 <- products %>%
    group_by(department_id) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>% 
    left_join(departments,by="department_id")%>%
    top_n(20)
  
  kable(temp3)
  
  temp3 %>% ggplot(aes(x=reorder(department,-count),y=count, fill=department))+
    geom_bar(stat="identity")+
    theme(axis.text.x=element_text(angle=60, hjust=1),axis.title.x = element_blank(),panel.grid.major = element_line(colour = "grey"))+
    labs(title="Max number of product variety in which Department? (Top 20)",y="Count of Products")
  
  
  #' #### Figure 11
  #' 
  #' 
  #' ### 12. Heat map of Order hour vs order day
  #'Trying to Find out WHich Day and what time do people order the most,
  #'People order the most on Day 0 & Day 1 between 9:00Am to 16:PM<br/>
  
  temp4 <- orders %>%
    mutate(order_dow = as.factor(order_dow)) %>%
    mutate(order_hour_of_day = as.factor(order_hour_of_day)) %>%
    group_by(order_dow,order_hour_of_day) %>%
    summarise(count = n()) %>%
    arrange(desc(count))
    
  kable(head(temp4))
  
  temp4 %>% ggplot(aes(x=order_dow,y=order_hour_of_day))+
    geom_tile(aes(fill=count),colour = "white") + 
    scale_fill_gradient(low = "aquamarine",high = "red")+
    #scale_fill_distiller(palette = "Spectral")+
    theme(axis.text.x=element_text(angle=60, hjust=1),panel.grid.major = element_line(colour = "grey"))+
    labs(title="Which Day and what time do people order the most?",x="Day of the Week", y="Order Hour of the day")
  
  #Had to convert the dow and hour variable as factor variable to generate heat map
  #' #### Figure 12
  #'
  #' ### 13. No of Orders for each Departments?
  
  temp6 <- opp %>% 
   # arrange(desc(-order_id)) %>% 
    left_join(products,by="product_id")%>%
    left_join(departments,by="department_id")%>%
    select(order_id,department)
    #distinct(order_id,department)
  
  kable(head(temp6,10))
  dim(temp6)

  hod <-  sort(table(temp6$department),decreasing = TRUE)
  dim(hod)
  hod <- hod[1:21]
  kable(hod)
  hod <-as.data.frame(hod)
  
  hod %>% ggplot(aes(x=Var1,y=Freq, fill=Var1)) +
    geom_bar(stat="identity")+
    theme(axis.text.x=element_text(angle=60, hjust=1),axis.title.x = element_blank(),panel.grid.major = element_line(colour = "grey"),legend.position="none" )+
    scale_y_continuous(breaks=seq(30000, 9500000, 500000))+
    labs(title="Highest Ordered Department", y= "No of Orders")
  
  #' ### No of Re-Orders for each Departments?
  
  temp6 <- opp %>% 
    left_join(products,by="product_id")%>%
    left_join(departments,by="department_id")%>%
    filter(reordered == 1) %>%
    select(order_id,department)
  
  kable(head(temp6,10))
  dim(temp6)
  
  hrod <-  sort(table(temp6$department),decreasing = TRUE)
  dim(hrod)
  hrod <- hrod[1:21]
  kable(hrod)
  hrod <-as.data.frame(hrod)
  
  hrod %>% ggplot(aes(x=Var1,y=Freq, fill=Var1)) +
    geom_bar(stat="identity")+
    theme(axis.text.x=element_text(angle=60, hjust=1),axis.title.x = element_blank(),panel.grid.major = element_line(colour = "grey"),legend.position="none" )+
    scale_y_continuous(breaks=seq(30000, 9500000, 500000))+
    labs(title="Highest Re-Ordered Department", y= "No of Orders")
  
  #' #### Finiding Departments which are not reordered
  uniq_dept <- hod %>%
    left_join(hrod, by="Var1")%>%
    arrange(desc(Freq.x))
  uniq_dept
  
  #' During reorder in departments, Beverages  jump to 3rd position compared to 4th in first order <br/>
  #' Bakery jumps to 6th position during reordered compared to 7th in the first order <br/>
  #' Deli jumps to 8th position during  reorder compared to 9th in the first order<br/>
  #' Similarly Breakfast & meat seafood also jumps the position during the reorder compared to first time order
  
  
  #' ###  No of Orders for Top 50 Aisles?
  
  temp6 <- opp %>% 
    left_join(products,by="product_id")%>%
    left_join(aisles,by="aisle_id")%>%
    select(order_id,aisle)
  
  kable(head(temp6,10))
  dim(temp6)
  
  hoa <-  sort(table(temp6$aisle),decreasing = TRUE)
  dim(hoa)
  hoa <- hoa[1:50]
  kable(hoa)
  hoa <-as.data.frame(hoa)
  
  hoa %>% ggplot(aes(x=Var1,y=Freq, fill=Var1)) +
    geom_bar(stat="identity")+
    theme(axis.text.x=element_text(angle=60, hjust=1),axis.title.x = element_blank(),panel.grid.major = element_line(colour = "grey"),legend.position="none" )+
    scale_y_continuous(breaks=seq(300000, 3700000, 500000))+
    labs(title="Highest Ordered Aisles - Top 50", y= "No of Orders")
  

  #' ### 13. No of Re-Orders for Top 50 Aisles?
  
  temp6 <- opp %>% 
    left_join(products,by="product_id")%>%
    left_join(aisles,by="aisle_id")%>%
    filter(reordered == 1)
    select(order_id,aisle)
  
  kable(head(temp6,10))
  dim(temp6)
  
  hroa <-  sort(table(temp6$aisle),decreasing = TRUE)
  dim(hroa)
  hroa <- hroa[1:50]
  kable(hroa)
  hroa <-as.data.frame(hroa)
  
  hroa %>% ggplot(aes(x=Var1,y=Freq, fill=Var1)) +
    geom_bar(stat="identity")+
    theme(axis.text.x=element_text(angle=60, hjust=1),axis.title.x = element_blank(),panel.grid.major = element_line(colour = "grey"),legend.position="none" )+
    scale_y_continuous(breaks=seq(100000, 2700000, 500000))+
    labs(title="Highest RE-Ordered Aisles - Top 50", y= "No of Orders")
  
  #' #### Finiding Aisles which are not reordered
  uniq_aisles <- hoa %>%
    anti_join(hroa, by="Var1")
  uniq_aisles
  
  #'oils vinegars, condiments, spices seasonings are three aisles which are not reordered
    
  #' #### Fig
  
  #' Why are there 3 purple dots in prior orders of the customer vs Order Number
  #' 
  #' 
  p<-orders %>% 
  filter(eval_set=="prior")

  p1 <- sort(table(p$order_number), decreasing = TRUE)
  kable(head(p1,10))
  p1 <- as.data.frame(p1)
  p1 %>% ggplot(aes(x=Var1,y=Freq, fill=Var1)) +
    geom_bar(stat="identity")+
    theme(axis.text.x=element_text(angle=60, hjust=1),panel.grid.major = element_line(colour = "grey"))+
    labs(title="Prior Order Customers", x="No of Prior Orders", y="No of Customers")
  
  #' Reason for 3 purple dots was that no. of users who has ordered previously (1,2,3) orders is the same.
  
  #' ### Top 50 most ordered product
  
 temp <- orders  %>%
  inner_join(opp,by="order_id") %>%
  inner_join(products,by="product_id")
 
  summary(temp)  
  kable(head(temp,15))
  highest_ordered_product <- sort(table(temp$product_name),decreasing = TRUE)
  highest_ordered_product <- highest_ordered_product[1:50]
  kable(highest_ordered_product)
  #plot(highest_ordered_product)
  hop <-as.data.frame(highest_ordered_product)

hop %>% ggplot(aes(x=Var1,y=Freq, fill=Var1)) +
    geom_bar(stat="identity")+
    theme(axis.text.x=element_text(angle=60, hjust=1),axis.title.x = element_blank(),panel.grid.major = element_line(colour = "grey"),legend.position="none" )+
    scale_y_continuous(breaks=seq(50000, 480000, 25000))+
    labs(title="Highest Ordered Products - Top 50", y= "No of Orders")
  
#' ### Top 50 most re-ordered product

temp <- orders  %>%
  inner_join(opp,by="order_id") %>%
  inner_join(products,by="product_id") %>%
  filter(reordered==1)

summary(temp)  
kable(head(temp,15))
highest_reordered_product <- sort(table(temp$product_name),decreasing = TRUE)
highest_reordered_product <- highest_reordered_product[1:50]
kable(highest_reordered_product)
#plot(highest_ordered_product)
hrop <-as.data.frame(highest_reordered_product)

hrop %>% ggplot(aes(x=Var1,y=Freq, fill=Var1)) +
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=60, hjust=1),axis.title.x = element_blank(),panel.grid.major = element_line(colour = "grey"),legend.position="none")+
  #coord_cartesian(ylim=c(1000,20000,1000))+
  scale_y_continuous(breaks=seq(50000, 480000, 25000))+
  labs(title="Highest re-Ordered Product - Top 50", y="No of Orders")

#' Finiding unique products which are not reordered
uniq_prod <- hop %>%
  anti_join(hrop, by="Var1")
  #data.frame(unique_prod=union(hop$Var1, hrop$Var1))
uniq_prod

#' Organic Italian Parsley Bunch & Blueberries are 2 products which are part of highest ordered products but are not ordered again
#' 
#' 
#' 
#' ### Looking into Customers - Highest Ordered products in the First Order 
#' 
#' 
temp <- orders  %>%
  inner_join(opp,by="order_id") %>%
  inner_join(products,by="product_id")%>%
  filter(order_number<2)%>%
  select(user_id,order_number,product_id,product_name)
  #filter(add_to_cart_order == 1)
  
  summary(temp)  
  kable(head(temp,15))
  hopfo <- sort(table(temp$product_name),decreasing = TRUE)
  hopfo <- usr[1:50]
  kable(head(hopfo,15))
  dim(hopfo)
  #plot(highest_ordered_product)
  hopfo <-as.data.frame(hopfo)
  
  hopfo %>% ggplot(aes(x=Var1,y=Freq, fill=Var1)) +
    geom_bar(stat="identity")+
    theme(axis.text.x=element_text(angle=60, hjust=1),axis.title.x = element_blank(),panel.grid.major = element_line(colour = "grey"),legend.position="none")+
    #coord_cartesian(ylim=c(1000,20000,1000))+
    scale_y_continuous(breaks=seq(5000, 30000, 5000))+
    labs(title="Highest Ordered Products in First Order - Top 50", y="No of Orders")
  
  #' Checking to see which products our of the highest ordered products, are not part of the first order of the customer
First_prod <- hop %>%
    anti_join(hopfo, by="Var1")
  #data.frame(unique_prod=union(hop$Var1, hrop$Var1))
First_prod

#'    These 4 products inspite of being part of highest ordered products, are not ordered in the first order
#' 1     Organic Whole String Cheese 59676
#' 2      Organic Granny Smith Apple 58779
#' 3 Organic Unsweetened Almond Milk 57895
#' 4                    Spring Water 56087
#' 
#' Now Trying to look into customers first order
#' ### Avg no of products in the first order of the customer


temp <- orders  %>%
  inner_join(opp,by="order_id") %>%
  inner_join(products,by="product_id")%>%
  filter(order_number<2)%>%
  group_by(user_id,order_number)%>%
  summarise(cnt = n())
  #select(user_id,order_number,product_id,product_name)

summary(temp)  
kable(head(temp,15))

#'  Mean   :10.08  means, on an average customer purchases 10 products in the first order
