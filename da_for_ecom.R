setwd("E:/Aar Riana/DS/dqLAB/dataset+lomba+4+sept+2020")
products <- read.csv("products.csv", sep = ";")
orders <- read.csv("orders.csv", sep = ";")
users <- read.csv("users.csv", sep = ";")
order_details <- read.csv("order_details.csv", sep = ";")

library(sqldf)
library(ggplot2)
library(mice)
library(dplyr)
str(order_details)
str(orders)
str(users)
str(products)

#change to date format
orders$created_at <- as.Date(orders$created_at)

#check misssing value
md.pattern(orders)

orders %>% 
  group_by(Month_Yr <- format(as.Date(created_at), "%Y-%m")) %>%
  summarise(transaksi = n())
str(orders)
summary(orders$delivery_at)
dataz = sqldf("select count(order_id) from orders
              where paid_at = delivery_at ")
dataz
sqldf("select count(order_id) from orders
              where paid_at is not null and delivery_at is not null")
str(users)
str(orders)
sqldf("select count(distinct user_id) from users")
sqldf("select count(distinct seller_id) from orders")
sqldf("select count(distinct buyer_id) from orders")
#sqldf("select count(order_id) from orders where seller_id = buyer_id")
View(sqldf("select count(distinct user_id) from users left outer join
      orders on user_id = buyer_id where created_at is null"))


#mana saja yang merupakan 5 pembeli dengan dengan total pembelian terbesar (berdasarkan total harga barang setelah diskon)
top_buyer <- orders %>%
  group_by(buyer_id) %>%
  summarise(total=max(total))
top_buyer %>% arrange(desc(total))
str(orders)
str(users)

#top buyer pembelian termahal
top_buyer = sqldf("select user_id, nama_user, total from users left outer join
      (select buyer_id, sum(total) as total from orders group by
      buyer_id) on user_id = buyer_id order by 3 desc limit 5")

plt=ggplot(data = top_buyer, aes(x=reorder(nama_user, total), y=total, fill=nama_user)) +
  geom_bar(stat = "identity", width = 0.5, aes(y = 	total)) + 
  #geom_text(aes(x = nama_user, y = total/2, label = total), colour = "white") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma)+ scale_fill_manual(values =c("#638c80","#638c80","#638c80","#638c80","#638c80")) +
  labs(colour = "", x = "", y = "Total Pembelian", 
       title = "Top Buyer All Time", subtitle = "5 pembeli dengan total pembelian terbesar berdasarkan harga produk setelah diskon.") + 
  theme(axis.text = element_text(size = 12, face = "bold"),
        plot.title = element_text(hjust = 0, size = 16), 
        panel.background = element_rect(fill = "white"), 
        legend.position = "bottom") 
options(repr.plt.width = 20, repr.plt.height = 5)
plt
  
#frequent buyer
freq_buy =sqldf("select user_id, nama_user, freq from users left outer join
      (select buyer_id, count(*) as freq from orders where discount=0 group by
      buyer_id) on user_id = buyer_id order by 3 desc limit 5")

ggplot(data = freq_buy, aes(x=reorder(nama_user, freq), y=freq)) +
  geom_bar(stat = "identity", width = 0.5, fill="seagreen3", aes(y = 	freq)) + 
  geom_text(aes(x = nama_user, y = freq/2, label = freq), colour = "white") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma)+ scale_fill_manual(values =c("#638c80","#638c80","#638c80","#638c80","#638c80")) +
  labs(colour = "", x = "", y = "Total Pembelian", 
       title = "Top Frequent Buyer", subtitle = "5 pembeli dengan transaksi terbanyak tanpa menggunakan diskon.") + 
  theme(axis.text = element_text(size = 12, face = "bold"),
        axis.text.x = element_blank(), 
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0, size = 16), 
        panel.background = element_rect(fill = "white"), 
        legend.position = "bottom")
options(repr.plt.width = 10, repr.plt.height = 2)

orders1 <- orders
orders1$month <- format(as.Date(orders1$created_at), "%m")
orders1$years <- format(as.Date(orders1$created_at), "%Y")
str(orders1)
orders_month <- orders1 %>% 
  group_by(buyer_id, month, years) %>%
  summarise(transaksi = n(), total = mean(total))
sqldf("select * from orders_month")  

#big frequent buyer 2020
sqldf("select user_id, email,month, years, transaksi, total from users
      join
      (select buyer_id, transaksi, total, month, years
      from orders_month where years='2020' and transaksi>=1 and total>1000000 group by buyer_id)
      on user_id = buyer_id group by user_id,month order by month limit 10")

test = sqldf("select distinct(orders_month.buyer_id), month, transaksi,tot from orders_month 
              left outer join
             (select buyer_id, count(distinct month) as sm, avg(total) as tot 
             from orders_month
             where years='2020' group by 1 order by 2 desc) as b on b.buyer_id = orders_month.buyer_id
             where sm = 5 and years = '2020' ")
View(test)
# cek = sqldf("select buyer_id, count(distinct month) as sm 
#              from orders_month
#             where years='2020' group by buyer_id order by 2 desc")
# View(cek)
sqldf("select distinct user_id, email from users join
      orders on user_id = seller_id")

###Top 5 Product Desember 2019
sqldf("select order_id from orders1
      where month='12' and years = '2019'")
sqldf(" select products.product_id, desc_product,qty from products join
      (select product_id, sum(quantity) as qty 
      from order_details join
      (select order_id from orders1
      where month='12' and years = '2019') as a on order_details.order_id = a.order_id
      group by 1) as b on products.product_id = b.product_id 
      order by 3 desc limit 5")

sqldf("select product_id, month,years, quantity
      from order_details join
      (select order_id,month,years from orders1
      where month='12' and years = '2019') as a on order_details.order_id = a.order_id
      order by 4 desc limit 20")

#10 Transaksi terbesar user 12476
sqldf("select seller_id, buyer_id, total as nilai_transaksi, created_at as tanggal_transaksi
      from orders
      where buyer_id = 12476
      order by 3 desc
      limit 10")
#Transaksi per bulan
sqldf("select EXTRACT(YEAR_MONTH FROM created_at) as tahun_bulan, count(1) as jumlah_transaksi, sum(total) as total_nilai_transaksi
      from orders
      where created_at>='2020-01-01'
      group by 1
      order by 1")
s =sqldf("select strftime('%m',created_at) as bulan, count(1) as jumlah_transaksi, sum(total) as total_nilai_transaksi
      from orders
      where created_at>='2020-01-01'
      group by 1
      order by 1")
View(s)
z = sqldf("select strftime('%m',created_at) as bulan, count(1) as jumlah_transaksi, sum(total) as total_nilai_transaksi
      from orders
      where created_at>='2020-01-01'
      group by 1
      order by 1")
p <- ggplot(data = s, aes(x = bulan, y = total_nilai_transaksi)) +
  geom_col(aes(fill = bulan), width = 0.7)+ coord_flip() +
  geom_text(aes(y = total_nilai_transaksi/2, label = total_nilai_transaksi), color = "white")
p + labs(colour = "", x = "", y = "", 
         title = "Total Penjualan tahun 2020", subtitle = "Total nilai transaksi bulanan dalam Miliyar Rupiah") + 
  theme(axis.text = element_text(size = 12, face = "bold"),
        #axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0, size = 16), 
        panel.background = element_rect(fill = "white"), 
        legend.position = "bottom")
s$total_nilai_transaksi <- as.numeric(s$total_nilai_transaksi/1000000000)
str(s)
s$year = "2020"
sqldf("select * from orders")

#Pengguna dengan rata-rata transaksi terbesar di Januari 2020
sqldf("select buyer_id, count(1) as jumlah_transaksi, avg(total) as avg_nilai_transaksi
        from orders
        where created_at>='2020-01-01' and created_at<'2020-02-01'
        group by 1
        having count(1)>= 2 
        order by 3 desc
        limit 10")

#Transaksi besar di Desember 2019
sqldf("select nama_user as nama_pembeli, total as nilai_transaksi, created_at as tanggal_transaksi
      from orders
      inner join users on buyer_id = user_id
      where created_at>='2019-12-01' and created_at<'2020-01-01'
      and total >= 20000000
      order by 1 limit 5")

#Kategori Produk Terlaris di 2020
d =sqldf("select category, sum(quantity) as total_quantity, sum(price) as total_price
from orders
      inner join order_details using(order_id)
      inner join products using(product_id)
      where created_at>='2020-01-01'
      and delivery_at>= '2020-01-01'
      group by 1
      order by 2 desc
      limit 5")

##Mencari pembeli high value
sqldf("select nama_user as nama_pembeli, count(1) as jumlah_transaksi, sum(total) as total_nilai_transaksi, min(total) as min_nilai_transaksi
      from orders inner join users
      on buyer_id = user_id
      group by user_id,nama_user
      having count(1)> 5 and min(total)>2000000
      order by 3 desc")

#Mencari Dropshipper
sqldf("select nama_user as nama_pembeli, count(1) as jumlah_transaksi, count(distinct orders.kodepos) as distinct_kodepos,
      sum(total) as total_nilai_transaksi, avg(total) as avg_nilai_transaksi
      from orders inner join users
      on buyer_id = user_id
      group by user_id,nama_user
      having count(1)>=10 and count(1) = count(distinct orders.kodepos) 
      order by 2 desc")
z = sqldf("select nama_user as nama_pembeli, count(1) as jumlah_transaksi, count(distinct orders.kodepos) as distinct_kodepos,
      sum(total) as total_nilai_transaksi, avg(total) as avg_nilai_transaksi
      from orders inner join users
      on buyer_id = user_id
      group by user_id,nama_user
      having count(1)>=10 and count(1) = count(distinct orders.kodepos) 
      order by 2 desc")

library(reshape)
df1 <- data.frame(z$jumlah_transaksi, z$distinct_kodepos, z$total_nilai_transaksi, z$nama_pembeli)
df2 <- melt(df1)
head(df2)
# To separate the bars slightly:
position = position_dodge(width = .75)
width = .65
ggplot(df2, aes(x=nama_pembeli, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  coord_flip() +
  geom_text(aes(y = value, label = value, group =variable), color = "black", position = position) +
  labs(colour = "", x = "", y = "", 
       title = "Dropshipper") + 
  theme(axis.text = element_text(size = 12, face = "bold"),
        #axis.text.x = element_blank(), 
        axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0, size = 16), 
        panel.background = element_rect(fill = "white"), 
        legend.position = "bottom")

##Mencari Reseller Offline
str(order_details)
sqldf("select nama_user as nama_pembeli, count(1) as jumlah_transaksi,
sum(total) as total_nilai_transaksi, avg(total) as avg_nilai_transaksi, avg(total_quantity) as avg_quantity_per_transaksi
      from orders 
      inner join users on buyer_id = user_id
      inner join (select order_id, sum(quantity) as total_quantity from order_details group by 1) as summary_order using(order_id)
      where orders.kodepos = users.kodepos
      group by user_id,nama_user
      having count(1)>=8 and avg(total_quantity) > 10
      order by 3 desc")

#Pembeli sekaligus penjual
sqldf("select nama_user as nama_pembeli, jumlah_transaksi_beli, jumlah_transaksi_jual
from users inner join
      (select buyer_id, count(buyer_id) as jumlah_transaksi_beli from orders group by 1) as summary_buyer
      on user_id = summary_buyer.buyer_id
      inner join
      (select seller_id, count(seller_id) as jumlah_transaksi_jual from orders group by 1) as summary_seller
      on user_id = summary_seller.seller_id
      where jumlah_transaksi_beli>=7
      order by 1")
a =sqldf("select nama_user as nama_pembeli, jumlah_transaksi_beli, jumlah_transaksi_jual
from users inner join
      (select buyer_id, count(1) as jumlah_transaksi_beli from orders group by 1) as buyer
      on user_id = buyer.buyer_id
      inner join
      (select seller_id, count(1) as jumlah_transaksi_jual from orders group by 1) as seller
      on user_id = seller.seller_id
      where jumlah_transaksi_beli >= 7
      order by 1")
df2 <- melt(a)
df2
p <- ggplot(data = df2, aes(x = nama_pembeli, y = value)) +
  geom_col(aes(fill = variable), width = 0.7)+
  geom_text(aes(y = value, label = value, group =variable), color = "white")
p + coord_flip()

##
sqldf("select EXTRACT(YEAR_MONTH FROM created_at) as tahun_bulan, 
      count(1) as jumlah_transaksi, 
      avg(DATEDIFF(paid_at,created_at)) as avg_lama_dibayar, 
      min(DATEDIFF(paid_at,created_at)) as min_lama_dibayar, 
      max(DATEDIFF(paid_at,created_at)) as max_lama_dibayar 
      from orders
      group by 1
      order by 1")
sqldf("select EXTRACT(YEAR_MONTH from created_at) as tahun_bulan, 
      count(1) as jumlah_transaksi, avg(datediff(paid_at,created_at)) as avg_lama_dibayar, 
      min(datediff(paid_at,created_at)) min_lama_dibayar, max(datediff( paid_at,created_at)), 
      max_lama_dibayar from orders where paid_at is not null group by 1 order by 1")

#domain email seller
dom_seller = sqldf("select (SUBSTR(email, INSTR(email, '@') + 1)) as a,count(*) as c
FROM users
      group by a
      order by c desc;")

#make word cloud
install.packages("ggwordcloud")
install.packages("showtext")
library(ggwordcloud)
library(showtext)
font_add_google("Lacquer")
showtext_auto()

d %>%
  ggplot(
    aes(
      label = category,
      size = total_quantity,
      color=category
    )
  ) +
  geom_text_wordcloud_area(family = "Lacquer") +
  scale_size_area(max_size = 20) +
  #scale_colour_manual(values = c("#009AB3", "#B0E601")) +
  theme_void() #+
  #theme(plot.background = element_rect(fill = "#1E1E1E"))

#
cat = sqldf("select category as a, count(1) as c
      from products group by a order by c desc")
cat
summary(products$base_price)
products %>% ggplot(aes(x=base_price)) + barplot(height = 1000000)
