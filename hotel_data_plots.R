


mindate <- min(hotel_bookings$arrival_date_year)
maxdate <- max(hotel_bookings$arrival_date_year)

ggplot(data = hotel_bookings) +
  geom_bar(mapping = aes(x = market_segment, fill = market_segment)) +
  facet_wrap(~hotel) +
  labs(title = "Market Segment: City vs. Resort Hotels", caption = paste0("Data from: ", mindate, " to ", maxdate), x="Market Segment",
       y="Number of Bookings", text = "is this text outside he plot??>>") +
  theme(axis.text.x = element_blank())





