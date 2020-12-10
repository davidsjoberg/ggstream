blockbusters <- read.csv("./data-raw/Blockbusters_2019-1977.csv", stringsAsFactors = F) %>%
  mutate(worldwide_gross = as.numeric(as.character(worldwide_gross %>% str_remove_all(","))),
         year = as.numeric(as.character(release_year)),
         film_title = str_squish(film_title)) %>%
  group_by(year, genre = genre_1, film_title) %>%
  summarise(box_office = sum(worldwide_gross)) %>%
  ungroup() %>%
  drop_na()

# Source https://www.thebalance.com/u-s-inflation-rate-history-by-year-and-forecast-3306093
# Read on 2020-12-10
dollars <- structure(list(year = c(2019, 2018, 2017, 2016, 2015, 2014, 2013,
                                   2012, 2011, 2010, 2009, 2008, 2007, 2006, 2005, 2004, 2003, 2002,
                                   2001, 2000, 1999, 1998, 1997, 1996, 1995, 1994, 1993, 1992, 1991,
                                   1990, 1989, 1988, 1987, 1986, 1985, 1984, 1983, 1982, 1981, 1980,
                                   1979, 1978, 1977), rate = c(1.023, 1.019, 1.021, 1.021, 1.007,
                                                               1.008, 1.015, 1.017, 1.03, 1.015, 1.027, 1.001, 1.041, 1.025,
                                                               1.034, 1.033, 1.019, 1.024, 1.016, 1.034, 1.027, 1.016, 1.017,
                                                               1.033, 1.025, 1.027, 1.027, 1.029, 1.031, 1.061, 1.046, 1.044,
                                                               1.044, 1.011, 1.038, 1.039, 1.038, 1.038, 1.089, 1.125, 1.133,
                                                               1.09, 1.067), cum = c(1.023, 1.042437, 1.064328177, 1.086679068717,
                                                                                     1.09428582219802, 1.1030401087756, 1.11958571040724, 1.13861866748416,
                                                                                     1.17277722750868, 1.19036888592131, 1.22250884584119, 1.22373135468703,
                                                                                     1.2739043402292, 1.30575194873493, 1.35014751499192, 1.39470238298665,
                                                                                     1.4212017282634, 1.45531056974172, 1.47859553885758, 1.52886778717874,
                                                                                     1.57014721743257, 1.59526957291149, 1.62238915565099, 1.67592799778747,
                                                                                     1.71782619773215, 1.76420750507092, 1.81184110770784, 1.86438449983136,
                                                                                     1.92218041932614, 2.03943342490503, 2.13324736245066, 2.22711024639849,
                                                                                     2.32510309724002, 2.35067923130966, 2.44000504209943, 2.53516523874131,
                                                                                     2.63150151781348, 2.73149857549039, 2.97460194870904, 3.34642719229767,
                                                                                     3.79150200887326, 4.13273718967185, 4.40963058137986)), class = c("tbl_df",
                                                                                                                                                       "tbl", "data.frame"), row.names = c(NA, -43L))

blockbusters <- blockbusters %>%
  filter(!genre %in% c("Mystery", "Horror", "Musical ", "Sci-Fi", "Biography", "Crime", "Family")) %>%
  group_by(year, genre) %>%
  summarise(box_office = sum(box_office)/1e9) %>%
  ungroup() %>%
  left_join(dollars) %>%
  transmute(year,
            genre,
            box_office = box_office * cum)

usethis::use_data(blockbusters, overwrite = T)
