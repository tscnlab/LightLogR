# gg_photoperiod works

    Code
      Plot$data
    Output
      # A tibble: 69,120 x 10
      # Groups:   Id [2]
         Id       Datetime             MEDI Day.data Time   .group dawn               
         <fct>    <dttm>              <dbl> <fct>    <time>  <int> <dttm>             
       1 Partici~ 2023-08-29 00:00:04     0 29/08    00'04"      2 2023-08-29 06:01:10
       2 Partici~ 2023-08-29 00:00:14     0 29/08    00'14"      2 2023-08-29 06:01:10
       3 Partici~ 2023-08-29 00:00:24     0 29/08    00'24"      2 2023-08-29 06:01:10
       4 Partici~ 2023-08-29 00:00:34     0 29/08    00'34"      2 2023-08-29 06:01:10
       5 Partici~ 2023-08-29 00:00:44     0 29/08    00'44"      2 2023-08-29 06:01:10
       6 Partici~ 2023-08-29 00:00:54     0 29/08    00'54"      2 2023-08-29 06:01:10
       7 Partici~ 2023-08-29 00:01:04     0 29/08    01'04"      2 2023-08-29 06:01:10
       8 Partici~ 2023-08-29 00:01:14     0 29/08    01'14"      2 2023-08-29 06:01:10
       9 Partici~ 2023-08-29 00:01:24     0 29/08    01'24"      2 2023-08-29 06:01:10
      10 Partici~ 2023-08-29 00:01:34     0 29/08    01'34"      2 2023-08-29 06:01:10
      # i 69,110 more rows
      # i 3 more variables: dusk <dttm>, photoperiod <drtn>, photoperiod.state <chr>

---

    Code
      Plot2$data
    Output
      # A tibble: 69,120 x 8
      # Groups:   Id [2]
         Id          Datetime             MEDI .group dawn               
         <fct>       <dttm>              <dbl>  <int> <dttm>             
       1 Participant 2023-08-29 00:00:04     0      2 2023-08-29 06:01:10
       2 Participant 2023-08-29 00:00:14     0      2 2023-08-29 06:01:10
       3 Participant 2023-08-29 00:00:24     0      2 2023-08-29 06:01:10
       4 Participant 2023-08-29 00:00:34     0      2 2023-08-29 06:01:10
       5 Participant 2023-08-29 00:00:44     0      2 2023-08-29 06:01:10
       6 Participant 2023-08-29 00:00:54     0      2 2023-08-29 06:01:10
       7 Participant 2023-08-29 00:01:04     0      2 2023-08-29 06:01:10
       8 Participant 2023-08-29 00:01:14     0      2 2023-08-29 06:01:10
       9 Participant 2023-08-29 00:01:24     0      2 2023-08-29 06:01:10
      10 Participant 2023-08-29 00:01:34     0      2 2023-08-29 06:01:10
      # i 69,110 more rows
      # i 3 more variables: dusk <dttm>, photoperiod <drtn>, photoperiod.state <chr>

---

    Code
      Plot3$data
    Output
      # A tibble: 126,720 x 9
      # Groups:   Id, Date.data [12]
         Id          Datetime             MEDI Date.data  .group dawn               
         <fct>       <dttm>              <dbl> <date>      <int> <dttm>             
       1 Environment 2023-08-29 00:00:08     0 2023-08-29      1 2023-08-29 06:01:10
       2 Environment 2023-08-29 00:00:38     0 2023-08-29      1 2023-08-29 06:01:10
       3 Environment 2023-08-29 00:01:08     0 2023-08-29      1 2023-08-29 06:01:10
       4 Environment 2023-08-29 00:01:38     0 2023-08-29      1 2023-08-29 06:01:10
       5 Environment 2023-08-29 00:02:08     0 2023-08-29      1 2023-08-29 06:01:10
       6 Environment 2023-08-29 00:02:38     0 2023-08-29      1 2023-08-29 06:01:10
       7 Environment 2023-08-29 00:03:08     0 2023-08-29      1 2023-08-29 06:01:10
       8 Environment 2023-08-29 00:03:38     0 2023-08-29      1 2023-08-29 06:01:10
       9 Environment 2023-08-29 00:04:08     0 2023-08-29      1 2023-08-29 06:01:10
      10 Environment 2023-08-29 00:04:38     0 2023-08-29      1 2023-08-29 06:01:10
      # i 126,710 more rows
      # i 3 more variables: dusk <dttm>, photoperiod <drtn>, photoperiod.state <chr>

---

    Code
      Plot4$data
    Output
      # A tibble: 69,120 x 10
      # Groups:   Id [2]
         Id          Datetime             MEDI dawn                dusk               
         <fct>       <dttm>              <dbl> <dttm>              <dttm>             
       1 Participant 2023-08-29 00:00:04     0 2023-08-29 06:01:10 2023-08-29 19:20:36
       2 Participant 2023-08-29 00:00:14     0 2023-08-29 06:01:10 2023-08-29 19:20:36
       3 Participant 2023-08-29 00:00:24     0 2023-08-29 06:01:10 2023-08-29 19:20:36
       4 Participant 2023-08-29 00:00:34     0 2023-08-29 06:01:10 2023-08-29 19:20:36
       5 Participant 2023-08-29 00:00:44     0 2023-08-29 06:01:10 2023-08-29 19:20:36
       6 Participant 2023-08-29 00:00:54     0 2023-08-29 06:01:10 2023-08-29 19:20:36
       7 Participant 2023-08-29 00:01:04     0 2023-08-29 06:01:10 2023-08-29 19:20:36
       8 Participant 2023-08-29 00:01:14     0 2023-08-29 06:01:10 2023-08-29 19:20:36
       9 Participant 2023-08-29 00:01:24     0 2023-08-29 06:01:10 2023-08-29 19:20:36
      10 Participant 2023-08-29 00:01:34     0 2023-08-29 06:01:10 2023-08-29 19:20:36
      # i 69,110 more rows
      # i 5 more variables: photoperiod <drtn>, photoperiod.state <chr>,
      #   Day.data <fct>, Time <time>, .group <int>

