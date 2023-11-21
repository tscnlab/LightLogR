# sc2interval works as expected

    Code
      sc2interval(sample)
    Output
      # A tibble: 3 x 3
        State Id          Interval                                        
        <chr> <chr>       <Interval>                                      
      1 <NA>  <NA>        2023-08-15 00:00:00 UTC--2023-08-15 06:00:00 UTC
      2 wake  Participant 2023-08-15 06:00:00 UTC--2023-08-15 23:00:00 UTC
      3 sleep Participant 2023-08-15 23:00:00 UTC--2023-08-16 00:00:00 UTC

