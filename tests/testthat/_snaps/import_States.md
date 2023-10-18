# imports a wide and long dataset

    Code
      path <- system.file("extdata/", package = "LightLogR")
      file.sleep <- "205_sleepdiary_all_20230904.csv"
      import.Statechanges(file.sleep, path, Datetime.format = "dmyHM",
        State.colnames = c("sleep", "offset"), State.encoding = c("sleep", "wake"),
        ID.colname = record_id, sep = ";", dec = ",")
    Output
      Successfully read in 14 observations from Statechanges-file
      Timezone set is UTC.
      The system timezone is Europe/Berlin. Please correct if necessary!
      Start: 2023-08-28 23:20:00
      End: 2023-09-04 07:25:00
      Timespan: 6.3 days
      Observation intervals: 
         Id    interval.time             n pct  
       1 205   34860s (~9.68 hours)      1 8%   
       2 205   35520s (~9.87 hours)      1 8%   
       3 205   35700s (~9.92 hours)      1 8%   
       4 205   36000s (~10 hours)        1 8%   
       5 205   36900s (~10.25 hours)     1 8%   
       6 205   37020s (~10.28 hours)     1 8%   
       7 205   37920s (~10.53 hours)     1 8%   
       8 205   45780s (~12.72 hours)     1 8%   
       9 205   48480s (~13.47 hours)     1 8%   
      10 205   49200s (~13.67 hours)     1 8%   
      11 205   49500s (~13.75 hours)     1 8%   
      12 205   50040s (~13.9 hours)      1 8%   
      13 205   50580s (~14.05 hours)     1 8%   
      # A tibble: 14 x 3
      # Groups:   Id [1]
         Id    State Datetime           
         <fct> <chr> <dttm>             
       1 205   sleep 2023-08-28 23:20:00
       2 205   wake  2023-08-29 09:37:00
       3 205   sleep 2023-08-29 23:40:00
       4 205   wake  2023-08-30 09:21:00
       5 205   sleep 2023-08-30 23:15:00
       6 205   wake  2023-08-31 09:47:00
       7 205   sleep 2023-08-31 23:15:00
       8 205   wake  2023-09-01 09:30:00
       9 205   sleep 2023-09-01 23:10:00
      10 205   wake  2023-09-02 09:10:00
      11 205   sleep 2023-09-02 22:55:00
      12 205   wake  2023-09-03 08:47:00
      13 205   sleep 2023-09-03 21:30:00
      14 205   wake  2023-09-04 07:25:00

---

    Code
      path <- system.file("extdata/", package = "LightLogR")
      file.sleep <- "205_sleepdiary_all_20230904.csv"
      import.Statechanges(file.sleep, path, Datetime.format = "dmyHM",
        State.colnames = "comments", Datetime.column = sleep, ID.colname = record_id,
        sep = ";", dec = ",", structure = "long")
    Output
      Successfully read in 7 observations from Statechanges-file
      Timezone set is UTC.
      The system timezone is Europe/Berlin. Please correct if necessary!
      Start: 2023-08-28 23:20:00
      End: 2023-09-03 21:30:00
      Timespan: 5.9 days
      Observation intervals: 
        Id    interval.time             n pct  
      1 205   81300s (~22.58 hours)     1 17%  
      2 205   84900s (~23.58 hours)     1 17%  
      3 205   85500s (~23.75 hours)     1 17%  
      4 205   86100s (~23.92 hours)     1 17%  
      5 205   86400s (~1 days)          1 17%  
      6 205   87600s (~1.01 days)       1 17%  
      # A tibble: 7 x 3
      # Groups:   Id [1]
        Id    State                                                Datetime           
        <fct> <chr>                                                <dttm>             
      1 205   Slept longer than usual since my kids are on Summer~ 2023-08-28 23:20:00
      2 205   no                                                   2023-08-29 23:40:00
      3 205   Kids slept in my bed                                 2023-08-30 23:15:00
      4 205   none                                                 2023-08-31 23:15:00
      5 205   woke Up and could Not Fall asleep. went to the dini~ 2023-09-01 23:10:00
      6 205   none                                                 2023-09-02 22:55:00
      7 205   no                                                   2023-09-03 21:30:00

