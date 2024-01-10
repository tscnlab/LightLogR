# imports a wide and long dataset

    Code
      import_Statechanges(file.sleep, path, Datetime.format = "dmyHM",
        State.colnames = c("sleep", "offset"), State.encoding = c("sleep", "wake"),
        Id.colname = record_id, sep = ";", dec = ",", tz = tz, silent = TRUE)
    Output
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
      import_Statechanges(file.sleep, path, Datetime.format = "dmyHM",
        State.colnames = "comments", Datetime.column = sleep, Id.colname = record_id,
        sep = ";", dec = ",", structure = "long", tz = tz, silent = TRUE)
    Output
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

