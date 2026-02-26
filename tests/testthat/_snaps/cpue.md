# cpue errors when input is not numeric

    Code
      cpue("five", 10)
    Condition
      Error:
      ! 'catch' must be numeric, got character.

# cpue uses verbosity when option set to TRUE

    Code
      cpue(100, 10)
    Message
      Processing 1 records using ratio method
    Output
      CPUE Result
      Records:      1 
      Method:       ratio 
      Gear factor:  1 
      Values:       10 

