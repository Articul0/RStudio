drawDist <- function(datum) {
  hist(
    datum,
    breaks = 50,
    xlab = "Количество смертей в месяц", 
    ylab = "Количество месяцев",
    main = "Hospital 30-Day Death (Mortality) Rates from Heart Attack"
  )
}

getBestHospital <- function(state, criteria) {
  hospitalData = read.csv('results_of_care.csv')
  
  criterias = c(
    'heart attack' = 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack',
    'heart failure' = 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure',
    'pneumonia' = 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
  )
  states = unique(hospitalData[, 'State'])
  
  if (! state %in% states) {
    stop('недопустимый штат')
  }
  if (is.na(criterias[criteria])) {
    stop('недопустимый показатель')
  }

  # Преобразуем значения критерия в число
  hospitalData[, criterias[criteria]] = as.numeric(hospitalData[, criterias[criteria]])
  
  # Удаляем записи с нечисловым значением критерия
  hospitalData = hospitalData[!is.na(hospitalData[, criterias[criteria]]), ]
  
  # Фильтруем по штату
  hospitalData = hospitalData[(hospitalData[, 'State'] == state), ]
  
  # Сортируем оп критерию и инаименованию больницы
  hospitalData = hospitalData[order(hospitalData[, criterias[criteria]], hospitalData[, 'Hospital.Name'], decreasing = FALSE), ]
  
  return(hospitalData[1, 'Hospital.Name'])
}

getHospitalRating <- function(state, criteria, n = "best") {
  hospitalData = read.csv('results_of_care.csv')
  
  criterias = c(
    'heart attack' = 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack',
    'heart failure' = 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure',
    'pneumonia' = 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
  )
  states = unique(hospitalData[, 'State'])
  
  if (! state %in% states) {
    stop('недопустимый штат')
  }
  if (is.na(criterias[criteria])) {
    stop('недопустимый показатель')
  }
  
  # Преобразуем значения критерия в число
  hospitalData[, criterias[criteria]] = as.numeric(hospitalData[, criterias[criteria]])
  
  # Удаляем записи с нечисловым значением критерия
  hospitalData = hospitalData[!is.na(hospitalData[, criterias[criteria]]), ]
  
  # Фильтруем по штату
  hospitalData = hospitalData[(hospitalData[, 'State'] == state), ]
  
  # Сортируем оп значению критерия и наименованию больницы
  hospitalData = hospitalData[order(hospitalData[, criterias[criteria]], hospitalData[, 'Hospital.Name'], decreasing = FALSE), ]
  
  if (nrow(hospitalData) == 0) {
    return(NA)
  }
  
  if (is.numeric(n)) {
    if (nrow(hospitalData) < n) {
      return(NA)
    }
    return(hospitalData[n, 'Hospital.Name'])
  }
  
  if (n == 'best') {
    return(hospitalData[1, 'Hospital.Name'])
  }
  
  if (n == 'worst') {
    return(tail(hospitalData, n = 1)[, 'Hospital.Name'])
  }
}

getRaiting <- function(criteria, n = "best") {
  hospitalData = read.csv('results_of_care.csv')
  
  criterias = c(
    'heart attack' = 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack',
    'heart failure' = 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure',
    'pneumonia' = 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
  )
  states = unique(hospitalData[, 'State'])
  
  if (is.na(criterias[criteria])) {
    stop('недопустимый показатель')
  }
  
  statesData = data.frame(hospital = character(), state = character())
  
  # Преобразуем значения критерия в число
  hospitalData[, criterias[criteria]] = as.numeric(hospitalData[, criterias[criteria]])
  
  # Удаляем записи с нечисловым значением критерия
  hospitalData = hospitalData[!is.na(hospitalData[, criterias[criteria]]), ]
  
  for (state in states) {
    if (is.na(state)) {
      next
    }
    
    hospitalDatum = hospitalData
    
    # Фильтруем по штату
    hospitalDatum = hospitalDatum[(hospitalData[, 'State'] == state), ]
    
    # Сортируем оп значению критерия и наименованию больницы
    hospitalDatum = hospitalDatum[order(hospitalDatum[, criterias[criteria]], hospitalDatum[, 'Hospital.Name'], decreasing = FALSE), ]
    
    
    if (nrow(hospitalDatum) == 0 || nrow(hospitalDatum) < n) {
      datum <- data.frame(NA, state)
    } else if (is.numeric(n)) {
      datum <- data.frame(
          hospitalDatum[n, 'Hospital.Name'],
          hospitalDatum[n, 'State']
        )
    } else if (n == 'best') {
      datum <- data.frame(
          hospitalDatum[1, 'Hospital.Name'], 
          hospitalDatum[1, 'State']
        )
    } else if (n == 'worst') {
      datum <- data.frame(
          tail(hospitalDatum, n = 1)[, 'Hospital.Name'], 
          tail(hospitalDatum, n = 1)[, 'State']
        )
    }
    names(datum) <- c("hospital", "state")
    
    statesData <- rbind(statesData, datum)
  }
  
  return(statesData)
}