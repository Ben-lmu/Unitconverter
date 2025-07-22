  #' Convert temperature
  #'
  #' @param value Numeric temperature value
  #' @param from Unit: "C", "F", or "K"
  #' @param to Unit: "C", "F", or "K"
  #' @return Converted temperature
  #' @export
  convert_temperature <- function(value, from, to) {
    from <- tolower(from)
    to <- tolower(to)
    if (from == to) return(value)
    
    celsius <- switch(from,
                      "c" = value,
                      "f" = (value - 32) * 5 / 9,
                      "k" = value - 273.15,
                      stop("Unsupported unit"))
    
    switch(to,
           "c" = celsius,
           "f" = celsius * 9 / 5 + 32,
           "k" = celsius + 273.15,
           stop("Unsupported unit"))
  }
  
  #' Convert weight
  #'
  #' @param value Numeric weight value
  #' @param from Unit: "g", "kg", "lb", "oz"
  #' @param to Unit: "g", "kg", "lb", "oz"
  #' @return Converted weight
  #' @export
  convert_weight <- function(value, from, to) {
    units <- c(g = 1, kg = 1000, lb = 453.592, oz = 28.3495)
    from <- tolower(from)
    to <- tolower(to)
    if (!(from %in% names(units))||!(to %in% names(units))) {
      stop("Unsupported unit")
    }
    grams <- value * units[from]
    grams / units[to]
  }
  
  #' Convert volume
  #'
  #' @param value Numeric volume value
  #' @param from Unit: "ml", "l", "tsp", "tbsp", "cup"
  #' @param to Unit: "ml", "l", "tsp", "tbsp", "cup"
  #' @return Converted volume
  #' @export
  convert_volume <- function(value, from, to) {
    units <- c(ml = 1, l = 1000, tsp = 4.92892, tbsp = 14.7868, cup = 240)
    from <- tolower(from)
    to <- tolower(to)
    if (!(from %in% names(units))||!(to %in% names(units))) {
      stop("Unsupported unit")
    }
    ml <- value * units[from]
    ml / units[to]
  }