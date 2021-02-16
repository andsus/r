leap <- function(year) {
    isDivBy <- function(divisor) { year %% divisor == 0 }

    (isDivBy(4) && (!isDivBy(100) || isDivBy(400)) )

}
