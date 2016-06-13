type weekday = 
    |Mon
    |Tue
    |Wed
    |Thu
    |Fri
    |Sat
    |Sun
    ;;

let etym day =
    match day with
        |Mon -> "Moon"
        |Tue -> "Tyr"
        |Wed -> "Odin"
        |Thu -> "Thor"
        |Fri -> "Frigga"
        |Sat -> "Loki"
        |Sun -> "Sun"
        ;;
