module DateRangePicker.DateRecordParser exposing (DateParts, DateTimeParts, Input(..), InputDate(..), YearAndMonth, datePartsToPosix, dateTimePartsToPosix, parseDateTime, yearAndMonthToPosix, yearToPosix)

import Date exposing (Date)
import Derberos.Date.Core exposing (civilToPosix, posixToCivil)
import Parser exposing ((|.), (|=), Parser)
import Time exposing (Posix, Zone, posixToMillis, utc)


type alias YearAndMonth =
    { year : Int
    , month : Int
    }


type alias TimeParts =
    { hour : Int
    , minutes : Int
    , period : Maybe String
    }


type alias DateParts =
    { year : Int
    , month : Int
    , day : Int
    }


type alias DateTimeParts =
    { year : Int
    , month : Int
    , day : Int
    , hour : Int
    , minute : Int
    }


convert12FormatTo24 : TimeParts -> TimeParts
convert12FormatTo24 timeParts =
    case timeParts.period of
        Just period ->
            if period == "AM" then
                if timeParts.hour == 12 then
                    { timeParts | hour = 0 }

                else
                    timeParts

            else
                { timeParts | hour = timeParts.hour + 12, period = Nothing }

        Nothing ->
            timeParts


type InputDate
    = JustYear Int
    | JustYearAndMonth YearAndMonth
    | FullDate DateParts
    | FullDateTime DateTimeParts


type Input
    = SingleInput InputDate
    | RangeInput InputDate InputDate
    | CustomDate String


match : String -> Parser String
match toMatch =
    Parser.oneOf
        [ Parser.succeed toMatch
            |. Parser.keyword toMatch
        , Parser.succeed toMatch
            |. Parser.keyword (String.toLower toMatch)
        , Parser.succeed toMatch
            |. Parser.keyword (String.toUpper toMatch)
        , Parser.succeed toMatch
            |. Parser.keyword (String.replace " " "" toMatch)
        , Parser.succeed toMatch
            |. Parser.keyword (String.replace " " "" (String.toLower toMatch))
        ]


matchDigit : Int -> Parser Int
matchDigit toMatch =
    Parser.oneOf
        [ Parser.succeed toMatch
            |. Parser.keyword (paddedInt toMatch)
        , Parser.succeed toMatch
            |. Parser.keyword (String.fromInt toMatch)
        ]


fourDigitYear : Parser Int
fourDigitYear =
    Parser.succeed ()
        |. Parser.chompIf Char.isDigit
        |. Parser.chompIf Char.isDigit
        |. Parser.chompIf Char.isDigit
        |. Parser.chompIf Char.isDigit
        |> Parser.mapChompedString
            (\str _ -> String.toInt str |> Maybe.withDefault 0)


twoDigitYear : Parser Int
twoDigitYear =
    Parser.succeed ()
        |. Parser.chompIf Char.isDigit
        |. Parser.chompIf Char.isDigit
        |> Parser.mapChompedString
            convertToFullYear


convertToFullYear : String -> a -> Int
convertToFullYear chompedString _ =
    let
        yearEnding =
            Maybe.withDefault 0 <| String.toInt chompedString

        prefix =
            if yearEnding < 50 then
                "20"

            else
                "19"
    in
    Maybe.withDefault 0 <| String.toInt <| prefix ++ chompedString


fullOrAbbreviatedYearParser : Parser Int
fullOrAbbreviatedYearParser =
    Parser.oneOf
        [ Parser.backtrackable
            fourDigitYear
            |> Parser.andThen Parser.commit
        , twoDigitYear
        ]


dayOfMonthParser : Parser Int
dayOfMonthParser =
    -- todo currently doesn't support 1st, 2nd, 3rd, 4th, etc
    Parser.oneOf
        [ Parser.backtrackable
            (Parser.succeed ()
                |. Parser.chompIf Char.isDigit
                |. Parser.chompIf Char.isDigit
                |> Parser.mapChompedString
                    (\str _ -> String.toInt str |> Maybe.withDefault 0)
                |> Parser.andThen Parser.commit
            )
        , Parser.int
        ]


specificMonthParser : String -> String -> Int -> Parser Int
specificMonthParser fullName abbreviation monthNumericalValue =
    Parser.oneOf
        [ Parser.succeed monthNumericalValue
            |. Parser.keyword fullName
        , Parser.succeed monthNumericalValue
            |. Parser.keyword abbreviation
        , Parser.succeed monthNumericalValue
            |. Parser.keyword (paddedInt monthNumericalValue)
        , Parser.succeed monthNumericalValue
            |. Parser.keyword (String.fromInt monthNumericalValue)
        ]


monthParser : Parser Int
monthParser =
    Parser.oneOf
        [ specificMonthParser "January" "Jan" 1
        , specificMonthParser "February" "Feb" 2
        , specificMonthParser "March" "Mar" 3
        , specificMonthParser "April" "Apr" 4
        , specificMonthParser "May" "May" 5
        , specificMonthParser "June" "Jun" 6
        , specificMonthParser "July" "Jul" 7
        , specificMonthParser "August" "Aug" 8
        , specificMonthParser "September" "Sep" 9
        , specificMonthParser "October" "Oct" 10
        , specificMonthParser "November" "Nov" 11
        , specificMonthParser "December" "Dec" 12
        ]


yearSeparatorParser : Parser ()
yearSeparatorParser =
    Parser.succeed ()
        |. Parser.chompIf (\c -> c == '/' || c == ' ' || c == ',' || c == '-')
        |. Parser.spaces


daySeparatorParser : Parser ()
daySeparatorParser =
    Parser.succeed ()
        |. Parser.chompIf (\c -> c == '/' || c == ' ' || c == '-')
        |. Parser.spaces


rangeSeparator : Parser ()
rangeSeparator =
    Parser.oneOf
        [ Parser.succeed ()
            |. Parser.keyword "to"
        , Parser.succeed ()
            |. Parser.keyword "TO"
        , Parser.succeed ()
            |. Parser.symbol "-"
        ]


twelveDigits : List (Parser Int)
twelveDigits =
    List.map matchDigit <| List.range 1 12


twentyFourDigits : List (Parser Int)
twentyFourDigits =
    List.map matchDigit <| List.range 0 23


minuteDigits : List (Parser Int)
minuteDigits =
    List.map matchDigit <| List.range 0 59


digitsParser : List (Parser Int) -> Parser Int
digitsParser digits =
    Parser.succeed identity
        |= Parser.oneOf
            digits


periodParser : Parser String
periodParser =
    Parser.succeed String.toUpper
        |= Parser.oneOf
            [ match "am"
            , match "pm"
            ]


timeParser : Parser (Maybe TimeParts)
timeParser =
    let
        twelveHour =
            Parser.succeed
                (\hour minutes period ->
                    Just <|
                        convert12FormatTo24
                            { hour = hour
                            , minutes = minutes
                            , period = Just period
                            }
                )
                |= digitsParser twelveDigits
                |. Parser.symbol ":"
                |= digitsParser minuteDigits
                |. Parser.symbol " "
                |. Parser.spaces
                |= periodParser

        twentyFourHour =
            Parser.succeed
                (\hour minutes ->
                    Just <|
                        { hour = hour
                        , minutes = minutes
                        , period = Nothing
                        }
                )
                |= digitsParser twentyFourDigits
                |. Parser.symbol ":"
                |= digitsParser minuteDigits
                |. Parser.spaces
    in
    Parser.oneOf
        [ Parser.backtrackable twelveHour
            |> Parser.andThen Parser.commit
        , twentyFourHour
        , Parser.succeed Nothing
        ]


fullInputDateParser : List String -> Parser Input
fullInputDateParser customDates =
    Parser.oneOf
        [ Parser.backtrackable
            rangeInputDateParser
            |> Parser.andThen Parser.commit
        , Parser.succeed
            (\x -> SingleInput x)
            |= singleInputDateParser
            |. Parser.spaces
            |. Parser.end
        , Parser.succeed
            (\cd -> CustomDate cd)
            |= Parser.oneOf
                (List.map match customDates)
        ]


rangeInputDateParser : Parser Input
rangeInputDateParser =
    Parser.succeed (\start end -> RangeInput start end)
        |= singleInputDateParser
        |. Parser.spaces
        |. rangeSeparator
        |. Parser.spaces
        |= singleInputDateParser
        |. Parser.spaces
        |. Parser.end


createFullInput : Int -> Int -> Int -> Maybe TimeParts -> InputDate
createFullInput month day year time =
    case time of
        Just t ->
            FullDateTime <|
                { year = year
                , month = month
                , day = day
                , hour = t.hour
                , minute = t.minutes
                }

        Nothing ->
            FullDate <| { year = year, month = month, day = day }


createYearInput : Int -> InputDate
createYearInput year =
    JustYear year


createYearMonthInput : Int -> Int -> InputDate
createYearMonthInput month year =
    JustYearAndMonth
        { year = year
        , month = month
        }


singleInputDateParser : Parser InputDate
singleInputDateParser =
    let
        fullDateParser =
            Parser.succeed
                createFullInput
                |= monthParser
                |. daySeparatorParser
                |= dayOfMonthParser
                |. yearSeparatorParser
                |= fullOrAbbreviatedYearParser
                |. Parser.spaces
                |= timeParser

        justYearParser =
            Parser.succeed
                createYearInput
                |= fourDigitYear

        justYearAndMonth =
            Parser.succeed
                createYearMonthInput
                |= monthParser
                |. yearSeparatorParser
                |= fourDigitYear
    in
    Parser.oneOf
        [ Parser.backtrackable
            justYearParser
            |> Parser.andThen Parser.commit
        , Parser.backtrackable
            justYearAndMonth
            |> Parser.andThen Parser.commit
        , fullDateParser
        ]


paddedInt : Int -> String
paddedInt =
    String.fromInt >> String.padLeft 2 '0'


datePartsToIso : Int -> Int -> Int -> String
datePartsToIso year month day =
    String.join "-" [ String.fromInt year, paddedInt month, paddedInt day ]


validateDateViaLibrary : InputDate -> Input -> Result String Input
validateDateViaLibrary inputDate input =
    let
        dateToCheck =
            case inputDate of
                JustYear year ->
                    datePartsToIso year 1 1

                JustYearAndMonth yearAndMonth ->
                    datePartsToIso yearAndMonth.year yearAndMonth.month 1

                FullDate date ->
                    datePartsToIso date.year date.month date.day

                FullDateTime dateRecord ->
                    datePartsToIso dateRecord.year dateRecord.month dateRecord.day
    in
    Result.andThen
        (\_ -> Ok input)
        (Date.fromIsoString dateToCheck)


requireInputToBeInRange : InputDate -> Input -> Result String Input
requireInputToBeInRange inputDate input =
    let
        year =
            case inputDate of
                JustYear int ->
                    int

                JustYearAndMonth yearAndMonth ->
                    yearAndMonth.year

                FullDate date ->
                    date.year

                FullDateTime dateRecord ->
                    dateRecord.year
    in
    if year < 1900 then
        Err "Dates must occur after 1900"

    else if year >= 2100 then
        Err "Dates must occur before 2100"

    else
        Ok input


requireEndToBeAfterStart : Input -> Result String Input
requireEndToBeAfterStart input =
    let
        startMillis startInputDate =
            posixToMillis <| convertInputDateToPosix startInputDate

        endMillis endInputDate =
            posixToMillis <| convertInputDateToPosix endInputDate
    in
    case input of
        SingleInput _ ->
            Ok input

        RangeInput start end ->
            if startMillis start <= endMillis end then
                Ok input

            else
                Err "The starting date must be before the end date!"

        CustomDate _ ->
            Ok input


convertInputDateToPosix : InputDate -> Posix
convertInputDateToPosix inputDate =
    case inputDate of
        JustYear int ->
            yearToPosix int utc

        JustYearAndMonth yearAndMonth ->
            yearAndMonthToPosix yearAndMonth utc

        FullDate dateParts ->
            datePartsToPosix dateParts utc

        FullDateTime dateTimeParts ->
            dateTimePartsToPosix dateTimeParts utc


validate : (InputDate -> Input -> Result String Input) -> Input -> Result String Input
validate validateInputDateFunc input =
    case input of
        SingleInput inputDate ->
            validateInputDateFunc inputDate input

        RangeInput start end ->
            Result.andThen
                (\_ -> validateInputDateFunc end input)
                (validateInputDateFunc start input)

        CustomDate _ ->
            Ok input


yearToPosix : Int -> Zone -> Posix
yearToPosix year zone =
    let
        dateRecord =
            { year = year
            , month = 1
            , day = 1
            , hour = 0
            , minute = 0
            , second = 0
            , millis = 0
            , zone = zone
            }
    in
    civilToPosix dateRecord


yearAndMonthToPosix : YearAndMonth -> Zone -> Posix
yearAndMonthToPosix { year, month } zone =
    let
        dateRecord =
            { year = year
            , month = month
            , day = 1
            , hour = 0
            , minute = 0
            , second = 0
            , millis = 0
            , zone = zone
            }
    in
    civilToPosix dateRecord


datePartsToPosix : DateParts -> Zone -> Posix
datePartsToPosix { year, month, day } zone =
    let
        dateRecord =
            { year = year
            , month = month
            , day = day
            , hour = 0
            , minute = 0
            , second = 0
            , millis = 0
            , zone = zone
            }
    in
    civilToPosix dateRecord


dateTimePartsToPosix : DateTimeParts -> Zone -> Posix
dateTimePartsToPosix { year, month, day, hour, minute } zone =
    let
        dateRecord =
            { year = year
            , month = month
            , day = day
            , hour = hour
            , minute = minute
            , second = 0
            , millis = 0
            , zone = zone
            }
    in
    civilToPosix dateRecord


parseDateTime : List String -> String -> Result String Input
parseDateTime customDateInputs =
    Parser.run (fullInputDateParser customDateInputs)
        >> Result.mapError (always "")
        >> Result.andThen (validate validateDateViaLibrary)
        >> Result.mapError (always "Not a valid US Date!")
        >> Result.andThen (validate requireInputToBeInRange)
        >> Result.andThen requireEndToBeAfterStart
