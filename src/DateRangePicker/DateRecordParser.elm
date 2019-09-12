module DateRangePicker.DateRecordParser exposing (DateParts, DateTimeParts, Input(..), InputDate(..), Language, ParsingConfig, YearAndMonth, datePartsToPosix, dateTimePartsToPosix, parseDateTime, yearAndMonthToPosix, yearToPosix)

import Date exposing (Date)
import Derberos.Date.Core exposing (civilToPosix)
import Parser exposing ((|.), (|=), Parser)
import Time exposing (Month(..), Posix, Zone, posixToMillis, utc)


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


type alias Language =
    { toMonthName : Month -> String
    , toMonthAbbreviation : Month -> String
    , am : String
    , pm : String
    , to : String
    , andBefore : String
    , andAfter : String
    }


type alias ParsingConfig =
    { customDateInputs : List String
    , language : Language
    , allowTime : Bool
    }


convert12FormatTo24 : Language -> TimeParts -> TimeParts
convert12FormatTo24 language timeParts =
    case timeParts.period of
        Just period ->
            if period == String.toUpper language.am then
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
    | BeforeInput InputDate
    | AfterInput InputDate
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
            |. Parser.keyword (String.toLower fullName)
        , Parser.succeed monthNumericalValue
            |. Parser.keyword (String.toLower abbreviation)
        , Parser.succeed monthNumericalValue
            |. Parser.keyword abbreviation
        , Parser.succeed monthNumericalValue
            |. Parser.keyword (paddedInt monthNumericalValue)
        , Parser.succeed monthNumericalValue
            |. Parser.keyword (String.fromInt monthNumericalValue)
        ]


monthParser : Language -> Parser Int
monthParser language =
    Parser.oneOf
        [ specificMonthParser (language.toMonthName Jan) (language.toMonthAbbreviation Jan) 1
        , specificMonthParser (language.toMonthName Feb) (language.toMonthAbbreviation Feb) 2
        , specificMonthParser (language.toMonthName Mar) (language.toMonthAbbreviation Mar) 3
        , specificMonthParser (language.toMonthName Apr) (language.toMonthAbbreviation Apr) 4
        , specificMonthParser (language.toMonthName May) (language.toMonthAbbreviation May) 5
        , specificMonthParser (language.toMonthName Jun) (language.toMonthAbbreviation Jun) 6
        , specificMonthParser (language.toMonthName Jul) (language.toMonthAbbreviation Jul) 7
        , specificMonthParser (language.toMonthName Aug) (language.toMonthAbbreviation Aug) 8
        , specificMonthParser (language.toMonthName Sep) (language.toMonthAbbreviation Sep) 9
        , specificMonthParser (language.toMonthName Oct) (language.toMonthAbbreviation Oct) 10
        , specificMonthParser (language.toMonthName Nov) (language.toMonthAbbreviation Nov) 11
        , specificMonthParser (language.toMonthName Dec) (language.toMonthAbbreviation Dec) 12
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


rangeSeparator : Language -> Parser ()
rangeSeparator language =
    Parser.oneOf
        [ Parser.succeed ()
            |. Parser.keyword language.to
        , Parser.succeed ()
            |. Parser.keyword (String.toUpper language.to)
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


periodParser : Language -> Parser String
periodParser language =
    Parser.succeed String.toUpper
        |= Parser.oneOf
            [ match language.am
            , match language.pm
            ]


timeParser : Language -> Parser (Maybe TimeParts)
timeParser language =
    let
        twelveHour =
            Parser.succeed
                (\hour minutes period ->
                    Just <|
                        convert12FormatTo24
                            language
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
                |= periodParser language

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


fullSingleInputParser : ParsingConfig -> Parser Input
fullSingleInputParser parsingConfig =
    Parser.succeed
        (\x -> SingleInput x)
        |= singleInputDateParser parsingConfig
        |. Parser.spaces
        |. Parser.end


fullInputDateParser : ParsingConfig -> Parser Input
fullInputDateParser parsingConfig =
    Parser.oneOf
        [ rangeInputWithWildcardsDateParser parsingConfig
        , Parser.backtrackable
            (rangeInputDateParser parsingConfig)
            |> Parser.andThen Parser.commit
        , fullSingleInputParser parsingConfig
        , Parser.succeed
            (\cd -> CustomDate cd)
            |= Parser.oneOf
                (List.map match parsingConfig.customDateInputs)
        ]


rangeInputDateParser : ParsingConfig -> Parser Input
rangeInputDateParser parsingConfig =
    Parser.succeed (\start end -> RangeInput start end)
        |= singleInputDateParser parsingConfig
        |. Parser.spaces
        |. rangeSeparator parsingConfig.language
        |. Parser.spaces
        |= singleInputDateParser parsingConfig
        |. Parser.spaces
        |. Parser.end


rangeInputWithWildcardsDateParser : ParsingConfig -> Parser Input
rangeInputWithWildcardsDateParser parsingConfig =
    Parser.oneOf
        [ beforeWildCard parsingConfig
        , Parser.backtrackable
            (otherWildCard parsingConfig)
            |> Parser.andThen Parser.commit
        ]


beforeWildCard : ParsingConfig -> Parser Input
beforeWildCard parsingConfig =
    Parser.succeed (\date -> BeforeInput date)
        |. Parser.symbol "*"
        |. Parser.spaces
        |. rangeSeparator parsingConfig.language
        |. Parser.spaces
        |= singleInputDateParser parsingConfig


otherWildCard : ParsingConfig -> Parser Input
otherWildCard parsingConfig =
    Parser.succeed
        (\date isBefore ->
            if isBefore then
                BeforeInput date

            else
                AfterInput date
        )
        |= singleInputDateParser parsingConfig
        |. Parser.spaces
        |. rangeSeparator parsingConfig.language
        |. Parser.spaces
        |= Parser.oneOf
            [ after parsingConfig.language
            , before parsingConfig.language
            ]


after : Language -> Parser Bool
after language =
    Parser.succeed False
        |. Parser.oneOf
            [ Parser.symbol "*"
            , Parser.succeed () |. match language.andAfter
            ]


before : Language -> Parser Bool
before language =
    Parser.succeed True
        |. match language.andBefore


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


singleInputDateParser : ParsingConfig -> Parser InputDate
singleInputDateParser parsingConfig =
    let
        fullDateTimeParser =
            Parser.succeed
                createFullInput
                |= monthParser parsingConfig.language
                |. daySeparatorParser
                |= dayOfMonthParser
                |. yearSeparatorParser
                |= fullOrAbbreviatedYearParser
                |. Parser.spaces
                |= timeParser parsingConfig.language

        fullDateParser =
            Parser.succeed
                (\month day year -> FullDate <| { year = year, month = month, day = day })
                |= monthParser parsingConfig.language
                |. daySeparatorParser
                |= dayOfMonthParser
                |. yearSeparatorParser
                |= fullOrAbbreviatedYearParser

        justYearParser =
            Parser.succeed
                createYearInput
                |= fourDigitYear

        justYearAndMonth =
            Parser.succeed
                createYearMonthInput
                |= monthParser parsingConfig.language
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
        , if parsingConfig.allowTime then
            fullDateTimeParser

          else
            fullDateParser
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
        RangeInput start end ->
            if startMillis start <= endMillis end then
                Ok input

            else
                Err "The starting date must be before the end date!"

        _ ->
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

        BeforeInput inputDate ->
            validateInputDateFunc inputDate input

        AfterInput inputDate ->
            validateInputDateFunc inputDate input


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


parseDateTime : ParsingConfig -> String -> Result String Input
parseDateTime parsingConfig =
    Parser.run (fullInputDateParser parsingConfig)
        >> Result.mapError (always "")
        >> Result.andThen (validate validateDateViaLibrary)
        >> Result.mapError (always "Not a valid US Date!")
        >> Result.andThen (validate requireInputToBeInRange)
        >> Result.andThen requireEndToBeAfterStart
