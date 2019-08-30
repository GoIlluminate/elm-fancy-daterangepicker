# elm-fancy-daterangepicker

```shell
elm install goilluminate/elm-fancy-daterangepicker
```

A reusable daterange picker in Elm.

## DateRangePicker

### Usage

#### Initializing
The `DateRangePicker.init` function initialises the DateRangePicker's state.

```elm
init : Model
init =
    { ...
        dateRangePicker = DateRangePicker.init
      ...
    }

```

#### View
The `DateRangePicker` can be displayed in the view using the `DateRangePicker.view` function.  It returns its own message type so you should wrap it in one of your own messages using `Html.map`.
You will need to have fetched the currentTime and localZone beforehand using `Time.now` and `Time.here`
```elm
type Msg
    = ...
    | DatePickerMsgs DateRangePicker.Msg
    | ...

view : Model -> Html Msg
view model =
    ...
    div []
        [ DateRangePicker.view 
            model.currentTime
            model.localZone
            model.dateRangePicker |> Html.map DatePickerMsgs
        ]
```

There is a helper method for opening the daterangepicker, which puts an onClick with the appropriate message onto the html element.
```elm
button [ DateRangePicker.open ] [ text "Open Picker" ]
``` 

#### Update
To handle `Msg` in your update function, you should unwrap the `DateRangePicker.Msg` and pass it down to the `DateRangePicker.update` function.  The `DateRangePicker.update` function returns:

* the new model
* any command


```elm
    DatePickerMsgs msg_ ->
        let
            ( newDateRangePicker, dateRangePickerCmd ) =
                DateRangePicker.update msg_ model.dateRangePicker
        in
        ( { model | dateRangePicker = newDateRangePicker }, Cmd.map DatePickerMsgs dateRangePickerCmd )
```

#### Subscriptions
You will need to hook up subscriptions for the daterangepicker to fully function. It will also need the current time and local zone.

```elm
subscriptions : Model -> Sub Msg
subscriptions model =
    let
        pickerSubscriptions =
            case ( model.currentTime, model.localZone ) of
                ( Just currentTime, Just localZone ) ->
                    DateRangePicker.subscriptions model.datePicker currentTime localZone

                _ ->
                    Sub.none
    in
    Sub.map DatePickerMsgs pickerSubscriptions
```

#### Settings

The DateRangePicker has several settings which can be changed as wanted. You can set these at init time with `initWithOptions`. If you don't want to change all the options you can combine with this `defaultConfig` like so:
 

```elm
init : Model
init =
    { ...
        dateRangePicker = DateRangePicker.initWithOptions
                              { defaultConfig
                                  | calendarType = TwoMonths
                                  , datePickerType = DatePicker
                              }
      ...
    }

```

There is also a concept of Presets which can allow a user to quickly select a date or dateRange which is commonly used. There are a couple premade presets that you can use as well.

The CustomPresets are set relative to the current date. All of them can be set at init time.

```elm
init : Model
init =
    { ...
        dateRangePicker = DateRangePicker.initWithOptions
                              { defaultConfig
                                  | presets =
                                      [ DateRangePicker.Today
                                      , DateRangePicker.Yesterday
                                      , DateRangePicker.Custom <|
                                          { intervalStart = DateRangePicker.Days
                                          , intervalStartValue = -3
                                          , intervalEnd = DateRangePicker.Days
                                          , intervalEndValue = -1
                                          , display = "Past Three Days"
                                          }
                                      , DateRangePicker.PastWeek
                                      , DateRangePicker.PastMonth
                                      ]
                              }
      ...
    }

``` 


## CSS

The CSS for the date pickers are distributed separately.  You can grab
the compiled CSS from [here][compiled] or you can grab the SCSS source
from [here][scss]. There is both a light and dark theme available. You will need to have a parent element which sets the appropriate color class.

```elm
div [Attributes.class "theme-light"] [DateRangePicker.view ...]
```
or
```elm
div [Attributes.class "theme-dark"] [DateRangePicker.view ...]
```

[compiled]: https://github.com/goilluminate/elm-fancy-daterangepicker/blob/master/css/daterangepicker.css
[scss]: https://github.com/goilluminate/elm-fancy-daterangepicker/blob/master/css/daterangepicker.scss

## `Map.!` compiler error

If you run into the compiler error [`Map.!: given key is not an element in the
map`](https://github.com/elm/compiler/issues/1851), try moving
"justinmimbs/date" into your direct dependencies. It _may_ fix the error.

## Development

### Prerequisites
- [Ruby][ruby-link]
- Rake `gem install rake`

[ruby-link]: https://www.ruby-lang.org/en/documentation/installation/

### Running the example
To run the example run `rake`.
This will:
    - install all node modules
    - install all elm packages
    - run the webpack dev server with hmr on `localhost:8080`

## Tests

To run the tests run `rake test`.
This will:
    - install all elm packages in the tests folder
    - run `elm-test`

**Note** you need to have `elm-test` installed globally
```shell
npm install elm-test -g
```

