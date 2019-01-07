# elm-fancy-daterangepicker

```shell
elm install goilluminate/elm-fancy-daterangepicker
```

A reusable daterange picker component in Elm.

## Example
[Ellie App Example](https://ellie-app.com/3QPC6FK7DGsa1)

## DateRangePicker

### Usage

The `DateRangePicker.init` function initialises the DateRangePicker.  It returns the initialised DateRangePicker and associated `Cmds` so it must be done in your program's `init` or `update` functions:

**Note** Make sure you don't throw away the initial `Cmd`!

```elm
init : (Model, Cmd Msg)
...
    let
        (dateRangePicker, dateRangePickerCmd) =
            DateRangePicker.init
    in
        { model
            | dateRangePicker = dateRangePicker
            , Cmd.map SetDateRangePicker dateRangePickerCmd
        }

```
The `DateRangePicker` can be displayed in the view using the `DateRangePicker.view` function.  It returns its own message type so you should wrap it in one of your own messages using `Html.map`:
```elm
type Msg
    = ...
    | SetDateRangePicker DateRangePicker.Msg
    | ...

view : Model -> Html Msg
view model =
    ...
    div []
        [ DateRangePicker.view model.dateRangePicker |> Html.map SetDateRangePicker
        ]
```

To handle `Msg` in your update function, you should unwrap the `DateRangePicker.Msg` and pass it down to the `DateRangePicker.update` function.  The `DateRangePicker.update` function returns:

* the new model
* any command

To create the settings to pass to `setSettings` when you initialise the DateRangePicker, `DateRangePicker.defaultSettings` is provided to make it easier to use.  You only have to override the settings that you are interested in.

```elm
someSettings : DateRangePicker.Settings
someSettings =
    ...
    { defaultSettings
        | formatDateRange = newFormatDateRange
        , placeholder = "No daterange is selected."
    }

init : (Model, Cmd Msg)
...
    let
        (dateRangePicker_, dateRangePickerCmd) =
            DateRangePicker.init
        
        dateRangePicker =
            dateRangePicker_
                |> setSettings someSettings
    in
        { model
            | dateRangePicker = dateRangePicker
            , Cmd.map SetDateRangePicker dateRangePickerCmd
        }
```

The DateRangePicker has `Presets` which are preset date ranges that can be selected.  You can configure these yourself or use the `DateRangePicker.defaultPresetOptions`.

To create these presets yourself, you can create them from a `DateRangePicker.PresetOptions`.

```elm
somePresetSettings : List DateRangePicker.PresetSetting
somePresetSettings =
    let
        pastWeek =
            { name = "Past Week"
            , interval = Days
            , presetRelativeToday = ToToday
            , value = 7
            }

        pastMonth =
            { name = "Past Month"
            , interval = Months
            , presetRelativeToday = ToToday
            , value = 1
            }
    in
        [ pastWeek
        , pastMonth
        ]

somePresets : List DateRangePicker.Preset
somePresets =
    [ mkPresetFromDates "January 1, 2018 to February 1, 2018" (fromCalendarDate 2018 Jan 1) (fromCalendarDate 2018 Feb 1)
    , mkPresetFromDates "March 1, 2018 to April 1, 2018" (fromCalendarDate 2018 Mar 1) (fromCalendarDate 2018 Apr 1)
    ]

somePresetOptions : DateRangePicker.PresetOptions
somePresetOptions =
    { presetOption = CustomOnly
    , presetSettings = somePresetSettings
    , presets = somePresets
    }

someSettings : DateRangePicker.Settings
someSettings =
    ...
    { defaultSettings
        | presetOptions = somePresetOptions
    }

init : (Model, Cmd Msg)
...
    let
        (dateRangePicker_, dateRangePickerCmd) =
            DateRangePicker.init
        
        dateRangePicker =
            dateRangePicker_
                |> setSettings someSettings
    in
        { model
            | dateRangePicker = dateRangePicker
            , Cmd.map SetDateRangePicker dateRangePickerCmd
        }
```

You can create your settings and then use `DateRangePicker.setSettings` like we have been doing in the previous examples, or you can chain setters in your `init` function like so:

```elm
init : (Model, Cmd Msg)
...
    let
        (dateRangePicker_, dateRangePickerCmd) =
            DateRangePicker.init
        
        dateRangePicker =
            dateRangePicker_
                |> setSettings defaultSettings
                |> setInputId "myDateRangePicker"
                |> setPresetOptions somePresetOptions
                |> setPlaceholder "No date selected"
                |> setDateRange ( Just (mkDateRange startDate endDate) )
    in
        { model
            | dateRangePicker = dateRangePicker
            , Cmd.map SetDateRangePicker dateRangePickerCmd
        }
```

## DatePicker

### Usage

The `DatePicker.init` function initialises the DatePicker.  It returns the initialised DatePicker and associated `Cmds` so it must be done in your program's `init` or `update` functions:

**Note** Make sure you don't throw away the initial `Cmd`!

```elm
init : (Model, Cmd Msg)
...
    let
        (datePicker, datePickerCmd) =
            DatePicker.init
    in
        { model
            | datePicker = datePicker
            , Cmd.map SetDatePicker datePickerCmd
        }
```

The `DatePicker` can be displayed in the view using the `DatePicker.view` function.  It returns its own message type so you should wrap it in one of your own messages using `Html.map`:

```elm
type Msg
    = ...
    | SetDatePicker DatePicker.Msg
    | ...

view : Model -> Html Msg
view model =
    ...
    div []
        [ DatePicker.view model.datePicker |> Html.map SetDatePicker
        ]
```

To handle `Msg` in your update function, you should unwrap the `DatePicker.Msg` and pass it down to the `DatePicker.update` function.  The `DatePicker.update` function returns:

* the new model
* any command

To create the settings to pass to `setSettings` when you initialise the DatePicker, `DatePicker.defaultSettings` is provided to make it easier to use.  You only have to override the settings that you are interested in.

```elm
someSettings : DatePicker.Settings
someSettings =
    ...
    { defaultSettings
        | formatDate = newFormatDate
        , placeholder = "No date range is selected."
    }

init : (Model, Cmd Msg)
...
    let
        (datePicker_, datePickerCmd) =
            DatePicker.init
        
        datePicker =
            datePicker_
                |> setSettings someSettings
    in
        { model
            | datePicker = datePicker
            , Cmd.map SetDatePicker datePickerCmd
        }
```

The DatePicker has `Presets` which are preset dates that can be selected.  You can configure these yourself or use the `DatePicker.defaultPresetOptions`.

To create these presets yourself, you can create them from a `DatePicker.PresetOptions`.

```elm
somePresetSettings : List DatePicker.PresetSetting
somePresetSettings =
    let
        yesterday =
            { name = "Yesterday"
            , interval = Days
            , presetRelativeToday = Yesterday
            , value = 1
            }

        theDayAfterTomorrow =
            { name = "The day after tomorrow"
            , interval = Days
            , presetRelativeToday = AfterToday
            , value = 2
            }
    in
        [ yesterday
        , theDayAfterTomorrow
        ]

somePresets : List DatePicker.Preset
somePresets =
    [ mkPresetFromDate "January 1, 2018" (fromCalendarDate 2018 Jan 1)
    , mkPresetFromDate "March 1, 2018" (fromCalendarDate 2018 Mar 1)
    ]

somePresetOptions : DatePicker.PresetOptions
somePresetOptions =
    { presetOption = CustomOnly
    , presetSettings = somePresetSettings
    , presets = somePresets
    }

someSettings : DatePicker.Settings
someSettings =
    ...
    { defaultSettings
        | presetOptions = somePresetOptions
    }

init : (Model, Cmd Msg)
...
    let
        (datePicker_, datePickerCmd) =
            DatePicker.init
        
        datePicker =
            datePicker_
                |> setSettings someSettings
    in
        { model
            | datePicker = datePicker
            , Cmd.map SetDatePicker datePickerCmd
        }
```

You can create your settings and then use `DatePicker.setSettings` like we have been doing in the previous examples, or you can chain setters in your `init` function like so:

```elm
init : (Model, Cmd Msg)
...
    let
        (datePicker_, datePickerCmd) =
            DatePicker.init
        
        datePicker =
            datePicker_
                |> setSettings defaultSettings
                |> setInputId "myDatePicker"
                |> setPresetOptions somePresetOptions
                |> setPlaceholder "No date selected"
                |> setDate (Just (fromCalendarDate someDate) )
    in
        { model
            | datePicker = datePicker
            , Cmd.map SetDatePicker datePicker
        }
```

## CSS

The CSS for the date pickers are distributed separately.  You can grab
the compiled CSS from [here][compiled] or you can grab the SCSS source
from [here][scss].

[compiled]: https://github.com/goilluminate/elm-fancy-daterangepicker/blob/master/css/daterangepicker.css
[scss]: https://github.com/goilluminate/elm-fancy-daterangepicker/blob/master/css/daterangepicker.scss

## Example

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

## References

When this package was first created, the [elm-community/elm-datepicker][elm-community-datepicker] package was referenced and some of the code/ideas were used.

[elm-community-datepicker]: https://github.com/elm-community/elm-datepicker