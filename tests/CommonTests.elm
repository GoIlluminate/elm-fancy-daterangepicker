module CommonTests
    exposing
        ( commonTestSuite
        )

import Test
    exposing
        ( describe
        , test
        , Test
        )
import Expect
    exposing
        ( equal
        , Expectation
        )
import DateRangePicker


commonTestSuite : Test
commonTestSuite =
    describe "Common Tests"
        [ describe "Test Tests"
            [ test "testing a test" <|
                \_ ->
                    "asdf" |> equal "asdf"
            ]
        ]
