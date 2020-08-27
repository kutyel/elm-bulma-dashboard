module Main exposing (main)

import Browser exposing (element)
import Bulma.CDN exposing (stylesheet)
import Bulma.Columns exposing (column, columnModifiers, columns, columnsModifiers)
import Bulma.Components exposing (card, cardContent, cardHeader, cardIcon, cardTitle, dropdown, dropdownItem, dropdownMenu, dropdownModifiers, dropdownTrigger, message, messageBody)
import Bulma.Elements exposing (TitleSize(..), button, buttonModifiers, subtitle)
import Bulma.Form exposing (controlCheckBox)
import Bulma.Layout exposing (SectionSpacing(..), container, section)
import Bulma.Modifiers exposing (Color(..), Size(..), shadowless)
import Bulma.Modifiers.Typography exposing (textLeft)
import Html exposing (Html, div, i, text)
import Html.Attributes exposing (attribute, class)
import Html.Events exposing (onCheck, onClick)



---- MODEL ----


type alias Model =
    { open : List Menu
    , selected : List ( Menu, Int )
    }


type Menu
    = Overall
    | Category1
    | Category2
    | Category3


init : ( Model, Cmd Msg )
init =
    ( Model [] [], Cmd.none )



---- UPDATE ----


type Msg
    = ToggleMenu Menu
    | UpdateChoice Menu Int Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ open, selected } as model) =
    case msg of
        ToggleMenu menu ->
            if List.member menu open then
                ( { model | open = List.filter ((/=) menu) open }, Cmd.none )

            else
                ( { model | open = menu :: open }, Cmd.none )

        UpdateChoice menu num checked ->
            if checked then
                ( { model | selected = ( menu, num ) :: selected }, Cmd.none )

            else
                ( { model | selected = List.filter ((/=) ( menu, num )) selected }, Cmd.none )



---- VIEW ----


dropTrigger : Menu -> Html Msg
dropTrigger menu =
    dropdownTrigger []
        [ button
            { buttonModifiers
                | color = Light
                , rounded = True
                , iconRight = Just ( Large, [], i [ class "fas fa-chevron-down" ] [] )
            }
            [ onClick <| ToggleMenu menu
            , attribute "aria-haspopup" "true"
            , attribute "aria-controls" "dropdown-menu"
            ]
            [ text "COMPARE"
            ]
        ]


checkBox : Menu -> Int -> Html Msg
checkBox menu num =
    controlCheckBox False
        []
        []
        [ onCheck <| UpdateChoice menu num ]
        [ text <| "Option " ++ String.fromInt num ]


drop : Menu -> Bool -> Html Msg
drop menu isMenuOpen =
    dropdown isMenuOpen
        dropdownModifiers
        []
        [ dropTrigger menu
        , dropdownMenu []
            []
            (List.range 1 10
                |> List.map (\check -> dropdownItem False [] [ checkBox menu check ])
            )
        ]


viewCard : String -> Bool -> Model -> Menu -> Html Msg
viewCard title shadow model menu =
    card
        (if shadow then
            []

         else
            [ shadowless ]
        )
        [ cardHeader []
            [ cardTitle [] [ text title ]
            , cardIcon []
                [ drop menu <| List.member menu model.open
                ]
            ]
        , cardContent []
            [ column columnModifiers
                [ textLeft ]
                [ case List.filter (\( m, _ ) -> m == menu) model.selected of
                    [] ->
                        text ""

                    xs ->
                        xs
                            |> List.reverse
                            |> List.map (\( _, n ) -> "Option " ++ String.fromInt n)
                            |> String.join ", "
                            |> String.append "Selected: "
                            |> text
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ stylesheet
        , section NotSpaced
            []
            [ container []
                [ columns columnsModifiers
                    []
                    [ column columnModifiers
                        []
                        [ message { color = White, size = Medium }
                            []
                            [ messageBody [ textLeft ]
                                [ viewCard "OVERALL" True model Overall
                                , section NotSpaced
                                    [ class "submenus" ]
                                    [ subtitle H5 [] [ text "BREAKDOWN" ]
                                    , text "Select the options from dropdown menu."
                                    , viewCard "CATEGORY 1" False model Category1
                                    , columns columnsModifiers
                                        []
                                        [ column columnModifiers [] [ viewCard "CATEGORY 2" False model Category2 ]
                                        , column columnModifiers [] [ viewCard "CATEGORY 3" False model Category3 ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    element
        { view = view
        , init = always init
        , update = update
        , subscriptions = always Sub.none
        }
