module Main exposing (main)

import Browser exposing (sandbox)
import Bulma.CDN exposing (stylesheet)
import Bulma.Columns exposing (column, columnModifiers, columns, columnsModifiers)
import Bulma.Components exposing (card, cardContent, cardHeader, cardIcon, cardTitle, dropdown, dropdownItem, dropdownMenu, dropdownModifiers, dropdownTrigger, message, messageBody)
import Bulma.Elements exposing (TitleSize(..), button, buttonModifiers, subtitle)
import Bulma.Form exposing (controlCheckBox)
import Bulma.Layout exposing (SectionSpacing(..), container, section)
import Bulma.Modifiers exposing (Color(..), Size(..), shadowless)
import Bulma.Modifiers.Typography exposing (textLeft)
import Html exposing (Html, div, i, text)
import Html.Attributes exposing (attribute, class, id)
import Html.Events exposing (onClick, stopPropagationOn)
import Json.Decode as Json



---- MODEL ----


type alias Model =
    { open : Maybe Menu
    , selected : List ( Menu, Int )
    }


type Menu
    = Overall
    | Category1
    | Category2
    | Category3


init : Model
init =
    Model Nothing []



---- UPDATE ----


type Msg
    = NoOp
    | ToggleMenu Menu
    | UpdateChoice Menu Int
    | OnClickOutside


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        ToggleMenu menu ->
            case model.open of
                Nothing ->
                    { model | open = Just menu }

                Just m ->
                    if m == menu then
                        { model | open = Nothing }

                    else
                        { model | open = Just menu }

        UpdateChoice menu num ->
            if List.any (\( m, n ) -> m == menu && n == num) model.selected then
                { model | selected = List.filter ((/=) ( menu, num )) model.selected }

            else
                { model | selected = ( menu, num ) :: model.selected }

        OnClickOutside ->
            { model | open = Nothing }



---- VIEW ----


onClickAndStop : a -> Html.Attribute a
onClickAndStop msg =
    stopPropagationOn "click" <| Json.succeed ( msg, True )


dropTrigger : Menu -> Html Msg
dropTrigger menu =
    dropdownTrigger []
        [ button
            { buttonModifiers
                | color = Light
                , rounded = True
                , iconRight = Just ( Large, [], i [ class "fas fa-chevron-down" ] [] )
            }
            [ onClickAndStop <| ToggleMenu menu
            , attribute "aria-haspopup" "true"
            , attribute "aria-controls" "dropdown-menu"
            ]
            [ text "COMPARE" ]
        ]


drop : Menu -> Bool -> Html Msg
drop menu isMenuOpen =
    dropdown isMenuOpen
        dropdownModifiers
        [ id "dropdown" ]
        [ dropTrigger menu
        , dropdownMenu []
            []
            (List.range 1 10
                |> List.map
                    (\num ->
                        dropdownItem False
                            []
                            [ controlCheckBox False
                                []
                                [ onClickAndStop <| UpdateChoice menu num ]
                                [ onClickAndStop <| NoOp ]
                                [ text <| "Option " ++ String.fromInt num ]
                            ]
                    )
            )
        ]


viewCard : String -> Bool -> Model -> Menu -> Html Msg
viewCard title shadow { open, selected } menu =
    card
        (if shadow then
            []

         else
            [ shadowless ]
        )
        [ cardHeader []
            [ cardTitle [] [ text title ]
            , cardIcon []
                [ drop menu (open |> Maybe.map (\m -> m == menu) |> Maybe.withDefault False) ]
            ]
        , cardContent []
            [ column columnModifiers
                [ textLeft ]
                [ case List.filter (\( m, _ ) -> m == menu) selected of
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
    div (Maybe.withDefault [] <| Maybe.map (List.singleton << onClick << OnClickOutside) model.open)
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
    sandbox { view = view, init = init, update = update }
