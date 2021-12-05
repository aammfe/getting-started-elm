module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Iso8601
import Task
import Time


type ActivityType
    = Walk
    | Run
    | Bike


type alias Activity =
    { date : Time.Posix
    , activityType : ActivityType
    , distance : Float
    , commnet : String
    }


activityTypeToString : ActivityType -> String
activityTypeToString activityType =
    case activityType of
        Walk ->
            "Walk"

        Run ->
            "Run"

        Bike ->
            "Bike"


stringToActivity : String -> ActivityType
stringToActivity activityType =
    case activityType of
        "Walk" ->
            Walk

        "Run" ->
            Walk

        "Bike" ->
            Walk

        _ ->
            Walk


type alias DraftActivity =
    { date : String
    , activityType : String
    , distance : String
    , comment : String
    }


type alias Model =
    { activities : List Activity
    , draft : DraftActivity
    , timeZone : Time.Zone
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            { activities =
                [ Activity (toPosix "2021-08-10") Walk 2.3 "Blarg"
                , Activity (toPosix "2021-08-09") Run 7.15 "Foo"
                , Activity (toPosix "2021-08-08") Bike 42.11 "Bar"
                ]
            , draft = DraftActivity "" "" "" ""
            , timeZone = Time.utc
            }
    in
    ( model, Task.perform AdjustTimeZone Time.here )


toPosix : String -> Time.Posix
toPosix date =
    case Iso8601.toTime date of
        Ok x ->
            x

        _ ->
            Time.millisToPosix 0



---- UPDATE ----


type Msg
    = AddActivity
    | ChangeDate String
    | ChangeActivityType String
    | ChangeDistance String
    | ChangeComment String
    | AdjustTimeZone Time.Zone


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        AddActivity ->
            addActivity model

        ChangeDate date ->
            changeDate model date

        ChangeActivityType type_ ->
            changeActivityType model type_

        ChangeDistance distance ->
            changeDistance model distance

        ChangeComment comment ->
            changeComment model comment

        AdjustTimeZone zone ->
            adjustTimeZone model zone


adjustTimeZone : Model -> Time.Zone -> ( Model, Cmd msg )
adjustTimeZone model timeZone =
    let
        updatedModel =
            { model | timeZone = timeZone }
    in
    ( updatedModel, Cmd.none )


changeComment : Model -> String -> ( Model, Cmd msg )
changeComment model commnet =
    let
        draft =
            model.draft

        newDraft =
            { draft | comment = commnet }
    in
    ( { model | draft = newDraft }, Cmd.none )


changeDistance : Model -> String -> ( Model, Cmd msg )
changeDistance model distance =
    let
        draft =
            model.draft

        newDraft =
            { draft | distance = distance }
    in
    ( { model | draft = newDraft }, Cmd.none )


changeActivityType : Model -> String -> ( Model, Cmd msg )
changeActivityType model type_ =
    let
        draft =
            model.draft

        newDraft =
            { draft | activityType = type_ }
    in
    ( { model | draft = newDraft }, Cmd.none )


changeDate : Model -> String -> ( Model, Cmd msg )
changeDate model date =
    let
        draft =
            model.draft

        newDraft =
            { draft | date = date }
    in
    ( { model | draft = newDraft }, Cmd.none )


addActivity : Model -> ( Model, Cmd msg )
addActivity model =
    let
        date =
            toPosix model.draft.date

        activityType =
            stringToActivity model.draft.activityType

        distance =
            String.toFloat model.draft.distance |> Maybe.withDefault 0

        commnet =
            model.draft.comment

        newActivity =
            Activity date activityType distance commnet

        updatedModel =
            { model
                | draft = DraftActivity "" "" "" ""
                , activities = newActivity :: model.activities
            }
    in
    ( updatedModel, Cmd.none )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    let
        body =
            [ h1 [] [ text "Activity Tracker" ]
            , viewActivities model
            , viewAddNewActivity model.draft
            ]
    in
    { title = "Activity Tracker", body = body }


viewAddNewActivity : DraftActivity -> Html Msg
viewAddNewActivity draft =
    fieldset []
        [ legend [] [ text "Add New Activity" ]
        , Html.form [ onSubmit AddActivity ]
            [ table []
                [ tr []
                    [ td [] [ label [] [ text "Date" ] ]
                    , td []
                        [ input
                            [ style "width" "100%"
                            , type_ "date"
                            , value draft.date
                            , onInput ChangeDate
                            ]
                            []
                        ]
                    ]
                , tr []
                    [ td [] [ label [] [ text "Type" ] ]
                    , td []
                        [ select
                            [ style "width" "100%"
                            , value draft.activityType
                            , onInput ChangeActivityType
                            ]
                            [ option [ value "Walk" ] [ text "Walk" ]
                            , option [ value "Run" ] [ text "Run" ]
                            , option [ value "Bike" ] [ text "Bike" ]
                            ]
                        ]
                    ]
                , tr []
                    [ td [] [ label [] [ text "Distance" ] ]
                    , td []
                        [ input
                            [ style "width" "100%"
                            , type_ "number"
                            , attribute "step" "0.1"
                            , value draft.distance
                            , onInput ChangeDistance
                            ]
                            []
                        ]
                    ]
                , tr []
                    [ td [] [ label [] [ text "Comment" ] ]
                    , td []
                        [ textarea
                            [ style "width" "100%"
                            , value draft.comment
                            , onInput ChangeComment
                            ]
                            []
                        ]
                    ]
                , button [ type_ "submit" ] [ text "Add" ]
                ]
            ]
        ]


viewActivities : Model -> Html Msg
viewActivities model =
    table [] <|
        tr []
            [ th [] [ text "Date" ]
            , th [] [ text "Type" ]
            , th [] [ text "Distance [mi]" ]
            , th [] [ text "Commnet" ]
            ]
            :: List.map (viewActivity model.timeZone) model.activities


viewActivity : Time.Zone -> Activity -> Html msg
viewActivity zone activity =
    tr []
        [ td [] [ text <| viewDate zone activity.date ]
        , td [] [ text <| activityTypeToString activity.activityType ]
        , td [] [ text <| String.fromFloat activity.distance ]
        , td [] [ text <| activity.commnet ]
        ]


viewDate : Time.Zone -> Time.Posix -> String
viewDate zone time =
    String.concat
        [ Time.toYear zone time |> String.fromInt
        , "/"
        , Time.toMonth zone time |> monthToString
        , "/"
        , Time.toDay zone time |> String.fromInt
        ]


monthToString : Time.Month -> String
monthToString month =
    case month of
        Time.Jan ->
            "01"

        Time.Feb ->
            "02"

        Time.Mar ->
            "03"

        Time.Apr ->
            "04"

        Time.May ->
            "05"

        Time.Jun ->
            "06"

        Time.Jul ->
            "07"

        Time.Aug ->
            "08"

        Time.Sep ->
            "09"

        Time.Oct ->
            "10"

        Time.Nov ->
            "11"

        Time.Dec ->
            "12"


main : Program () Model Msg
main =
    Browser.document
        { init = always init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
