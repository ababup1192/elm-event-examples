module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (checked, class, for, id, name, type_, value)
import Html.Events exposing (on, onCheck, onInput, targetValue)
import Json.Decode as Json


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type ContactType
    = Email
    | Phone
    | Mail


type alias Model =
    { inputedText : String
    , changedText : String
    , contactTypeMaybe : Maybe ContactType
    , consentValue : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { inputedText = ""
      , changedText = ""
      , contactTypeMaybe = Nothing
      , consentValue = False
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = InputText String
    | ChangeText String
    | ChangeContactType String
    | CheckConsent Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputText inputedText ->
            ( { model | inputedText = inputedText }, Cmd.none )

        ChangeText changedText ->
            ( { model | changedText = changedText }, Cmd.none )

        ChangeContactType contactTypeMaybe ->
            ( { model | contactTypeMaybe = value2ContactType contactTypeMaybe }, Cmd.none )

        CheckConsent consentValue ->
            ( { model | consentValue = consentValue }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ input [ type_ "text", value model.inputedText, onInput InputText ] []
            , p [] [ text model.inputedText ]
            ]
        , div []
            [ input [ type_ "text", value model.changedText, onChangeValue ChangeText ] []
            , p [] [ text model.changedText ]
            ]
        , div []
            [ div [ class "contact-group" ]
                [ input [ id "contactChoice1", name "contact", type_ "radio", value "email", onChangeValue ChangeContactType ]
                    []
                , label [ for "contactChoice1" ]
                    [ text "電子メール" ]
                , input [ id "contactChoice2", name "contact", type_ "radio", value "phone", onChangeValue ChangeContactType ]
                    []
                , label [ for "contactChoice2" ]
                    [ text "電話" ]
                , input [ id "contactChoice3", name "contact", type_ "radio", value "mail", onChangeValue ChangeContactType ]
                    []
                , label [ for "contactChoice3" ]
                    [ text "郵便" ]
                ]
            , p []
                [ text <|
                    case model.contactTypeMaybe of
                        Just contactType ->
                            contactType2Text contactType ++ "が選択されています。"

                        Nothing ->
                            "何も選択されていません。"
                ]
            ]
        , div []
            [ input [ type_ "checkbox", checked model.consentValue, onCheck CheckConsent ] []
            , p []
                [ text <|
                    if model.consentValue then
                        "同意ありがとうございます。"

                    else
                        "同意してください。"
                ]
            ]
        ]


onChangeValue : (String -> Msg) -> Attribute Msg
onChangeValue tagger =
    on "change" <| Json.map tagger targetValue


contactType2Text : ContactType -> String
contactType2Text contactType =
    case contactType of
        Email ->
            "電子メール"

        Phone ->
            "電話"

        Mail ->
            "郵便"


value2ContactType : String -> Maybe ContactType
value2ContactType contact =
    case contact of
        "email" ->
            Just Email

        "phone" ->
            Just Phone

        "mail" ->
            Just Mail

        _ ->
            Nothing
