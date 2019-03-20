module Crud exposing (Model, Msg(..), initialCmd, initialModel, update, view)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Http.Extras as Ex
import Json.Decode as D exposing (Decoder, bool, dict, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as E
import Maybe.Extra exposing (isJust)
import Task exposing (Task)


apiUrl : String
apiUrl =
    "http://localhost:8000/api/users/"


type Msg
    = UserSelected User
    | FilterTyped String
    | NewName String
    | NewSurname String
    | UserCreate NewUser
    | UserUpdate (Maybe User) NewUser
    | UserDelete (Maybe User)
    | GotUsers (Result Http.Error Users)
    | UserCreated (Result Http.Error User)
    | UserUpdated (Result Http.Error User)
    | UserDeleted (Result Http.Error Id)


type Status
    = Success
    | Error String


type alias Id =
    Int


type alias NewUser =
    { name : String
    , surname : String
    }


type alias User =
    { id : Id
    , name : String
    , surname : String
    }


type alias Users =
    Dict Id User


type alias Model =
    { status : Status
    , users : Users
    , selectedUser : Maybe User
    , newUser : NewUser
    , filter : String
    }


isUserSelected : Model -> User -> Bool
isUserSelected model u =
    case model.selectedUser of
        Nothing ->
            False

        Just x ->
            u.id == x.id


isNewUserEntered : Model -> Bool
isNewUserEntered model =
    not <|
        String.isEmpty model.newUser.name
            || String.isEmpty model.newUser.surname


isListItemSelected : Model -> Bool
isListItemSelected model =
    isJust model.selectedUser


isCreateButtonDisabled : Model -> Bool
isCreateButtonDisabled model =
    not
        (isNewUserEntered model)


isUpdateButtonDisabled : Model -> Bool
isUpdateButtonDisabled model =
    not <|
        isNewUserEntered
            model
            && isListItemSelected model


filterUserList : String -> List User -> List User
filterUserList s =
    List.filter
        (\x -> String.startsWith (String.toLower s) (String.toLower x.surname))


userList : Model -> List (Html Msg)
userList model =
    [ ul [ class "current-list" ]
        (model.users
            |> Dict.values
            |> filterUserList model.filter
            |> List.map
                (\user ->
                    li
                        [ class <|
                            if isUserSelected model user then
                                "user-item active"

                            else
                                "user-item"
                        , onClick <| UserSelected user
                        ]
                        [ text <| user.surname ++ ", " ++ user.name ]
                )
        )
    ]


view : Model -> Html Msg
view model =
    div [ class "crud-container" ]
        [ case model.status of
            Error err ->
                div [ class "error" ] [ text err ]

            Success ->
                text ""
        , div [ class "crud-content" ]
            [ div [ class "left-view" ]
                [ div [ class "crud-field" ]
                    [ span [ class "crud-label" ] [ text "Filter prefix:" ]
                    , input [ class "filter-input", onInput FilterTyped ] []
                    ]
                , div [ class "main-content" ]
                    [ div [ class "list-container" ] <| userList model ]
                ]
            , div [ class "update-details" ]
                [ div [ class "crud-field" ]
                    [ span [ class "crud-label" ] [ text "Name:" ]
                    , input
                        [ class "new-name--input"
                        , onInput NewName
                        ]
                        []
                    ]
                , div [ class "crud-field" ]
                    [ span [ class "crud-label" ] [ text "Surname:" ]
                    , input
                        [ class "new-name--input"
                        , onInput NewSurname
                        ]
                        []
                    ]
                ]
            ]
        , div [ class "" ]
            [ button
                [ class "crud-btn"
                , disabled <| isCreateButtonDisabled model
                , onClick <|
                    UserCreate <|
                        NewUser model.newUser.name model.newUser.surname
                ]
                [ text "Create" ]
            , button
                [ class "crud-btn"
                , disabled <| isUpdateButtonDisabled model
                , onClick <| UserUpdate model.selectedUser model.newUser
                ]
                [ text "Update" ]
            , button
                [ class "crud-btn crud-btn--delete"
                , disabled <| not <| isListItemSelected model
                , onClick <| UserDelete model.selectedUser
                ]
                [ text "Delete" ]
            ]
        ]


initialModel : Model
initialModel =
    { status = Success
    , users =
        Dict.empty
    , selectedUser = Nothing
    , newUser =
        NewUser "" ""
    , filter = ""
    }



-- TODO: refactor it


createUser : User -> Users -> Users
createUser user =
    Dict.insert user.id user


updateUser : User -> Users -> Users
updateUser { id, name, surname } =
    Dict.update id (Maybe.map (\old -> { old | name = name, surname = surname }))


deleteUser : Model -> Id -> Model
deleteUser model id =
    { model | users = Dict.remove id model.users, selectedUser = Nothing }


handleError : Http.Error -> String
handleError err =
    case err of
        Http.BadUrl url ->
            "bad url " ++ url

        Http.Timeout ->
            "timeout"

        Http.NetworkError ->
            "network error"

        Http.BadStatus st ->
            "bad status " ++ String.fromInt st

        Http.BadBody e ->
            "bad body " ++ e


handleResponse : Result Http.Error a -> Model -> (a -> Model) -> ( Model, Cmd Msg )
handleResponse resp model updateModel =
    case resp of
        Ok res ->
            ( updateModel res, Cmd.none )

        Err e ->
            ( { model | status = Error ("Sorry, an error occured: " ++ handleError e) }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserSelected k ->
            ( { model
                | selectedUser =
                    if isUserSelected model k then
                        Nothing

                    else
                        Just k
              }
            , Cmd.none
            )

        NewName n ->
            let
                { newUser } =
                    model
            in
            ( { model | newUser = { newUser | name = n } }, Cmd.none )

        NewSurname s ->
            let
                { newUser } =
                    model
            in
            ( { model | newUser = { newUser | surname = s } }, Cmd.none )

        UserCreate u ->
            ( model, createUserCmd u )

        UserUpdate currentUser { name, surname } ->
            case currentUser of
                Nothing ->
                    ( model, Cmd.none )

                Just u ->
                    ( model, updateUserCmd u.id (NewUser name surname) )

        UserDelete u ->
            case u of
                Nothing ->
                    ( model, Cmd.none )

                Just x ->
                    ( model, deleteUserCmd x.id )

        GotUsers resp ->
            handleResponse resp model (\x -> { model | users = x })

        UserCreated resp ->
            handleResponse resp model (\x -> { model | users = createUser x model.users })

        UserUpdated resp ->
            handleResponse resp model (\x -> { model | users = updateUser x model.users })

        UserDeleted resp ->
            handleResponse resp model (\x -> deleteUser model x)

        FilterTyped val ->
            ( { model | filter = val }, Cmd.none )


userDecoder : Decoder User
userDecoder =
    succeed
        User
        |> required "id" int
        |> required "name" string
        |> required "surname" string


usersDecoder : Decoder Users
usersDecoder =
    D.list userDecoder
        |> D.map
            (\x ->
                x
                    |> List.map (\{ id, name, surname } -> ( id, User id name surname ))
                    |> Dict.fromList
            )


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = apiUrl ++ "list.json"
        , expect = Http.expectJson GotUsers usersDecoder
        }


jsonEncUser : NewUser -> Http.Body
jsonEncUser { name, surname } =
    E.object
        [ ( "name", E.string name )
        , ( "surname", E.string surname )
        ]
        |> Http.jsonBody


createUserCmd : NewUser -> Cmd Msg
createUserCmd user =
    Http.post
        { url = apiUrl ++ "create.json"
        , body = jsonEncUser user
        , expect = Http.expectJson UserCreated userDecoder
        }


updateUserCmd : Id -> NewUser -> Cmd Msg
updateUserCmd id user =
    Http.request
        { method = "PUT"
        , headers = []
        , url = apiUrl ++ String.fromInt id ++ "/update.json"
        , body = jsonEncUser user
        , expect = Http.expectJson UserUpdated userDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


deleteUserCmd : Id -> Cmd Msg
deleteUserCmd id =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = apiUrl ++ String.fromInt id ++ "/delete.json"
        , body = Http.emptyBody
        , expect =
            expectJson
                (\r ->
                    case r of
                        Ok _ ->
                            UserDeleted (Ok id)

                        Err e ->
                            UserDeleted (Err e)
                )
        , timeout = Nothing
        , tracker = Nothing
        }


expectJson : (Result Http.Error String -> msg) -> Http.Expect msg
expectJson toMsg =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ metadata body ->
                    Ok body


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
