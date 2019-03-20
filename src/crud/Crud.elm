module Crud exposing (Model, Msg(..), initialCmd, initialModel, update, view)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as A exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D exposing (Decoder, bool, dict, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as E
import Maybe.Extra exposing (isNothing)
import Task exposing (Task)


type Msg
    = UserSelected UserJson
    | FilterTyped String
    | NewName String
    | NewSurname String
    | UserCreate User
    | UserUpdate (Maybe UserJson) User
    | UserDelete (Maybe UserJson)
    | GotUsers (Result Http.Error Users)
    | UserCreated (Result Http.Error UserJson)
    | UserUpdated (Result Http.Error UserJson)
    | UserDeleted (Result Http.Error Id)


type Status
    = Success
    | Error String


type alias Id =
    Int


type alias User =
    { name : String
    , surname : String
    }


type alias Users =
    Dict Id UserJson


type alias Model =
    { status : Status
    , users : Users
    , selectedUser : Maybe UserJson
    , newUser : User
    , filter : String
    }


isUserSelected : Model -> UserJson -> Bool
isUserSelected model u =
    case model.selectedUser of
        Nothing ->
            False

        Just x ->
            u.id == x.id


isButtonDisabled : Model -> Bool
isButtonDisabled model =
    isNothing model.selectedUser


isUpdateButtonDisabled : Model -> Bool
isUpdateButtonDisabled model =
    String.isEmpty model.newUser.name
        && String.isEmpty model.newUser.surname
        || isButtonDisabled model


filterUserList : String -> List UserJson -> List UserJson
filterUserList s =
    List.filter
        (\x -> String.startsWith (String.toLower s) (String.toLower x.surname))


makeUserName : String -> String -> String
makeUserName n s =
    if String.isEmpty n then
        s

    else if String.isEmpty s then
        n

    else
        [ n, s ] |> List.intersperse ", " |> String.concat


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
                        [ text <| makeUserName user.surname user.name ]
                )
        )
    ]


newValue : Maybe UserJson -> (UserJson -> String) -> String
newValue us f =
    us |> Maybe.map f |> Maybe.withDefault ""


view : Model -> Html Msg
view model =
    div [ class "crud-container" ]
        [ case model.status of
            Error err ->
                div [ class "error" ] [ text err ]

            Success ->
                text ""
        , div [ class "filter" ]
            [ span [ class "filter-text" ] [ text "Filter prefix:" ]
            , input [ class "", onInput FilterTyped ] []
            ]
        , div [ class "main-content" ]
            [ div [ class "list-container" ] <| userList model
            , div [ class "update-details" ]
                [ div [ class "new-name" ]
                    [ text "Name:"
                    , input
                        [ class "new-name--input"
                        , onInput NewName
                        ]
                        []
                    ]
                , div [ class "new-name" ]
                    [ text "Surname:"
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
                , disabled <| model.newUser.name == "" && model.newUser.surname == ""
                , onClick <|
                    UserCreate <|
                        User model.newUser.name model.newUser.surname
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
                , disabled <| isButtonDisabled model
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
        User "" ""
    , filter = ""
    }



-- TODO: refactor it


createUser : Users -> UserJson -> Users
createUser users user =
    users |> Dict.insert user.id user


updateUser : Users -> UserJson -> Users
updateUser users { id, name, surname } =
    -- TODO: refactor
    users |> Dict.update id (Maybe.map (\old -> { old | name = name, surname = surname }))


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
                    ( model, updateUserCmd u.id (User name surname) )

        UserDelete u ->
            case u of
                Nothing ->
                    ( model, Cmd.none )

                Just x ->
                    ( model, deleteUserCmd x.id )

        GotUsers resp ->
            handleResponse resp model (\x -> { model | users = x })

        UserCreated resp ->
            handleResponse resp model (\x -> { model | users = createUser model.users x })

        UserUpdated resp ->
            handleResponse resp model (\x -> { model | users = updateUser model.users x })

        UserDeleted resp ->
            handleResponse resp model (\x -> deleteUser model x)

        FilterTyped val ->
            ( { model | filter = val }, Cmd.none )


type alias UserJson =
    { id : Id
    , name : String
    , surname : String
    }


userDecoder : Decoder UserJson
userDecoder =
    succeed
        UserJson
        |> required "id" int
        |> required "name" string
        |> required "surname" string



-- TODO: refactor


usersDecoder : Decoder Users
usersDecoder =
    D.list userDecoder
        |> D.map
            (\x ->
                x
                    |> List.map (\{ id, name, surname } -> ( id, UserJson id name surname ))
                    |> Dict.fromList
            )


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "http://localhost:8000/api/users/list.json"
        , expect = Http.expectJson GotUsers usersDecoder
        }


jsonEncUser : User -> Http.Body
jsonEncUser { name, surname } =
    E.object
        [ ( "name", E.string name )
        , ( "surname", E.string surname )
        ]
        |> Http.jsonBody


createUserCmd : User -> Cmd Msg
createUserCmd user =
    Http.post
        { url = "http://localhost:8000/api/users/create.json"
        , body = jsonEncUser user
        , expect = Http.expectJson UserCreated userDecoder
        }


updateUserCmd : Id -> User -> Cmd Msg
updateUserCmd id user =
    Http.request
        { method = "PUT"
        , headers = []
        , url = "http://localhost:8000/api/users/" ++ String.fromInt id ++ "/update.json"
        , body = jsonEncUser user
        , expect = Http.expectJson UserUpdated userDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


expectString : (Result Http.Error String -> msg) -> Http.Expect msg
expectString toMsg =
    Http.expectStringResponse toMsg Ex.responseToString


deleteUserCmd : Id -> Cmd Msg
deleteUserCmd id =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "http://localhost:8000/api/users/" ++ String.fromInt id ++ "/delete.json"
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
