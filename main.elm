module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update }


type Filter
    = All
    | Completed
    | Incomplete


type alias Todo =
    { title : String
    , completed : Bool
    }


type alias Model =
    { newTodoTitle : String
    , todoList : List Todo
    , currentFilter : Filter
    }


type Msg
    = TodoTitleChange String
    | TodoAdded Todo
    | TodoStatusChanged Todo
    | FilterChanged Filter


createTodo : String -> Todo
createTodo title =
    { title = title
    , completed = False
    }


filterTodos : List Todo -> Filter -> List Todo
filterTodos todoList filter =
    case filter of
        All ->
            todoList

        Completed ->
            (List.filter (\t -> t.completed) todoList)

        Incomplete ->
            (List.filter (\t -> not t.completed) todoList)


todoListItem : Todo -> Html Msg
todoListItem todo =
    let
        ( completed, incomplete ) =
            statusCheckboxes todo
    in
        li [] [ text todo.title, completed, incomplete ]


statusCheckboxes : Todo -> ( Html Msg, Html Msg )
statusCheckboxes todo =
    let
        complete =
            "Complete"

        incomplete =
            "Incomplete"

        -- isChecked isCompleted val =
        --     isCompleted == val
    in
        ( label []
            [ input [ type_ "checkbox", checked todo.completed, onClick (TodoStatusChanged todo) ] []
            , text complete
            ]
        , label
            []
            [ input [ type_ "checkbox", checked (not todo.completed), onClick (TodoStatusChanged todo) ] []
            , text incomplete
            ]
        )


radio : String -> Filter -> Html Msg
radio filterText filter =
    label []
        [ text filterText
        , input [ name "Filter", type_ "radio", checked ((\m f -> m.currentFilter == f) model filter), onClick (FilterChanged filter) ] []
        ]


todoPlaceHolderText : String
todoPlaceHolderText =
    "Title..."


model : Model
model =
    { newTodoTitle = ""
    , todoList = []
    , currentFilter = All
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        TodoTitleChange title ->
            { model
                | newTodoTitle = title
            }

        TodoAdded todo ->
            { model
                | todoList = todo :: model.todoList
                , newTodoTitle = ""
            }

        TodoStatusChanged todo ->
            (let
                updateStatus t =
                    if t == todo then
                        { todo | completed = not todo.completed }
                    else
                        t
             in
                { model | todoList = List.map updateStatus model.todoList }
            )

        FilterChanged filter ->
            { model
                | currentFilter = filter
            }


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Create a Todo" ]
        , label [] [ text "Todo Title: " ]
        , input [ placeholder todoPlaceHolderText, onInput TodoTitleChange, value model.newTodoTitle ] []
        , button [ onClick (TodoAdded (createTodo model.newTodoTitle)) ] [ text "Create Todo" ]
        , div []
            [ fieldset []
                [ label []
                    [ text "Filter Todos: " ]
                , radio "All" All
                , radio "Completed" Completed
                , radio "Incomplete" Incomplete
                ]
            ]
        , ul [] (filterTodos model.todoList model.currentFilter |> List.map todoListItem)
        ]
