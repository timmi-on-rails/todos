module Backend exposing (Backend)

import Task
import Todo


type alias Backend =
    { getTodos : Task.Task String (List Todo.Todo)
    , addTodo : String -> Task.Task String (List Todo.Todo)
    }
