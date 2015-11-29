module Api.Todo.Rest where

import Api.Todo.Service (TodoService(..), pg)

type TodoServiceHandler base = Handler base TodoService ()

todoRoutes :: [Route b TodoService ()]
todoRoutes =
  [("/",method GET getTodos),("/",method POST createTodo)]
