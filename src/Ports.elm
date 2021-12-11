port module Ports exposing (setStorage)

import Json.Encode as E

port setStorage : E.Value -> Cmd msg
