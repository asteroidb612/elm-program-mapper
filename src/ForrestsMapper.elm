module ForrestsMapper exposing (process)

import Elm.Parser
import Elm.Processing
import Elm.RawFile as RawFile
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Expression as Expression
import Elm.Syntax.Node as Node


process : RawFile.RawFile -> List String
process input =
    []
