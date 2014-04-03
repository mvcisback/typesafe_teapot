{
module Main where
import Tokens
}

%name obj
%tokentype { Token }
%error { parseError }

%token
        vert    { VertToken }
        face    { FaceToken }
        geo     { GeometricToken }
        id      { IdToken $$ }
        float   { FloatToken $$ }

%%

Link   : Exp Link                       { $1:$2 }
       | Exp                            { [$1] }

Exp     : vert float float float        { Vertex $2 $3 $4 }
        | face float float float        { Face $2 $3 $4 }
        | geo id                        { Geo $2}

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Link
     = Node Exp Link
     | Empty
     deriving (Eq, Show)

data Exp
     = Vertex Float Float Float
     | Face Float Float Float
     | Geo String
     deriving (Eq, Show)
}
