{
module Parser(parse, Exp(..)) where
import Tokens
}

%name obj
%tokentype { Token }
%error { parseError }

%token
        vert    { VertToken }
        face    { FaceToken }
        geo     { GroupToken }
        id      { IdToken $$ }
        float   { FloatToken $$ }
        index   { IndexToken $$ }

%%

Link   : Exp Link                       { $1:$2 }
       | Exp                            { [$1] }

Exp     : vert float float float        { Vertex $2 $3 $4 }
        | face index index index        { Face $2 $3 $4 }
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
     | Face Int Int Int
     | Geo String
     deriving (Eq, Show)

parse = obj . alexScanTokens
}
