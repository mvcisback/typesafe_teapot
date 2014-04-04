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
        int     { IntToken $$ }

%%

Link   : Exp Link                       { $1:$2 }
       | Exp                            { [$1] }

Exp     : vert float float float        { Vertex $2 $3 $4 }
        | vert float float int          { Vertex $2 $3 (fromIntegral $4) }
        | vert float int float          { Vertex $2 (fromIntegral $3) $4 }
        | vert int float float          { Vertex (fromIntegral $2) $3 $4 }
        | vert float int int            { Vertex $2 (fromIntegral $3) (fromIntegral $4) }
        | vert int float int            { Vertex (fromIntegral $2) $3 (fromIntegral $4) }    
        | vert int int float            { Vertex (fromIntegral $2) (fromIntegral $3) $4 }
        | vert int int int              { Vertex (fromIntegral $2) (fromIntegral $3) (fromIntegral $4) }
        | face int int int              { Face $2 $3 $4 }
        | geo id                        { Geo $2}

{
parseError :: [Token] -> a
parseError (h:h2:h3:h4:h5:h6:t) = error $ "Parse error " ++ (show (h, h2, h3, h4, h5, h6))
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
