{
module Parser(parse, Exp(..)) where
import Tokens
}

%name obj
%tokentype { Token }
%monad { E } { thenE } { returnE }
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
        | face int int int              { Face ($2 - 1) ($3 - 1) ($4 - 1) }
        | geo id                        { Geo $2 }

{

parseError tokens = failE "Parse error"

data Exp
     = Vertex Float Float Float
     | Face Int Int Int
     | Geo String
  deriving (Eq, Show, Ord)

     
parse = obj . alexScanTokens

data E a = Ok a | Failed String
  deriving (Eq, Show, Ord)

thenE :: E a -> (a -> E b) -> E b
m `thenE` k = 
   case m of 
       Ok a -> k a
       Failed e -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k = 
   case m of
      Ok a -> Ok a
      Failed e -> k e

}
