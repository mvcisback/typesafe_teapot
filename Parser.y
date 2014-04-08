{
  module Parser(parse, Exp(..), E(..)) where
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
        tex     { TextureToken }
        norm    { NormalToken }
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
        
        | norm float float float        { Normal $2 $3 $4 }
        | norm float float int          { Normal $2 $3 (fromIntegral $4) }
        | norm float int float          { Normal $2 (fromIntegral $3) $4 }
        | norm int float float          { Normal (fromIntegral $2) $3 $4 }
        | norm float int int            { Normal $2 (fromIntegral $3) (fromIntegral $4) }
        | norm int float int            { Normal (fromIntegral $2) $3 (fromIntegral $4) }    
        | norm int int float            { Normal (fromIntegral $2) (fromIntegral $3) $4 }
        | norm int int int              { Normal (fromIntegral $2) (fromIntegral $3) (fromIntegral $4) }

        | tex float float               { Texture $2 $3 }
        | tex float int                 { Texture $2 (fromIntegral $3) }
        | tex int int                   { Texture (fromIntegral $2) (fromIntegral $3) }

        | face int int int              { Face ($2 - 1) ($3 - 1) ($4 - 1) }
        | geo id                        { Geo $2 }

{

parseError tokens = failE "Parse error"

data Exp
     = Vertex Float Float Float
     | Face Int Int Int
     | Geo String
     | Normal Float Float Float
     | Texture Float Float
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
