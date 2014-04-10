{
module Parser(parse, Vertex(..), Face(..), Normal(..), Texture(..), E(..)) where
import Tokens
}

%name obj
%tokentype { Token }
%monad { E } { thenE } { returnE }
%error { parseError }

%token
        vert    { VertToken }
        face    { FaceToken }
        tex     { TextureToken }
        norm    { NormalToken }
        float   { FloatToken $$ }
        int     { IntToken $$ }
        sep     { SepToken }

%%

Start  : Vert Tex Norm Face             { ($1, $2, $3, $4) }
       | Vert Norm Face                 { ($1, [], $2, $3) }
       | Vert Tex Face                  { ($1, $2, [], $3) }
       | Vert Face                      { ($1, [], [], $2) }

Face   : Face1 Face                     { $1:$2 }
       | Face1                          { [$1] }
Vert   : Vert1 Vert                     { $1:$2 }
       | Vert1                          { [$1] }
Tex    : Tex1 Tex                       { $1:$2 }
       | Tex1                           { [$1] }
Norm   : Norm1 Norm                     { $1:$2 }
       | Norm1                          { [$1] }

Num     : float                         { $1 }
        | int                           { fromIntegral $1 }

Vert1   : vert Num Num Num              { Vertex $2 $3 $4 }
Norm1   : norm Num Num Num              { Normal $2 $3 $4 }
Tex1    : tex Num Num                   { Texture $2 $3 }
Face1   : face Point Point Point        { Face $2 $3 $4 }
Point   : Index sep Index sep Index     { ($1, Just $3, Just $5) }
        | Index sep sep Index           { ($1, Nothing, Just $4) }
        | Index                         { ($1, Nothing, Nothing)}
Index   : int                           { $1 - 1 }

{

parseError tokens = failE $ "Parse error" ++ (show tokens)

data Vertex = Vertex Float Float Float
  deriving (Eq, Show, Ord)
data Face = Face (Int, Maybe Int, Maybe Int) (Int, Maybe Int, Maybe Int) (Int, Maybe Int, Maybe Int)
  deriving (Eq, Show, Ord)
data Normal = Normal Float Float Float
  deriving (Eq, Show, Ord)
data Texture = Texture Float Float
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
