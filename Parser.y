{
module Parser(parse, Obj(..), Vertex(..), Face(..), Normal(..), Texture(..), E(..)) where
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

Start  : Vert Tex Norm Face2            { Processed $1 $2 $3 $4 }
       | Vert Face                      { UnProcessed $1 $2}

Face2  : Face3 Face2                    { $1:$2 }
       | Face3                          { [$1] }
Vert   : Vert1 Vert                     { $1:$2 }
       | Vert1                          { [$1] }
Face   : Face1 Face                     { $1:$2 }
       | Face1                          { [$1] }
Tex    : Tex1 Tex                       { $1:$2 }
       | Tex1                           { [$1] }
Norm   : Norm1 Norm                     { $1:$2 }
       | Norm1                          { [$1] }

Num     : float                         { $1 }
        | int                           { fromIntegral $1 }

Vert1   : vert Num Num Num              { Vertex $2 $3 $4 }
Norm1   : norm Num Num Num              { Normal $2 $3 $4 }
Tex1    : tex Num Num                   { Texture $2 $3 }
Face1   : face int int int              { Face ($2 - 1) ($3 - 1) ($4 - 1) }
Face3   : face Point Point Point        { Face2 $2 $3 $4 }
Point   : int sep int sep int           { ($1, $3, $5) }

{

parseError tokens = failE $ "Parse error" ++ (show tokens)

data Obj = UnProcessed [Vertex] [Face] 
         | Processed [Vertex] [Texture] [Normal] [Face2]
  deriving (Eq, Show, Ord)

data Vertex = Vertex Float Float Float
  deriving (Eq, Show, Ord)
data Face = Face Int Int Int
  deriving (Eq, Show, Ord)
data Face2 = Face2 (Int, Int, Int) (Int, Int, Int) (Int, Int, Int)
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
