{
module Main (main) where
}

%wrapper "basic"

@float = [0-9]+(.)?[0-9]+
$digit       = 0-9
$octit       = 0-7
$hexit       = [$digit A-F a-f]

@sign        = [\-\+]
@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+]? @decimal

@number      = @decimal
             | @decimal \. @decimal @exponent?
             | @decimal @exponent
             | 0[oO] @octal
             | 0[xX] @hexadecimal
$vert = v
$face = f

tokens :-

  $white+				;
  "#".*			        	;
  @sign? @number			{ \s -> Float (read s) }
  v	                		{ \s -> Vert }
  f                     		{ \s -> Face }

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
        Vert |
        Face |
	Float Float
	deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
