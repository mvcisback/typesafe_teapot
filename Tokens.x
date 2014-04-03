{
module Tokens(alexScanTokens, Token(..)) where
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

@id = [A-Za-z][A-Za-z0-9]*

$vert = v
$face = f

tokens :-

  $white+				;
  "#".*			        	;
  @sign? @number			{ \s -> FloatToken (read s) }
  v	                		{ \s -> VertToken }
  f                     		{ \s -> FaceToken }
  g                                     { \s -> GeometricToken }
  @id                                   { \s -> IdToken s }

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
        VertToken |
        FaceToken |
        GeometricToken |
        IdToken String |
	FloatToken Float
	deriving (Eq,Show)
}
