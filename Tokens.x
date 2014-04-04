{
module Tokens(alexScanTokens, Token(..)) where
}

%wrapper "basic"

@float = [0-9]+(.)?[0-9]+
$digit       = 0-9
$octit       = 0-7
$hexit       = [$digit A-F a-f]
$sep         = \/

@sign        = [\-\+]
@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+]? @decimal

@index       = @decimal
             | 0[oO] @octal
             | 0[xX] @hexadecimal

@number      = @index
             | @decimal \. @decimal @exponent?
             | @decimal @exponent


@id = [A-Za-z][A-Za-z0-9]*

$vert = v
$face = f

tokens :-

  $white+				;
  "#".*			        	;
  @index                                { \s -> IndexToken ((read s) - 1) }
  @sign? @number			{ \s -> FloatToken (read s) }
  v	                		{ \s -> VertToken }
  f                     		{ \s -> FaceToken }
  g                                     { \s -> GroupToken }
  o                                     { \s -> ObjectToken }
  s                                     { \s -> SmoothToken }
  vt                                    { \s -> TextureToken }
  vn                                    { \s -> NormalToken }
  vp                                    { \s -> ParameterToken }
  mtllib                                { \s -> MatlibToken }
  usemtl                                { \s -> UseMatToken }
  @id                                   { \s -> IdToken s }
  $sep                                  { \s -> SepToken }

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
        VertToken |
        FaceToken |
        GroupToken |
        IdToken String |
	FloatToken Float |
        IndexToken Int |
        NormalToken |
        TextureToken | 
        ParameterToken |
        ObjectToken |
        SmoothToken |
        MatlibToken |
        UseMatToken | 
        SepToken
	deriving (Eq,Show)
}
