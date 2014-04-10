{
  module Tokens(alexScanTokens, Token(..)) where
}

%wrapper "basic"

@float = [0-9]+(.)?[0-9]+
$digit       = 0-9
$octit       = 0-7
$hexit       = [$digit A-F a-f]
$sep         = \/
$newline = [\n]

@sign        = [\-\+]
@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+]? @decimal

@int         = @decimal
             | 0[oO] @octal
             | 0[xX] @hexadecimal

@number      = @int
             | @decimal \. @decimal @exponent?
             | @decimal @exponent


@id = [A-Za-z][A-Za-z0-9]*
@file = @id \. @id

$vert = v
$face = f

tokens :-
  $white+				;
  "#".*			        	;
  @int                                  { \s -> IntToken (read s) }
  @sign? @number			{ \s -> FloatToken (read s) }
  v	                		{ \s -> VertToken }
  f                     		{ \s -> FaceToken }
  g                                     ;
  o                                     ;
  s                                     ;
  vt                                    { \s -> TextureToken }
  vn                                    { \s -> NormalToken }
  vp                                    { \s -> ParameterToken }
  mtllib                                ;
  usemtl                                ;
  @id                                   ;
  $sep                                  { \s -> SepToken }
  @file                                 ;

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
        VertToken |
        FaceToken |
        GroupToken |
        IdToken String |
	FloatToken Float |
        IntToken Int |
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
