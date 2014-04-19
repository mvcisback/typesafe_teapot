# Typesafe_teapot #

# About #
see: [http://mvcisback.github.io/posts/2014-04-15-teapot.html](http://mvcisback.github.io/posts/2014-04-15-teapot.html) 

# Build Instructions #
1. install alex and happy
2. Generate Lexer
  $ alex Tokens.x
3. Generate Parser
  $ happy Parser.y
4. Install Dependencies
  - $ cabal configure
  - $ cabal install
  - $ cabal build
  - $ cat objs/teapot_0.obj| typesafe_teapot -t texs/metal.jpg -e envs/grace_probe.jpg -b bumps/noBump.png
  - Note, I needed to grab GPipe 1.4 from github (since its not on hackage) to get this to build
5. Test



# Relevent Files #
- Main.hs: option parser
- Render.hs: renderer
- Objs.hs: Obj -> Triangle Stream
- Parser.y: Grammar for Obj
- Tokens.x: Tokens for Parser

# Features: #
- perspective
- hidden surfaces (via the z-buffer),
- shading using the Phong surface reflection model
- texture mapping
- environment mapping.
- Seeliger lighting model
- Bump mapping
- general obj parsing
