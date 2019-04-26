-- ------ language="Haskell" file="src/Synthesis.hs"
module Synthesis where

import Data.Complex

import AST
import Array

-- type NoTwiddleCodelet = Function () 

planNoTwiddle :: RealFloat a => Function () b -> Array (Complex a) -> Array (Complex a) -> Expr ()
planNoTwiddle f inp out = TNull
-- ------ end
