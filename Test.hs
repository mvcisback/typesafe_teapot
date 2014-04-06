import Parser
import Obj
import Data.List
import Data.Set (fromList)
import Test.QuickCheck


instance Arbitrary Exp where
    arbitrary = oneof $ map return
                [Vertex 1 2 3
                ,Vertex 2 3 4
                ,Vertex 4 5 6
                ,Face 1 2 3]

prop_permutation xs = processObj xs `check` processObj (reverse xs)

check (vs, fs, os) (vs', fs', os') = all check' [(vs, vs'), (fs, fs'), (os, os')]
check' (as, as') = fromList as == fromList as'

