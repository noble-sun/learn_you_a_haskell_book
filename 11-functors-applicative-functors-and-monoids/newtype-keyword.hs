
-- This is just wrapping a data type - in this case a list - and creating another 
-- type. It is also using the record syntax to generate a getZipList method to extract
-- the list from this type.
data ZipList a = ZipList { getZipList :: [a] }

-- Using the keyword 'newtype' is faster if the only thing you want is to wrap 
-- a existing type into another one.
newtype ZipList' a = ZipList' { getZipList' :: [a] }

-- Wrapping is a good way to adapt types to typeclasses signatures.
-- We're creating a wrapper for a tuple using 'newtype' where the first field is
-- the second element of the tuple, and the second field is the first element of 
-- the tuple.
newtype Pair b a = Pair { getPair :: (a,b) } deriving (Show)

-- We're creating a new instance of Functor for the Pair type. Since we can't just
-- pass 'Pair' because the type constructor can only take one parameter, and with
-- tuples we pass 2. So we partially apply the type by only passing one of the
-- parameters to the Functor.
-- In this case, the function will be applied to the first element of the tuple
-- fmap :: (a -> b) -> Pair c a -> Pair c b
instance Functor (Pair c) where
  fmap f (Pair (x,y)) = Pair (f x, y)

-- getPair $ fmap (*100) (Pair (2,3))
-- (200,3)

-- data CoolBool = CoolBool { getCoolBool :: Bool }
newtype CoolBool = CoolBool { getCoolBool :: Bool }

-- If CoolBool type was defined with 'data' keyword, and we tried to apply it to
-- 'undefined', it would throw an exception, but with 'newtype' it wouldn't. This
-- is because since 'data' can have more than one constructor and multiple fields, 
-- Haskell has to evaluate at least some part of see if the pattern match fits,
-- but 'newtype' has only one possible constructor and one field, so it doesn't 
-- even need to evaluate anything.
helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

