{-# LANGUAGE RankNTypes #-}

module Person
  ( Address (..),
    Born (..),
    Name (..),
    Person (..),
    bornStreet,
    renameStreets,
    setBirthMonth,
    setCurrentStreet,
  )
where

import Data.Functor.Const (Const (Const), getConst)
import Data.Functor.Identity (Identity (Identity), runIdentity)
import Data.Time.Calendar (Day, fromGregorian, toGregorian)

type Optic c s t a b = forall f. (c f) => (a -> f b) -> s -> f t

type Lens s t a b = Optic Functor s t a b

type Lens' s a = Lens s s a a

type Traversal s t a b = Optic Applicative t s a b

type Traversal' s a = Traversal s s a a

over :: Traversal' s a -> (a -> a) -> s -> s
over lens f = runIdentity . lens (Identity . f)

view :: Lens' s a -> s -> a
view lens = getConst . lens Const

data Person = Person
  { _name :: Name,
    _born :: Born,
    _address :: Address
  }

born :: Lens' Person Born
born f p = (\b -> p {_born = b}) <$> f (_born p)

address :: Lens' Person Address
address f p = (\a -> p {_address = a}) <$> f (_address p)

addresses :: Traversal' Person Address
addresses f (Person {_name = n, _born = b, _address = a}) =
  Person n
    <$> bornAt f b
    <*> f a

data Name = Name
  { _foreNames :: String,
    _surName :: String
  }

data Born = Born
  { _bornAt :: Address,
    _bornOn :: Day
  }

bornAt :: Lens' Born Address
bornAt f a = (\b -> a {_bornAt = b}) <$> f (_bornAt a)

bornOn :: Lens' Born Day
bornOn f a = (\b -> a {_bornOn = b}) <$> f (_bornOn a)

data Address = Address
  { _street :: String,
    _houseNumber :: Int,
    _place :: String,
    _country :: String
  }

street :: Lens' Address String
street f s = (\a -> s {_street = a}) <$> f (_street s)

bornStreet :: Born -> String
bornStreet = view (bornAt . street)

setCurrentStreet :: String -> Person -> Person
setCurrentStreet = over (address . street) . const

setBirthMonth :: Int -> Person -> Person
setBirthMonth month = over (born . bornOn) (\day -> let (y, _, d) = toGregorian day in fromGregorian y month d)

renameStreets :: (String -> String) -> Person -> Person
renameStreets = over (addresses . street)
