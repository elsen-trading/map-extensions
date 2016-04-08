-- Utility infixes

module Data.Map.Function (
  (<$$>),
  (<$$$>),
  (<$$$$>),
  (<&>),
  (<&&>),
  (<&&&>),
  (<&&&&>),
) where

infixl 4 <$$> -- same binding as (Data.Functor.<$>)
infixl 4 <$$$>
infixl 4 <$$$$>

infixl 1 <&>  -- same binding as (Data.Function.&)
infixl 1 <&&>
infixl 1 <&&&>
infixl 1 <&&&&>

(<$$>) :: (Functor f, Functor g)
  => (a -> b)
  -> f (g a)
  -> f (g b)
(<$$>) = fmap . fmap

(<$$$>) :: (Functor f, Functor g, Functor h)
  => (a -> b)
  -> f (g (h a))
  -> f (g (h b))
(<$$$>) = fmap . fmap . fmap

(<$$$$>) :: (Functor f, Functor g, Functor h, Functor i)
  => (a -> b)
  -> f (g (h (i a)))
  -> f (g (h (i b)))
(<$$$$>) = fmap . fmap . fmap . fmap

(<&>) :: (Functor f)
  => f a
  -> (a -> b)
  -> f b
(<&>) = flip (<$>)

(<&&>) :: (Functor f, Functor g)
  => f (g a)
  -> (a -> b)
  -> f (g b)
(<&&>) = flip (<$$>)

(<&&&>) :: (Functor f, Functor g, Functor h)
  => f (g (h a))
  -> (a -> b)
  -> f (g (h b))
(<&&&>) = flip (<$$$>)

(<&&&&>) :: (Functor f, Functor g, Functor h, Functor i)
  => f (g (h (i a)))
  -> (a -> b)
  -> f (g (h (i b)))
(<&&&&>) = flip (<$$$$>)

