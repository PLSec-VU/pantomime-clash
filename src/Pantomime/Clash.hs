{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}

-- | Exports 'PluginAxioms' that may be provided to 'Pantomime'.
module Pantomime.Clash
  -- | The primary export of this package.
  ( axioms

  -- | The bitvector primitive used for the axioms.
  --
  -- This one allows for zero-sized bitvectors, unlike the primitive pantomime
  -- version.
  , BitVec (..)
  , withSize
  , nullary
  , unary
  , binary
  ) where

import Data.Bits (Bits (..))
import Data.Composition ((.:))
import Data.Constraint (Dict (..))
import Data.Constraint.Unsafe (unsafeAxiom)
import Data.Data (Proxy (..))
import Data.Functor.Identity (Identity (..))
import Clash.Prelude (SNat (..))
import Clash.Sized.Internal.BitVector (Bit, BitVector)
import Clash.Sized.Internal.BitVector qualified as Bit
  ( eq##
  , neq##
  , msb#
  , high
  , low
  )
import Clash.Sized.Internal.BitVector qualified as BitVector
import Clash.Sized.Internal.Unsigned (Unsigned)
import Clash.Sized.Internal.Unsigned qualified as Unsigned
import Clash.Sized.Internal.Signed (Signed)
import Clash.Sized.Internal.Signed qualified as Signed
import GHC.Exts (IsList (..), Coercible, coerce, Int#, Int (..))
import GHC.TypeNats
  ( KnownNat
  , Natural
  , type (<=)
  , type (+)
  , type (-)
  )
import GHC.TypeLits qualified as TypeLits (natVal)
import Pantomime (PluginAxioms (..))
import Pantomime.BuiltIn qualified as Pantomime
import Pantomime.Dict (unsafeEq)

axioms :: PluginAxioms
axioms = PluginAxioms
  { typeAxioms = fromList
    [ (''Signed, ''BitVec)
    , (''Unsigned, ''BitVec)
    , (''BitVector, ''BitVec)
    , (''Bit, ''BitVec1)
    ]
  , termAxioms =
    [ ('(Signed.+#), 'addA)
    , ('(Signed.-#), 'subA)
    , ('(Signed.*#), 'mulA)
    , ('Signed.negate#, 'negateA)
    , ('Signed.complement#, 'complementA)
    , ('Signed.and#, 'andA)
    , ('Signed.or#, 'orA)
    , ('Signed.xor#, 'xorA)
    , ('Signed.abs#, 'absA)
    , ('Signed.eq#, 'eqA)
    , ('Signed.neq#, 'neqA)
    -- FIXME: These comparisons are for unsigned bitvectors.
    , ('Signed.lt#, 'ltA)
    , ('Signed.le#, 'leA)
    , ('Signed.gt#, 'gtA)
    , ('Signed.ge#, 'geA)
    , ('Signed.shiftL#, 'shlA)
    , ('Signed.shiftR#, 'ashrA)
    , ('Signed.fromInteger#, 'fromIntegerA)
    , ('Signed.unpack#, 'bvcoerceA)
    , ('Signed.pack#, 'bvcoerceA)
    , ('Signed.size#, 'sizeA)
    , ('Signed.resize#, 'sresizeA)
    -- FIXME: This enum from is for unsigned bitvectors.
    , ('Signed.enumFrom#, 'enumFromU)

    , ('(Unsigned.+#), 'addA)
    , ('(Unsigned.-#), 'subA)
    , ('(Unsigned.*#), 'mulA)
    , ('Unsigned.negate#, 'negateA)
    , ('Unsigned.complement#, 'complementA)
    , ('Unsigned.and#, 'andA)
    , ('Unsigned.or#, 'orA)
    , ('Unsigned.xor#, 'xorA)
    , ('Unsigned.eq#, 'eqA)
    , ('Unsigned.neq#, 'neqA)
    , ('Unsigned.lt#, 'ltA)
    , ('Unsigned.le#, 'leA)
    , ('Unsigned.gt#, 'gtA)
    , ('Unsigned.ge#, 'geA)
    , ('Unsigned.shiftL#, 'shlA)
    , ('Unsigned.shiftR#, 'lshrA)
    , ('Unsigned.fromInteger#, 'fromIntegerA)
    , ('Unsigned.unpack#, 'bvcoerceA)
    , ('Unsigned.pack#, 'bvcoerceA)
    , ('Unsigned.size#, 'sizeA)
    , ('Unsigned.resize#, 'zresizeA)
    , ('Unsigned.enumFrom#, 'enumFromU)

    , ('(BitVector.+#), 'addA)
    , ('(BitVector.-#), 'subA)
    , ('(BitVector.*#), 'mulA)
    , ('BitVector.negate#, 'negateA)
    , ('BitVector.complement#, 'complementA)
    , ('BitVector.and#, 'andA)
    , ('BitVector.or#, 'orA)
    , ('BitVector.xor#, 'xorA)
    , ('BitVector.eq#, 'eqA)
    , ('BitVector.neq#, 'neqA)
    , ('BitVector.lt#, 'ltA)
    , ('BitVector.le#, 'leA)
    , ('BitVector.gt#, 'gtA)
    , ('BitVector.ge#, 'geA)
    , ('BitVector.shiftL#, 'shlA)
    , ('BitVector.shiftR#, 'lshrA)
    , ('BitVector.fromInteger#, 'fromIntegerA2)
    , ('BitVector.unpack#, 'bv2bit)
    , ('BitVector.pack#, 'bit2bv)
    , ('BitVector.size#, 'sizeA)
    , ('BitVector.xToBV, 'id)
    , ('BitVector.toInteger#, 'toIntegerA)
    , ('BitVector.slice#, 'sliceA)
    , ('(BitVector.++#), 'concatA)
    -- , ('BitVector.minBound#, 'minBoundBitVector)

    , ('Bit.msb#, 'msb#)
    , ('Bit.eq##, 'eqbit)
    , ('Bit.neq##, 'neqbit)
    , ('Bit.high, 'highbit)
    , ('Bit.low, 'lowbit)
    ]
  }

data BitVec n where
  BitVecZ :: BitVec 0
  BitVecP :: 1 <= n => Pantomime.BitVec n -> BitVec n

withSize
  :: forall n r
   . Pantomime.KnownNat n
  => (n ~ 0 => r)
  -> (1 <= n => r)
  -> r
withSize zero pos = case Pantomime.natVal @n of
  0 -> case unsafeEq @n @0 of Dict -> zero
  _ -> case unsafeAxiom @(1 <= n) of Dict -> pos

-- | Helper function to wrap a 'Pantomime' bit-vector primitive.
nullary
  :: forall n
   . Pantomime.KnownNat n
  => (1 <= n => Pantomime.BitVec n)
  -> BitVec n
nullary value = withSize @n BitVecZ $ BitVecP value

-- | Helper function to wrap a unary function over 'Pantomime' bit-vector
-- primitives.
unary
  :: forall n
   . (1 <= n => Pantomime.BitVec n -> Pantomime.BitVec n)
  -> BitVec n
  -> BitVec n
unary op = \case
  BitVecZ -> BitVecZ
  BitVecP value -> BitVecP $ op value

-- | Helper function to wrap a binary function over 'Pantomime' bit-vector
-- primitives.
binary
  :: forall n
   . (1 <= n => Pantomime.BitVec n -> Pantomime.BitVec n -> Pantomime.BitVec n)
  -> BitVec n
  -> BitVec n
  -> BitVec n
binary op = \cases
  BitVecZ BitVecZ -> BitVecZ
  (BitVecP lhs) (BitVecP rhs) -> BitVecP $ op lhs rhs

convert :: Pantomime.Bool -> Bool
convert value = Pantomime.ite value True False

equality
  :: forall n
   . Pantomime.Bool
  -> (1 <= n => Pantomime.BitVec n -> Pantomime.BitVec n -> Pantomime.Bool)
  -> BitVec n
  -> BitVec n
  -> Bool
equality zero pos = convert .: \cases
  BitVecZ BitVecZ -> zero
  (BitVecP lhs) (BitVecP rhs) -> pos lhs rhs

instance Eq (BitVec n) where
  (==) = equality Pantomime.True Pantomime.bveq
  (/=) = equality Pantomime.False Pantomime.bvneq

instance Ord (BitVec n) where
  (<=) = equality Pantomime.True Pantomime.bvule
  (<) = equality Pantomime.False Pantomime.bvult

instance KnownNat n => Num (BitVec n) where
  (+) = binary (+)
  (*) = binary (*)
  abs = unary abs
  signum = unary signum
  fromInteger i = nullary $ fromInteger i
  negate = unary negate

instance Bits (BitVec n) where
  (.&.) = binary Pantomime.bvand
  (.|.) = binary Pantomime.bvor
  xor = binary Pantomime.bvxor
  complement = unary Pantomime.bvnot
  -- TODO: I should probably implement these!
  shift = undefined
  rotate = undefined
  bitSize = undefined
  bitSizeMaybe = undefined
  isSigned _ = False
  testBit = undefined
  bit = undefined
  popCount = undefined

type BitVec1 = BitVec 1

addA
  :: forall bv n
   . Coercible BitVec bv
  => KnownNat n
  => bv n
  -> bv n
  -> bv n
addA = coerce $ (+) @(BitVec n)

subA
  :: forall bv n
   . Coercible BitVec bv
  => KnownNat n
  => bv n
  -> bv n
  -> bv n
subA = coerce $ (-) @(BitVec n)

mulA
  :: forall bv n
   . Coercible BitVec bv
  => KnownNat n
  => bv n
  -> bv n
  -> bv n
mulA = coerce $ (*) @(BitVec n)

complementA
  :: forall bv n
   . Coercible BitVec bv
  => bv n
  -> bv n
complementA = coerce $ complement @(BitVec n)

negateA
  :: forall bv n
   . Coercible BitVec bv
  => KnownNat n
  => bv n
  -> bv n
negateA = coerce $ negate @(BitVec n)

-- | Absolute number for (signed) bitvector.
absA
  :: forall bv n
   . Coercible BitVec bv
  => KnownNat n
  => bv n
  -> bv n
absA = coerce $ abs @(BitVec n)

andA
  :: forall bv n
   . Coercible BitVec bv
  => bv n
  -> bv n
  -> bv n
andA = coerce $ (.&.) @(BitVec n)

orA
  :: forall bv n
   . Coercible BitVec bv
  => bv n
  -> bv n
  -> bv n
orA = coerce $ (.|.) @(BitVec n)

xorA
  :: forall bv n
   . Coercible BitVec bv
  => bv n
  -> bv n
  -> bv n
xorA = coerce $ xor @(BitVec n)

-- | Helper function to create shift instances.
shiftA
  :: forall bv n
   . Coercible BitVec bv
  => Pantomime.Embeddable (Pantomime.BitVec Pantomime.PlatformWordSize) Int#
  => KnownNat n
  => (Pantomime.BitVec n -> Pantomime.BitVec n -> Pantomime.BitVec n)
  -> bv n
  -> Int
  -> bv n
shiftA f = coerce $ \val idx -> nullary do
  let val' = case val of BitVecP inner -> inner
  let idx' = let !(I# inner) = idx in Pantomime.project inner
  -- FIXME: A shift past the bitsize is undefined behaviour. The best thing to
  -- do would be to model it as either an uninterpreted function, or to match
  -- whatever Clash does.
  let idx'' = Pantomime.bvsresize @Pantomime.PlatformWordSize idx'
  f val' idx''

shlA
  :: forall bv n
   . Coercible BitVec bv
  => Pantomime.Embeddable (Pantomime.BitVec Pantomime.PlatformWordSize) Int#
  => KnownNat n
  => bv n
  -> Int
  -> bv n
shlA = shiftA Pantomime.bvshl

ashrA
  :: forall bv n
   . Coercible BitVec bv
  => Pantomime.Embeddable (Pantomime.BitVec Pantomime.PlatformWordSize) Int#
  => KnownNat n
  => bv n
  -> Int
  -> bv n
ashrA = shiftA Pantomime.bvashr

lshrA
  :: forall bv n
   . Coercible BitVec bv
  => Pantomime.Embeddable (Pantomime.BitVec Pantomime.PlatformWordSize) Int#
  => KnownNat n
  => bv n
  -> Int
  -> bv n
lshrA = shiftA Pantomime.bvlshr

eqA
  :: forall bv n
   . Coercible BitVec bv
  => bv n
  -> bv n
  -> Bool
eqA = coerce $ (==) @(BitVec n)

neqA
  :: forall bv n
   . Coercible BitVec bv
  => bv n
  -> bv n
  -> Bool
neqA = coerce $ (/=) @(BitVec n)

ltA
  :: forall bv n
   . Coercible BitVec bv
  => bv n
  -> bv n
  -> Bool
ltA = coerce $ (<) @(BitVec n)

leA
  :: forall bv n
   . Coercible BitVec bv
  => bv n
  -> bv n
  -> Bool
leA = coerce $ (<=) @(BitVec n)

gtA
  :: forall bv n
   . Coercible BitVec bv
  => bv n
  -> bv n
  -> Bool
gtA = coerce $ (>) @(BitVec n)

geA
  :: forall bv n
   . Coercible BitVec bv
  => bv n
  -> bv n
  -> Bool
geA = coerce $ (>=) @(BitVec n)

fromIntegerA
  :: forall bv n
   . Coercible BitVec bv
  => KnownNat n
  => Integer
  -> bv n
fromIntegerA = coerce $ fromInteger @(BitVec n)

fromIntegerA2
  :: forall bv n
   . Coercible BitVec bv
  => KnownNat n
  => Natural
  -> Integer
  -> bv n
fromIntegerA2 _ = fromIntegerA

toIntegerA
  :: forall bv n
   . Coercible BitVec bv
  => bv n
  -> Integer
toIntegerA = coerce \case
  BitVecZ -> 0
  BitVecP x -> Pantomime.i2hsi $ Pantomime.bv2i x

bvcoerceA
  :: forall bv bv' n
   . Coercible BitVec bv
  => Coercible BitVec bv'
  => bv n
  -> bv' n
bvcoerceA = coerce

sizeA
  :: forall n bv
   . KnownNat n
  => bv n
  -> Int
sizeA _ = fromInteger $ TypeLits.natVal @n Proxy

concatA
  :: forall l r bv
   . Coercible BitVec bv
  => bv l
  -> bv r
  -> bv (l + r)
concatA = coerce @(BitVec l -> BitVec r -> BitVec (l + r)) \cases
  BitVecZ rhs -> rhs
  lhs BitVecZ -> lhs
  (BitVecP lhs) (BitVecP rhs) -> case unsafeAxiom @(1 <= l + r) of
    Dict -> BitVecP $ Pantomime.bvconcat lhs rhs

sliceA
  :: forall hi lo top bv
   . Coercible BitVec bv
  => bv (hi + 1 + top)
  -> SNat hi
  -> SNat lo
  -> bv (hi + 1 - lo)
sliceA bv SNat {} SNat {} = coerce go bv
  where
    go :: BitVec (hi + 1 + top) -> BitVec (hi + 1 - lo)
    go x = runIdentity do
      -- SAFETY: Reduced, you get '0 <= hi + top', which is true for all
      -- natural numbers.
      Dict <- pure $ unsafeAxiom @(1 <= hi + 1 + top)
      let x' = case x of BitVecP inner -> inner

      let hi = Pantomime.SNat @hi
      let one = Pantomime.SNat @1
      let lo = Pantomime.SNat @lo
      -- TODO: I think this is a requirement on the function. I'm not sure if
      -- you could violate it on the call somehow. Any sound usage of this
      -- function should satisfy this though! It says that the slice is
      -- non-negative sized.
      Dict <- pure $ unsafeAxiom @(lo <= hi + 1)
      Pantomime.SNat @width <- pure $ hi Pantomime.%+ one Pantomime.%- lo

      -- SAFETY: If you reduce this term, you end up with '0 <= top'.
      Dict <- pure $ unsafeAxiom @(lo + width <= hi + 1 + top)

      pure $ nullary (Pantomime.bvselect @lo @width x')

resizeA
  :: forall l r bv
   . Coercible BitVec bv
  => KnownNat r
  => (1 <= r => Pantomime.BitVec l -> Pantomime.BitVec r)
  -> bv l
  -> bv r
resizeA f = coerce @(BitVec l -> BitVec r) \case
  BitVecZ -> 0
  BitVecP x -> nullary $ f x

zresizeA
  :: forall l r bv
   . Coercible BitVec bv
  => KnownNat r
  => bv l
  -> bv r
zresizeA = resizeA Pantomime.bvzresize

sresizeA
  :: forall l r bv
   . Coercible BitVec bv
  => KnownNat r
  => bv l
  -> bv r
sresizeA = resizeA Pantomime.bvsresize

-- TODO: Also make a signed one.
enumFromU
  :: forall bv n
   . Coercible BitVec bv
  => bv n
  -> [bv n]
enumFromU = coerce @(BitVec n -> [BitVec n]) \case
  BitVecZ -> [BitVecZ]
  BitVecP value -> BitVecP <$> go value
  where
    upper :: Pantomime.KnownNat n => 1 <= n => Pantomime.BitVec n
    upper = -1

    go :: 1 <= n => Pantomime.BitVec n -> [Pantomime.BitVec n]
    go x = runIdentity do
      Dict <- pure $ Pantomime.bvnat x
      pure $ x : if
        | x == upper -> []
        | otherwise -> go $ Pantomime.bvadd x 1

bv2bit
  :: Coercible BitVec bv
  => Coercible BitVec1 Bit
  => bv 1
  -> Bit
bv2bit = coerce

bit2bv
  :: Coercible BitVec bv
  => Coercible BitVec1 Bit
  => Bit
  -> bv 1
bit2bv = coerce

eqbit :: Coercible (BitVec 1) Bit => Bit -> Bit -> Bool
eqbit = coerce $ (==) @(BitVec 1)

neqbit :: Coercible (BitVec 1) Bit => Bit -> Bit -> Bool
neqbit = coerce $ (/=) @(BitVec 1)

lowbit :: Coercible (BitVec 1) Bit => Bit
lowbit = coerce $ BitVecP @1 0

highbit :: Coercible (BitVec 1) Bit => Bit
highbit = coerce $ BitVecP @1 1

msb#
  :: forall n bv bit
   . Coercible BitVec bv
  => Coercible BitVec1 bit
  => bv n
  -> bit
msb# = coerce \case
  -- NOTE: Fetching the most significant bit from an empty bitvector doesn't
  -- really make sense, but Clash allows it so we need to have behaviour for it.
  -- TODO: Maybe we should check what the behaviour is of clash in this respect.
  BitVecZ -> BitVecP 0
  BitVecP x -> runIdentity do
    Dict <- pure $ Pantomime.bvnat x
    let n = Pantomime.SNat @n
    let one = Pantomime.SNat @1
    Pantomime.SNat @idx <- pure $ n Pantomime.%- one

    -- SAFETY: 'idx = n - 1', thus this inequality must also hold.
    Dict <- pure $ unsafeAxiom @(idx + 1 <= n)
    let result = Pantomime.bvselect @idx @1 x
    pure $ BitVecP result
