
-- | A Sequence number for situation where you want to manually manage the
-- lifecycle of some bricks.
module Minitools.Seqnum where

import Prelude (($),(<$>),(+),class Show, show, class Eq, eq, class Ord,compare,map)
import Halogen as H
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson,encodeJson)

newtype Seqnum :: forall k. k -> Type
newtype Seqnum table = Seqnum Int

instance eqSeqnum :: Eq (Seqnum t) where
  eq (Seqnum k1) (Seqnum k2) = eq k1 k2

instance ordSeqnum :: Ord (Seqnum t) where
  compare (Seqnum k1) (Seqnum k2) = compare k1 k2

instance showSeqnum :: Show (Seqnum t) where
  show (Seqnum k) = show k

instance encodeJsonSeqnum :: EncodeJson (Seqnum t) where
  encodeJson (Seqnum k) = encodeJson k

instance decodeJsonSeqnum :: DecodeJson (Seqnum t) where
  decodeJson a = Seqnum <$> decodeJson a

increment :: forall t. Seqnum t -> Seqnum t
increment (Seqnum k) = (Seqnum $ 1+k)

zero :: forall t. Seqnum t
zero = Seqnum 0

-- | Allocates a seqnum from central seqnum registry.
allocate1
  :: forall table m state action slots output.
  H.HalogenM {seqnum::Seqnum table|state} action slots output m (Seqnum table)
allocate1 = map _.seqnum (H.modify (\st0 -> st0 {seqnum = increment st0.seqnum}))

-- | Allocates a seqnum for a table given a central seqnum registry.
allocate
  :: forall registry table m state action slots output.
  H.HalogenM {seqnum::Seqnum registry|state} action slots output m (Seqnum table)
allocate = map copy allocate1

-- | Copies a seqnum from one table to another one.
copy :: forall t1 t2. Seqnum t1 -> Seqnum t2
copy (Seqnum k) = (Seqnum k)
