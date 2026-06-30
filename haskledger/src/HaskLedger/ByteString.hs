module HaskLedger.ByteString
  ( equalsByteString,
    lessThanByteString,
    lessThanEqualsByteString,
    appendByteString,
    lengthByteString,
    indexByteString,
    consByteString,
    mkByteString,
    mkString,
    emptyByteString,
  )
where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Covenant.ASG (Ref (AnId), lit)
import Covenant.Constant (AConstant (AByteString, AString))
import Covenant.Prim
  ( OneArgFunc (LengthOfByteString),
    TwoArgFunc (AppendByteString, ConsByteString, EqualsByteString,
                IndexByteString, LessThanByteString, LessThanEqualsByteString),
  )
import HaskLedger.Contract (Condition, Contract, Expr)
import HaskLedger.Internal.Builtin (liftBuiltin1, liftBuiltin2)

equalsByteString :: Contract Expr -> Contract Expr -> Contract Condition
equalsByteString = liftBuiltin2 EqualsByteString

lessThanByteString :: Contract Expr -> Contract Expr -> Contract Condition
lessThanByteString = liftBuiltin2 LessThanByteString

lessThanEqualsByteString :: Contract Expr -> Contract Expr -> Contract Condition
lessThanEqualsByteString = liftBuiltin2 LessThanEqualsByteString

appendByteString :: Contract Expr -> Contract Expr -> Contract Expr
appendByteString = liftBuiltin2 AppendByteString

lengthByteString :: Contract Expr -> Contract Expr
lengthByteString = liftBuiltin1 LengthOfByteString

indexByteString :: Contract Expr -> Contract Expr -> Contract Expr
indexByteString = liftBuiltin2 IndexByteString

consByteString :: Contract Expr -> Contract Expr -> Contract Expr
consByteString = liftBuiltin2 ConsByteString

mkByteString :: ByteString -> Contract Expr
mkByteString bs = AnId <$> lit (AByteString bs)

mkString :: Text -> Contract Expr
mkString s = AnId <$> lit (AString s)

emptyByteString :: Contract Expr
emptyByteString = mkByteString ""
