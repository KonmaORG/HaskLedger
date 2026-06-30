module HaskLedger.Crypto
  ( sha2_256,
    blake2b_256,
    blake2b_224,
    keccak_256,
    sha3_256,
    ripemd_160,
    integerToByteString,
    byteStringToInteger,
    bls12_381_G1_uncompress,
    bls12_381_G2_uncompress,
    bls12_381_G1_add,
    bls12_381_G2_add,
    bls12_381_G1_scalarMul,
    bls12_381_G2_scalarMul,
    bls12_381_millerLoop,
    bls12_381_finalVerify,
  )
where

import Covenant.Prim
  ( OneArgFunc (BLS12_381_G1_uncompress, BLS12_381_G2_uncompress,
                Blake2b_224, Blake2b_256, Keccak_256, Ripemd_160, Sha2_256, Sha3_256),
    ThreeArgFunc (IntegerToByteString),
    TwoArgFunc (BLS12_381_G1_add, BLS12_381_G1_scalarMul, BLS12_381_G2_add,
                BLS12_381_G2_scalarMul, BLS12_381_finalVerify, BLS12_381_millerLoop,
                ByteStringToInteger),
  )
import HaskLedger.Contract (Condition, Contract, Expr)
import HaskLedger.Internal.Builtin (liftBuiltin1, liftBuiltin2, liftBuiltin3)

sha2_256 :: Contract Expr -> Contract Expr
sha2_256 = liftBuiltin1 Sha2_256

blake2b_256 :: Contract Expr -> Contract Expr
blake2b_256 = liftBuiltin1 Blake2b_256

blake2b_224 :: Contract Expr -> Contract Expr
blake2b_224 = liftBuiltin1 Blake2b_224

keccak_256 :: Contract Expr -> Contract Expr
keccak_256 = liftBuiltin1 Keccak_256

sha3_256 :: Contract Expr -> Contract Expr
sha3_256 = liftBuiltin1 Sha3_256

ripemd_160 :: Contract Expr -> Contract Expr
ripemd_160 = liftBuiltin1 Ripemd_160

-- First arg: endianness (True=big, False=little). Second: output width (0=minimal).
integerToByteString :: Contract Expr -> Contract Expr -> Contract Expr -> Contract Expr
integerToByteString = liftBuiltin3 IntegerToByteString

byteStringToInteger :: Contract Expr -> Contract Expr -> Contract Expr
byteStringToInteger = liftBuiltin2 ByteStringToInteger

bls12_381_G1_uncompress :: Contract Expr -> Contract Expr
bls12_381_G1_uncompress = liftBuiltin1 BLS12_381_G1_uncompress

bls12_381_G2_uncompress :: Contract Expr -> Contract Expr
bls12_381_G2_uncompress = liftBuiltin1 BLS12_381_G2_uncompress

bls12_381_G1_add :: Contract Expr -> Contract Expr -> Contract Expr
bls12_381_G1_add = liftBuiltin2 BLS12_381_G1_add

bls12_381_G2_add :: Contract Expr -> Contract Expr -> Contract Expr
bls12_381_G2_add = liftBuiltin2 BLS12_381_G2_add

bls12_381_G1_scalarMul :: Contract Expr -> Contract Expr -> Contract Expr
bls12_381_G1_scalarMul = liftBuiltin2 BLS12_381_G1_scalarMul

bls12_381_G2_scalarMul :: Contract Expr -> Contract Expr -> Contract Expr
bls12_381_G2_scalarMul = liftBuiltin2 BLS12_381_G2_scalarMul

bls12_381_millerLoop :: Contract Expr -> Contract Expr -> Contract Expr
bls12_381_millerLoop = liftBuiltin2 BLS12_381_millerLoop

bls12_381_finalVerify :: Contract Expr -> Contract Expr -> Contract Condition
bls12_381_finalVerify = liftBuiltin2 BLS12_381_finalVerify
