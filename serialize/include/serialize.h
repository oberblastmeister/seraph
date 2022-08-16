#define deriveSerializePrimWith(ty, pokeName, peekName) \
instance Serialize (ty) where { \
    unsafeSize# _ = sizeOf# (undefined :: (ty)); \
    {-# INLINE unsafeSize# #-}; \
    unsafeConstSize = ConstSize (sizeOf (undefined :: (ty))); \
    {-# INLINE unsafeConstSize #-}; \
    poke = pokeName; \
    {-# INLINE poke #-}; \
    peek = peekName; \
    {-# INLINE peek #-}; \
}

#define deriveSerializePrim(ty) \
deriveSerializePrimWith(ty, pokePrim, peekPrim)
