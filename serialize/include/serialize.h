#define deriveSerializePrimWith(ty, putName, getName) \
instance Serialize (ty) where { \
    size# _ = sizeOf# (undefined :: (ty)); \
    {-# INLINE size# #-}; \
    constSize# _ = ConstSize# (sizeOf# (undefined :: (ty))); \
    {-# INLINE constSize# #-}; \
    put = putName; \
    {-# INLINE put #-}; \
    get = getName; \
    {-# INLINE get #-}; \
}

#define deriveSerializePrim(ty) \
deriveSerializePrimWith(ty, putPrim, getPrim)
