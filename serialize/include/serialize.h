#define deriveSerializeFixed(ty) \
instance FixedOrdering b => Serialize (Fixed b (ty)) where { \
    size# _ = sizeOf# (undefined :: (ty)); \
    {-# INLINE size# #-}; \
    constSize# _ = ConstSize# (sizeOf# (undefined :: (ty))); \
    {-# INLINE constSize# #-}; \
    put = putPrim; \
    {-# INLINE put #-}; \
    get = getPrim; \
    {-# INLINE get #-}; \
}
    
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

#define deriveSerializePrimLE(ty) \
deriveSerializePrimWith(ty, coerce (put @(Fixed LittleEndian ty)), coerce (get @(Fixed LittleEndian ty)))

#define deriveSerializePrim(ty) \
deriveSerializePrimLE(ty); \
deriveSerializeFixed(ty)
