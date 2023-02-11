#define deriveSerializeFixed(ty) \
instance ByteOrder.FixedOrdering b => Serialize (ByteOrder.Fixed b (ty)) where { \
    size# _ = sizeOf# (undefined :: (ty)); \
    constSize# _ = ConstSize# (sizeOf# (undefined :: (ty))); \
    put = putPrim; \
    get = getPrim; \
    {-# INLINE size# #-}; \
    {-# INLINE constSize# #-}; \
    {-# INLINE put #-}; \
    {-# INLINE get #-}; \
}
    
#define deriveSerializePrimWith(ty, putName, getName) \
instance Serialize (ty) where { \
    size# _ = sizeOf# (undefined :: (ty)); \
    constSize# _ = ConstSize# (sizeOf# (undefined :: (ty))); \
    put = putName; \
    get = getName; \
    {-# INLINE size# #-}; \
    {-# INLINE constSize# #-}; \
    {-# INLINE put #-}; \
    {-# INLINE get #-}; \
}

#define deriveSerializePrimLE(ty) \
deriveSerializePrimWith(ty, coerce (put @(ByteOrder.Fixed ByteOrder.LittleEndian ty)), coerce (get @(ByteOrder.Fixed ByteOrder.LittleEndian ty)))

#define deriveSerializePrim(ty) \
deriveSerializePrimLE(ty); \
deriveSerializeFixed(ty)

#define deriveSerializeNewtype(ty) \
instance Serialize a => Serialize (ty a) where { \
    size# = coerce (size# @a); \
    constSize# _ = constSize# (proxy# @a); \
    put = coerce (put @a); \
    get = coerce (get @a); \
    {-# INLINE size# #-}; \
    {-# INLINE constSize# #-}; \
    {-# INLINE put #-}; \
    {-# INLINE get #-}; \
}
