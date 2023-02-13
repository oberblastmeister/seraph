#define deriveSerializeFixed(ty) \
instance ByteOrder.FixedOrdering b => Serialize (ByteOrder.Fixed b (ty)) where { \
    type IsConstSize _ = True; \
    size = sizeOf (undefined :: (ty)); \
    put = putPrim; \
    get = getPrim; \
    {-# INLINE size #-}; \
    {-# INLINE put #-}; \
    {-# INLINE get #-}; \
}
    
#define deriveSerializePrimWith(ty, putName, getName) \
instance Serialize (ty) where { \
    type IsConstSize _ = True; \
    size = sizeOf (undefined :: (ty)); \
    put = putName; \
    get = getName; \
    {-# INLINE size #-}; \
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
    type IsConstSize (ty a) = IsConstSize a; \
    size = size @a; \
    put = coerce (put @a); \
    get = coerce (get @a); \
    {-# INLINE size #-}; \
    {-# INLINE put #-}; \
    {-# INLINE get #-}; \
}
