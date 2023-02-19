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

#define deriveSerializePrimDefault(ty) \
deriveSerializePrimWith(ty, coerce (put @(ByteOrder.Fixed DefaultEndian ty)), coerce (get @(ByteOrder.Fixed DefaultEndian ty)))

#define deriveSerializePrim(ty) \
deriveSerializePrimDefault(ty); \
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
