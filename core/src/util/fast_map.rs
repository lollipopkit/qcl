#[cfg(feature = "fast_hash")]
pub type FastHashMap<K, V> = rustc_hash::FxHashMap<K, V>;

#[cfg(not(feature = "fast_hash"))]
pub type FastHashMap<K, V> = std::collections::HashMap<K, V>;

#[inline]
pub fn fast_hash_map_new<K, V>() -> FastHashMap<K, V> {
    #[cfg(feature = "fast_hash")]
    {
        rustc_hash::FxHashMap::default()
    }
    #[cfg(not(feature = "fast_hash"))]
    {
        std::collections::HashMap::new()
    }
}

#[inline]
pub fn fast_hash_map_with_capacity<K, V>(capacity: usize) -> FastHashMap<K, V> {
    #[cfg(feature = "fast_hash")]
    {
        rustc_hash::FxHashMap::with_capacity_and_hasher(capacity, Default::default())
    }
    #[cfg(not(feature = "fast_hash"))]
    {
        std::collections::HashMap::with_capacity(capacity)
    }
}
