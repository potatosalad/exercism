#[macro_export]
macro_rules! hashmap {
    () => {
        ::std::collections::HashMap::new()
    };
    ($($key:expr => $value:expr,)+) => (hashmap!($($key => $value),+));
    ($($key:expr => $value:expr),*) => {
        {
            let mut _hm = ::std::collections::HashMap::new();
            $(
                let _ = _hm.insert($key, $value);
            )*
            _hm
        }
    };
}
