// Taken from https://github.com/abcperf/trait-alias-macro

#[macro_export]
macro_rules! trait_alias {
    ($vis:vis trait $name:ident = $($base:tt)+) => {
        $vis trait $name: $($base)+ { }
        impl<T: $($base)+> $name for T { }
    };
}
