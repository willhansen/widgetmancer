// Taken from https://github.com/abcperf/trait-alias-macro

macro_rules! trait_alias {
    ($vis:vis trait $name:ident = $($base:tt)+) => {
        $vis trait $name: $($base)+ { }
        impl<T: $($base)+> $name for T { }
    };
}

pub(crate) use trait_alias;
