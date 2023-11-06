// Taken from https://github.com/abcperf/trait-alias-macro

macro_rules! trait_alias_macro {
    ($vis:vis trait $name:ident = $($base:tt)+) => {
        $vis trait $name: $($base)+ { }
        impl<T: $($base)+> $name for T { }
    };
}

pub(crate) use trait_alias_macro;

macro_rules! function_full_name {
    () => {{
        fn f() {}
        fn type_name_of<T>(_: T) -> &'static str {
            std::any::type_name::<T>()
        }
        let name = type_name_of(f);
        name.strip_suffix("::f").unwrap()
    }};
}
pub(crate) use function_full_name;

macro_rules! function_short_name {
    () => {{
        fn f() {}
        fn type_name_of<T>(_: T) -> &'static str {
            std::any::type_name::<T>()
        }
        let name = type_name_of(f);

        // Find and cut the rest of the path
        match &name[..name.len() - 3].rfind(':') {
            Some(pos) => &name[pos + 1..name.len() - 3],
            None => &name[..name.len() - 3],
        }
    }};
}
pub(crate) use function_short_name;
