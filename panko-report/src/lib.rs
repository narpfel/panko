pub use panko_derive_report::Report;

pub trait Report: std::fmt::Debug {
    fn print(&self);
    fn exit_code(&self) -> i32;
}

impl<'a, T> From<T> for Box<dyn Report + 'a>
where
    T: Report + 'a,
{
    fn from(value: T) -> Self {
        Box::new(value)
    }
}
