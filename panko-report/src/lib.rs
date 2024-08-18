pub use panko_derive_report::Report;
pub use panko_lex::Loc;

pub trait Report: std::fmt::Debug {
    fn print(&self);
    fn exit_code(&self) -> u8;
    fn location(&self) -> Loc;
}

impl<'a, T> From<T> for Box<dyn Report + 'a>
where
    T: Report + 'a,
{
    fn from(value: T) -> Self {
        Box::new(value)
    }
}
