use std::fmt;
use std::io;

pub use panko_derive_report::Report;
pub use panko_lex::Loc;

pub trait Report: fmt::Debug {
    fn print(&self);
    fn write(&self, writer: &mut dyn std::io::Write);
    fn exit_code(&self) -> u8;
    fn location(&self) -> Loc;
}

impl fmt::Display for &dyn Report {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        struct Writer<'a, 'b>(&'b mut fmt::Formatter<'a>);

        impl io::Write for Writer<'_, '_> {
            fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
                self.0.write_str(std::str::from_utf8(buf).unwrap()).unwrap();
                Ok(buf.len())
            }

            fn flush(&mut self) -> io::Result<()> {
                Ok(())
            }
        }

        self.write(&mut Writer(f));
        Ok(())
    }
}

impl<'a, T> From<T> for Box<dyn Report + 'a>
where
    T: Report + 'a,
{
    fn from(value: T) -> Self {
        Box::new(value)
    }
}
