pub use panko_derive_report::Report;

pub trait Report: std::fmt::Debug {
    fn print(&self);
    fn exit_code(&self) -> i32;
}
