use std::ffi::OsStr;
use std::path::Path;

#[derive(Clone, Copy)]
pub(super) struct ConstablePath<'a> {
    bytes: &'a [u8],
}

impl<'a> ConstablePath<'a> {
    pub(super) const fn from_str(s: &'a str) -> Self {
        Self { bytes: s.as_bytes() }
    }

    pub(super) fn new(path: &'a Path) -> Self {
        let bytes = path.as_os_str().as_encoded_bytes();
        Self { bytes }
    }
}

impl AsRef<Path> for ConstablePath<'_> {
    fn as_ref(&self) -> &Path {
        let Self { bytes } = self;
        // SAFETY: `bytes` can only be created from a `&str` or from an existing `Path`
        let os_str = unsafe { OsStr::from_encoded_bytes_unchecked(bytes) };
        os_str.as_ref()
    }
}
