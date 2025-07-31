use std::io;
use std::path::Path;
use std::path::PathBuf;

use itertools::Itertools as _;
use itertools::Position;

type IncludedFile = (PathBuf, io::Result<String>);

#[derive(Debug)]
pub struct IncludePaths {
    quoted: Vec<PathBuf>,
    bracketed: Vec<PathBuf>,
}

impl IncludePaths {
    pub fn new(quoted: Vec<PathBuf>, mut bracketed: Vec<PathBuf>) -> Self {
        bracketed.extend(["/usr/local/include", "/usr/include"].map(PathBuf::from));
        Self { quoted, bracketed }
    }

    fn quoted_search_path<'a>(&'a self, from_filename: &'a Path) -> impl Iterator<Item = &'a Path> {
        from_filename
            .parent()
            .into_iter()
            .chain(self.quoted.iter().map(PathBuf::as_ref))
            .chain(self.bracketed_search_path())
    }

    fn bracketed_search_path(&self) -> impl Iterator<Item = &Path> {
        self.bracketed.iter().map(PathBuf::as_ref)
    }

    pub(super) fn lookup_quoted(&self, from_filename: &Path, filename: &Path) -> IncludedFile {
        lookup(self.quoted_search_path(from_filename), filename)
    }

    pub(super) fn lookup_bracketed(&self, filename: impl AsRef<Path>) -> IncludedFile {
        lookup(self.bracketed_search_path(), filename.as_ref())
    }
}

fn lookup<'a>(dirs: impl Iterator<Item = &'a Path>, filename: &Path) -> IncludedFile {
    for (pos, filename) in dirs.map(|dir| dir.join(filename)).with_position() {
        let result = std::fs::read_to_string(&filename);
        match (pos, &result) {
            (_, Ok(_)) => return (filename, result),
            (_, Err(err)) if !matches!(err.kind(), io::ErrorKind::NotFound) =>
                return (filename, result),
            (Position::First | Position::Middle, Err(_)) => (),
            (Position::Only | Position::Last, _) => return (filename, result),
        }
    }
    unreachable!()
}
