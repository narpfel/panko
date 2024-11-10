use std::mem;
use std::path::Path;

use indexmap::IndexMap;
use panko_lex::Loc;

type LineBreakPositions = Vec<usize>;

#[derive(Debug, Default)]
pub(super) struct Linenos<'a> {
    files: IndexMap<&'a Path, LineBreakPositions>,
}

impl<'a> Linenos<'a> {
    fn add(&mut self, path: &'a Path, src: &'a str) -> (usize, &LineBreakPositions) {
        let entry = self.files.entry(path);
        (
            entry.index(),
            entry.or_insert_with(|| {
                src.split_inclusive('\n')
                    .scan(0, |pos, line| Some(mem::replace(pos, *pos + line.len())))
                    .collect()
            }),
        )
    }

    pub(super) fn lookup(&mut self, loc: Loc<'a>) -> (usize, usize, usize) {
        let (fileno, line_break_positions) = self.add(loc.file(), loc.src());
        let lineno = line_break_positions
            .binary_search(&loc.start())
            .unwrap_or_else(|i| i - 1);
        let line_start = line_break_positions[lineno];
        (fileno, lineno + 1, loc.start() - line_start + 1)
    }
}

impl<'a, 'slf> IntoIterator for &'slf Linenos<'a> {
    type Item = (usize, &'slf &'a Path);

    type IntoIter = impl Iterator<Item = Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.files.keys().enumerate()
    }
}
