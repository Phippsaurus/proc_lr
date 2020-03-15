pub(super) trait Escape {
    fn escape(&self) -> String;
}

impl Escape for str {
    fn escape(&self) -> String {
        self
            .replace('"', "\\\"")
            .replace('[', "\\[")
            .replace(']', "\\]")
            .replace('{', "\\{")
            .replace('}', "\\}")
    }
}
