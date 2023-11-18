#[cfg(test)]
use crate::vm::Printer;

#[cfg(test)]
pub struct TestPrinter {
    pub strings: Vec<String>,
}

#[cfg(test)]
impl TestPrinter {
    pub fn new() -> TestPrinter {
        TestPrinter {
            strings: Vec::new(),
        }
    }
}

#[cfg(test)]
impl Printer for TestPrinter {
    fn print(&mut self, s: &str) {
        self.strings.push(s.to_string());
    }
}
