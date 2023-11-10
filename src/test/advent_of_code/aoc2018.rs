#[cfg(test)]
mod day1 {
    use crate::vm;
    use crate::rust_function_ctx::{RustValue, RustFunctionCtx};

    struct TestPrinter {
        strings: Vec<String>,
    }

    impl TestPrinter {
        fn new() -> TestPrinter {
            TestPrinter {
                strings: Vec::new(),
            }
        }
    }

    impl vm::Printer for TestPrinter {
        fn print(&mut self, s: &str) {
            self.strings.push(s.to_string());
        }
    }

    fn input(ctx: &mut dyn RustFunctionCtx) {

        let mut v = vec![];
        
        for l in include_str!("aoc2018/day1-input.txt").lines() {
            v.push(l.to_string());
        }

        ctx.set_result(RustValue::StringVector(v));
    }

    #[test]
    fn part1() {
        let mut printer = TestPrinter::new();
        let mut vm = vm::VM::new(&mut printer);

        vm.create_function("fn readinput() -> [string]", &input).expect("Failed to create function");

        let src = "
            let lines: [string] = readinput();

            let len: int = lines.len();
            let mut i:int = 0;

            let mut freq: int = 0;

            while i < len {
                let f: int = lines[i].to_integer();
                freq = freq + f;
                i = i + 1;
            }

            print(freq);
        ";

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 1);
        assert_eq!(printer.strings[0], "576");
    }

    #[test]
    fn part2() {
        let mut printer = TestPrinter::new();
        let mut vm = vm::VM::new(&mut printer);

        vm.create_function("fn readinput() -> [string]", &input).expect("Failed to create function");

        let src = "
            let lines: [string] = readinput();

            let len: int = lines.len();
            let mut i:int = 0;


            let mut seen: [int: int] = hash_map<int, int>{};

            seen[0] = 1;
            let mut freq: int = 0;
            let mut run: int = 1;

            while run == 1 {
                i = 0;
                while i < len {
                    let f: int = lines[i].to_integer();
                    freq = freq + f;

                    if seen.contains_key(freq) {
                        print(freq);
                        run = 0;
                        break;
                    }

                    seen[freq] = 1;
                    i = i + 1;
                }
            }
        ";

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 1);
        assert_eq!(printer.strings[0], "77674");
    }
}

