#[cfg(test)]
mod day1 {
    use crate::vm;
    use crate::rust_function_ctx::{RustValue, RustFunctionCtx};
    use crate::test::utils;

    fn input(ctx: &mut dyn RustFunctionCtx) {

        let mut v = vec![];
        
        for l in include_str!("aoc2018/day1-input.txt").lines() {
            v.push(l.to_string());
        }

        ctx.set_result(RustValue::StringVector(v));
    }

    #[test]
    fn part1() {
        let mut printer = utils::TestPrinter::new();
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
        let mut printer = utils::TestPrinter::new();
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

mod day2 {
    use crate::vm;
    use crate::rust_function_ctx::{RustValue, RustFunctionCtx};
    use crate::test::utils;

    fn input(ctx: &mut dyn RustFunctionCtx) {

        let mut v = vec![];
        
        for l in include_str!("aoc2018/day2-input.txt").lines() {
            v.push(l.to_string());
        }

        ctx.set_result(RustValue::StringVector(v));
    }

    #[test]
    fn part1() {
        let mut printer = utils::TestPrinter::new();
        let mut vm = vm::VM::new(&mut printer);

        vm.create_function("fn readinput() -> [string]", &input).expect("Failed to create function");

        let src = "
            let lines: [string] = readinput();

            let nlines: int = lines.len();
            let mut ilines: int = 0;
            while ilines < nlines {
                ilines = ilines + 1;
            }
            ilines = 0;

            let mut count_two: int = 0;
            let mut count_three: int = 0;

            while ilines < nlines {

                let h: [char: int] = hash_map<char, int>{};

                let word: string = lines[ilines];
                ilines = ilines + 1;
                let word_len: int = word.len();

                let mut i: int = 0;
                while i < word_len {
                    let c: char = word[i];
                    i = i + 1;

                    if h.contains_key(c) {
                        h[c] = h[c] + 1;
                    }
                    else {
                        h[c] = 1;
                    }
                }

                let mut has_two: int = 0;
                let mut has_three: int = 0;

                let keys: [char] = h.keys();
                let kl: int = keys.len();
                i = 0;
                while i < kl {
                    let count: int = h[keys[i]];
                    if count == 2 {
                        has_two = 1;
                    }
                    if count == 3 {
                        has_three = 1;
                    }

                    i = i + 1;
                }

                if has_two == 1 {
                    count_two = count_two + 1;
                }
                if has_three == 1 {
                    count_three = count_three + 1;
                }
            }

            print(count_two);
            print(count_three);
            print(count_two * count_three);

        ";

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 3);
        assert_eq!(printer.strings[0], "245");
        assert_eq!(printer.strings[1], "29");
        assert_eq!(printer.strings[2], "7105");
    }

    #[test]
    fn part2() {
        let mut printer = utils::TestPrinter::new();
        let mut vm = vm::VM::new(&mut printer);

        vm.create_function("fn readinput() -> [string]", &input).expect("Failed to create function");

        let src = "
            fn find_pair() {
                let lines: [string] = readinput();

                let nlines: int = lines.len();
                let mut ilines: int = 0;
                let mut jlines: int = 0;

                let mut count_two: int = 0;
                let mut count_three: int = 0;

                while ilines < nlines {
                    jlines = 0;
                    while jlines < nlines {
                        let iword: string = lines[ilines];
                        let jword: string = lines[jlines];

                        let len: int = iword.len();
                        let mut iw: int = 0;

                        let mut differences: int = 0;
                        while iw < len {
                            if iword[iw] != jword[iw] {
                                differences = differences + 1;
                                if differences > 1 {
                                    break;
                                }
                            }
                            iw = iw + 1;
                        }

                        if differences == 1 {
                            print(\"Pair found!\");
                            print(iword);
                            print(jword);
                            return;		
                        }
                        jlines = jlines + 1;
                    }
                    ilines = ilines + 1;
                }

                print(\"No pair found!\");
            }

            find_pair();
        ";

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 3);
        assert_eq!(printer.strings[0], "Pair found!");
        assert_eq!(printer.strings[1], "omlvgdokxfncvqyersasjwziup");
        assert_eq!(printer.strings[2], "omlvgdokxfncvqyersasjlziup");
    }

}

