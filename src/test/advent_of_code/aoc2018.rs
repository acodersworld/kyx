#[cfg(test)]
use crate::rust_function_ctx::{RustFunctionCtx, RustValue};
#[cfg(test)]
use crate::test::utils;
#[cfg(test)]
use crate::vm;

#[cfg(test)]
mod day1 {
    use super::*;

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

        vm.create_function("fn readinput() -> [string]", &input)
            .expect("Failed to create function");

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

        vm.create_function("fn readinput() -> [string]", &input)
            .expect("Failed to create function");

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

#[cfg(test)]
mod day2 {
    use super::*;

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

        vm.create_function("fn readinput() -> [string]", &input)
            .expect("Failed to create function");

        let src = include_str!("aoc2018/day2_part1.kyx");

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

        vm.create_function("fn readinput() -> [string]", &input)
            .expect("Failed to create function");

        let src = include_str!("aoc2018/day2_part2.kyx");

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 3);
        assert_eq!(printer.strings[0], "Pair found!");
        assert_eq!(printer.strings[1], "omlvgdokxfncvqyersasjwziup");
        assert_eq!(printer.strings[2], "omlvgdokxfncvqyersasjlziup");
    }
}

#[cfg(test)]
mod day3 {
    use super::*;

    fn input(ctx: &mut dyn RustFunctionCtx) {
        let mut v = vec![];

        for l in include_str!("aoc2018/day3-input.txt").lines() {
            v.push(l.to_string());
        }

        ctx.set_result(RustValue::StringVector(v));
    }

    #[test]
    fn part_1_and_2() {
        let mut printer = utils::TestPrinter::new();
        let mut vm = vm::VM::new(&mut printer);

        vm.create_function("fn readinput() -> [string]", &input)
            .expect("Failed to create function");

        let src = include_str!("aoc2018/day3.kyx");
        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 2);
        assert_eq!(printer.strings[0], "113576");
        assert_eq!(printer.strings[1], "825");
    }
}

#[cfg(test)]
mod day4 {
    use super::*;

    fn input(ctx: &mut dyn RustFunctionCtx) {
        let mut v = vec![];

        for l in include_str!("aoc2018/day4-input.txt").lines() {
            v.push(l.to_string());
        }

        ctx.set_result(RustValue::StringVector(v));
    }

    #[test]
    fn part_1_and_2() {
        let mut printer = utils::TestPrinter::new();
        let mut vm = vm::VM::new(&mut printer);

        vm.create_function("fn readinput() -> [string]", &input)
            .expect("Failed to create function");

        let src = include_str!("aoc2018/day4.kyx");

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 2);
        assert_eq!(printer.strings[0], "94040");
        assert_eq!(printer.strings[1], "39940");
    }
}

#[cfg(test)]
mod day5 {
    use super::*;

    fn input(ctx: &mut dyn RustFunctionCtx) {
        // not sure why the trim_end is needed, there seems to be a newline at the end which is not
        // in the file
        let input = include_str!("aoc2018/day5-input.txt").trim_end();
        ctx.set_result(RustValue::Str(input.to_string()));
    }

    #[test]
    fn part_1_and_2() {
        let mut printer = utils::TestPrinter::new();
        let mut vm = vm::VM::new(&mut printer);

        vm.create_function("fn readinput() -> string", &input)
            .expect("Failed to create function");

        let src = include_str!("aoc2018/day5.kyx");

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 2);
        assert_eq!(printer.strings[0], "9390");
        assert_eq!(printer.strings[1], "5898");
    }
}

#[cfg(test)]
mod day6 {
    use super::*;

    fn input(ctx: &mut dyn RustFunctionCtx) {
        let mut v = vec![];

        for l in include_str!("aoc2018/day6-input.txt").lines() {
            v.push(l.to_string());
        }

        ctx.set_result(RustValue::StringVector(v));
    }

    #[test]
    fn part_1() {
        let mut printer = utils::TestPrinter::new();
        let mut vm = vm::VM::new(&mut printer);

        vm.create_function("fn readinput() -> [string]", &input)
            .expect("Failed to create function");

        let src = include_str!("aoc2018/day6-p1.kyx");

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 1);
        assert_eq!(printer.strings[0], "3969");
    }

    #[test]
    fn part_2() {
        let mut printer = utils::TestPrinter::new();
        let mut vm = vm::VM::new(&mut printer);

        vm.create_function("fn readinput() -> [string]", &input)
            .expect("Failed to create function");

        let src = include_str!("aoc2018/day6-p2.kyx");

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 1);
        assert_eq!(printer.strings[0], "42123");
    }
}

#[cfg(test)]
mod day7 {
    use super::*;

    fn input(ctx: &mut dyn RustFunctionCtx) {
        let mut v = vec![];

        for l in include_str!("aoc2018/day7-input.txt").lines() {
            v.push(l.to_string());
        }

        ctx.set_result(RustValue::StringVector(v));
    }

    #[test]
    fn part_1_and_2() {
        let mut printer = utils::TestPrinter::new();
        let mut vm = vm::VM::new(&mut printer);

        vm.create_function("fn readinput() -> [string]", &input)
            .expect("Failed to create function");

        let src = include_str!("aoc2018/day7.kyx");

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 27);
        assert_eq!(printer.strings[0..26].concat(), "ABDCJLFMNVQWHIRKTEUXOZSYPG");
        assert_eq!(printer.strings[26], "896");
    }
}

#[cfg(test)]
mod day8 {
    use super::*;

    fn input(ctx: &mut dyn RustFunctionCtx) {
        let input = include_str!("aoc2018/day8-input.txt").trim();
        ctx.set_result(RustValue::Str(input.to_string()));
    }

    #[test]
    fn part_1_and_2() {
        let mut printer = utils::TestPrinter::new();
        let mut vm = vm::VM::new(&mut printer);

        vm.create_function("fn readinput() -> string", &input)
            .expect("Failed to create function");

        let src = include_str!("aoc2018/day8.kyx");

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 2);
        assert_eq!(printer.strings[0], "46829");
        assert_eq!(printer.strings[1], "37450");
    }
}

