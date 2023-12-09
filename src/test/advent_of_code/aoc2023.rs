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

        for l in include_str!("aoc2023/day1-input.txt").lines() {
            v.push(l.to_string());
        }

        ctx.set_result(RustValue::StringVector(v));
    }

    #[test]
    fn part1_and_2() {
        let mut printer = utils::TestPrinter::new();
        let mut vm = vm::VM::new(&mut printer);

        vm.create_function("fn readinput() -> [string]", &input)
            .expect("Failed to create function");

        let src = include_str!("aoc2023/day1.kyx");

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 2);
        assert_eq!(printer.strings[0], "54644");
        assert_eq!(printer.strings[1], "53348");
    }
}

#[cfg(test)]
mod day2 {
    use super::*;

    fn input(ctx: &mut dyn RustFunctionCtx) {
        let mut v = vec![];

        for l in include_str!("aoc2023/day2-input.txt").lines() {
            v.push(l.to_string());
        }

        ctx.set_result(RustValue::StringVector(v));
    }

    #[test]
    fn part1_and_2() {
        let mut printer = utils::TestPrinter::new();
        let mut vm = vm::VM::new(&mut printer);

        vm.create_function("fn readinput() -> [string]", &input)
            .expect("Failed to create function");

        let src = include_str!("aoc2023/day2.kyx");

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 2);
        assert_eq!(printer.strings[0], "2685");
        assert_eq!(printer.strings[1], "83707");
    }
}

#[cfg(test)]
mod day3 {
    use super::*;

    fn input(ctx: &mut dyn RustFunctionCtx) {
        let mut v = vec![];

        for l in include_str!("aoc2023/day3-input.txt").lines() {
            v.push(l.to_string());
        }

        ctx.set_result(RustValue::StringVector(v));
    }

    #[test]
    fn part1_and_2() {
        let mut printer = utils::TestPrinter::new();
        let mut vm = vm::VM::new(&mut printer);

        vm.create_function("fn readinput() -> [string]", &input)
            .expect("Failed to create function");

        let src = include_str!("aoc2023/day3.kyx");

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 2);
        assert_eq!(printer.strings[0], "537832");
        assert_eq!(printer.strings[1], "81939900");
    }
}

#[cfg(test)]
mod day4 {
    use super::*;

    fn input(ctx: &mut dyn RustFunctionCtx) {
        let mut v = vec![];

        for l in include_str!("aoc2023/day4-input.txt").lines() {
            v.push(l.to_string());
        }

        ctx.set_result(RustValue::StringVector(v));
    }

    #[test]
    fn part1_and_2() {
        let mut printer = utils::TestPrinter::new();
        let mut vm = vm::VM::new(&mut printer);

        vm.create_function("fn readinput() -> [string]", &input)
            .expect("Failed to create function");

        let src = include_str!("aoc2023/day4.kyx");

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 2);
        assert_eq!(printer.strings[0], "20855");
        assert_eq!(printer.strings[1], "5489600");
    }
}

#[cfg(test)]
mod day5 {
    use super::*;

    fn input(ctx: &mut dyn RustFunctionCtx) {
        let mut v = vec![];

        for l in include_str!("aoc2023/day5-input.txt").lines() {
            v.push(l.to_string());
        }

        ctx.set_result(RustValue::StringVector(v));
    }

    #[test]
    fn part1_and_2() {
        let mut printer = utils::TestPrinter::new();
        let mut vm = vm::VM::new(&mut printer);

        vm.create_function("fn readinput() -> [string]", &input)
            .expect("Failed to create function");

        let src = include_str!("aoc2023/day5.kyx");

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 2);
        assert_eq!(printer.strings[0], "551761867");
        assert_eq!(printer.strings[1], "57451709");
    }
}

#[cfg(test)]
mod day6 {
    use super::*;

    fn input(ctx: &mut dyn RustFunctionCtx) {
        let mut v = vec![];

        for l in include_str!("aoc2023/day6-input.txt").lines() {
            v.push(l.to_string());
        }

        ctx.set_result(RustValue::StringVector(v));
    }

    #[test]
    fn part1_and_2() {
        let mut printer = utils::TestPrinter::new();
        let mut vm = vm::VM::new(&mut printer);

        vm.create_function("fn readinput() -> [string]", &input)
            .expect("Failed to create function");

        let src = include_str!("aoc2023/day6.kyx");

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 2);
        assert_eq!(printer.strings[0], "2065338");
        assert_eq!(printer.strings[1], "34934171");
    }
}

#[cfg(test)]
mod day8 {
    use super::*;

    fn input(ctx: &mut dyn RustFunctionCtx) {
        let mut v = vec![];

        for l in include_str!("aoc2023/day8-input.txt").lines() {
            v.push(l.to_string());
        }

        ctx.set_result(RustValue::StringVector(v));
    }

    #[test]
    fn part1_and_2() {
        let mut printer = utils::TestPrinter::new();
        let mut vm = vm::VM::new(&mut printer);

        vm.create_function("fn readinput() -> [string]", &input)
            .expect("Failed to create function");

        let src = include_str!("aoc2023/day8.kyx");

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 2);
        assert_eq!(printer.strings[0], "20777");
        assert_eq!(printer.strings[1], "13289612809129");
    }
}
