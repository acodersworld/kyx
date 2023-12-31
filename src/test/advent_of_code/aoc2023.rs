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

#[cfg(test)]
mod day9 {
    use super::*;

    fn input(ctx: &mut dyn RustFunctionCtx) {
        let mut v = vec![];

        for l in include_str!("aoc2023/day9-input.txt").lines() {
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

        let src = include_str!("aoc2023/day9.kyx");

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 2);
        assert_eq!(printer.strings[0], "1930746032");
        assert_eq!(printer.strings[1], "1154");
    }
}

#[cfg(test)]
mod day10 {
    use super::*;

    fn input(ctx: &mut dyn RustFunctionCtx) {
        let mut v = vec![];

        for l in include_str!("aoc2023/day10-input.txt").lines() {
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

        let src = include_str!("aoc2023/day10.kyx");

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 2);
        assert_eq!(printer.strings[0], "6846");
        assert_eq!(printer.strings[1], "325");
    }
}

#[cfg(test)]
mod day11 {
    use super::*;

    fn input(ctx: &mut dyn RustFunctionCtx) {
        let mut v = vec![];

        for l in include_str!("aoc2023/day11-input.txt").lines() {
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

        let src = include_str!("aoc2023/day11.kyx");

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 2);
        assert_eq!(printer.strings[0], "9724940");
        assert_eq!(printer.strings[1], "569052586852");
    }
}

#[cfg(test)]
mod day12 {
    use super::*;

    fn input(ctx: &mut dyn RustFunctionCtx) {
        let mut v = vec![];

        for l in include_str!("aoc2023/day12-input.txt").lines() {
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

        let src = include_str!("aoc2023/day12.kyx");

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 2);
        assert_eq!(printer.strings[0], "6852");
        assert_eq!(printer.strings[1], "8475948826693");
    }
}

#[cfg(test)]
mod day13 {
    use super::*;

    fn input(ctx: &mut dyn RustFunctionCtx) {
        let mut v = vec![];

        for l in include_str!("aoc2023/day13-input.txt").lines() {
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

        let src = include_str!("aoc2023/day13.kyx");

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 2);
        assert_eq!(printer.strings[0], "36448");
        assert_eq!(printer.strings[1], "35799");
    }
}

#[cfg(test)]
mod day14 {
    use super::*;

    fn input(ctx: &mut dyn RustFunctionCtx) {
        let mut v = vec![];

        for l in include_str!("aoc2023/day14-input.txt").lines() {
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

        let src = include_str!("aoc2023/day14.kyx");

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 2);
        assert_eq!(printer.strings[0], "108614");
        assert_eq!(printer.strings[1], "96447");
    }
}

#[cfg(test)]
mod day15 {
    use super::*;

    fn input(ctx: &mut dyn RustFunctionCtx) {
        let mut v = vec![];

        for l in include_str!("aoc2023/day15-input.txt").lines() {
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

        let src = include_str!("aoc2023/day15.kyx");

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 2);
        assert_eq!(printer.strings[0], "511215");
        assert_eq!(printer.strings[1], "236057");
    }
}


#[cfg(test)]
mod day16 {
    use super::*;

    fn input(ctx: &mut dyn RustFunctionCtx) {
        let mut v = vec![];

        for l in include_str!("aoc2023/day16-input.txt").lines() {
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

        let src = include_str!("aoc2023/day16.kyx");

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 2);
        assert_eq!(printer.strings[0], "7798");
        assert_eq!(printer.strings[1], "8026");
    }
}

#[cfg(test)]
mod day19 {
    use super::*;

    fn input(ctx: &mut dyn RustFunctionCtx) {
        let mut v = vec![];

        for l in include_str!("aoc2023/day19-input.txt").lines() {
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

        let src = include_str!("aoc2023/day19.kyx");

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 2);
        assert_eq!(printer.strings[0], "383682");
        assert_eq!(printer.strings[1], "117954800808317");
    }
}

#[cfg(test)]
mod day23 {
    use super::*;

    fn input(ctx: &mut dyn RustFunctionCtx) {
        let mut v = vec![];

        for l in include_str!("aoc2023/day23-input.txt").lines() {
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

        let src = include_str!("aoc2023/day23.kyx");

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 2);
        assert_eq!(printer.strings[0], "1998");
        assert_eq!(printer.strings[1], "6434");
    }
}
