#[cfg(test)] use crate::rust_function_ctx::{RustFunctionCtx, RustValue};
#[cfg(test)] use crate::test::utils;
#[cfg(test)] use crate::vm;

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

        vm.create_function("fn readinput() -> [string]", &input)
            .expect("Failed to create function");

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

        let src = "
            let lines: [string] = readinput();

            let mut map: [int: [int: int]] = hash_map<int, [int: int]>{};

            for i : 0..lines.len() {
                let l: string = lines[i][1..];
                let vid: [string] = l.split('@');
                let vcoord: [string] = vid[1].split(':');

                let id: string = vid[0].trim();
                let scoord: string = vcoord[0].trim();
                let coord: [string] = scoord.split(',');
                let sarea: string = vcoord[1].trim();
                let area: [string] = sarea.split('x');

                let start_x: int = coord[0].to_integer();
                let start_y: int = coord[1].to_integer();

                let ax: int = area[0].to_integer();
                let ay: int = area[1].to_integer();

                let end_x: int = start_x + ax;
                let end_y: int = start_y + ay;

                for x : start_x..end_x {
                    for y : start_y..end_y {
                        if map.contains_key(x) {
                        }
                        else {
                            map[x] = hash_map<int, int>{};
                        }

                        let mut mapx: [int: int] = map[x];
                        if mapx.contains_key(y) {
                            let count: int = mapx[y] + 1;
                            mapx[y] = count;
                        }
                        else {
                            mapx[y] = 1;
                        }
                    }
                }
            }

            let mut overlapping_claims: int = 0;
            let xkeys: [int] = map.keys();

            for ix : 0..xkeys.len() {
                let mapx: [int: int] = map[xkeys[ix]];
                let ykeys: [int] = mapx.keys();

                for iy : 0..ykeys.len() {
                    if mapx[ykeys[iy]] > 1 {
                        overlapping_claims = overlapping_claims + 1;
                    }
                }
            }

            // part 1
            print(overlapping_claims);

            union R {
                Overlapped,
                NotOverlapped(string),
            }

            fn is_overlapped(line: string) -> R {
                let l: string = line[1..];
                let vid: [string] = l.split('@');
                let vcoord: [string] = vid[1].split(':');

                let id: string = vid[0].trim();
                let scoord: string = vcoord[0].trim();
                let coord: [string] = scoord.split(',');
                let sarea: string = vcoord[1].trim();
                let area: [string] = sarea.split('x');

                let start_x: int = coord[0].to_integer();
                let start_y: int = coord[1].to_integer();

                let ax: int = area[0].to_integer();
                let ay: int = area[1].to_integer();

                let end_x: int = start_x + ax;
                let end_y: int = start_y + ay;

                for x : start_x..end_x {
                    for y : start_y..end_y {
                        let mut mapx: [int: int] = map[x];
                        if mapx[y] > 1 {
                            return R.Overlapped;
                        }
                    }
                }

                return R.NotOverlapped(id,);
            }

            // part 2
            for i : 0..lines.len() {
                if let R.NotOverlapped(id,) = is_overlapped(lines[i]) {
                    print(id);
                }
            }";

        assert_eq!(vm.interpret(src), Ok(()));
        assert_eq!(printer.strings.len(), 2);
        assert_eq!(printer.strings[0], "113576");
        assert_eq!(printer.strings[1], "825");
    }

}
