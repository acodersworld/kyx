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
                        if !map.contains_key(x) {
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

        let src = "
            let lines: [string] = readinput();

            struct Event {
                time: int,
                desc: string,
            }

            fn parse_time(s: string) -> int {
                let date_time: string = s.split(']')[0][1..];

                let month: string = date_time[5..7];
                let day: string = date_time[8..10];

                let hour: string = date_time[11..13];
                let minute: string = date_time[14..];

                let seconds: int = (43200 * month.to_integer()) + (1440 * day.to_integer()) + minute.to_integer();
                return seconds;
            }

            let mut events: [Event] = vec<Event>{};

            for i : 0..lines.len() {
                events.push(Event {
                    time = parse_time(lines[i]),
                    desc = lines[i].split(']')[1][1..],
                });
            }

            fn pred(a: Event, b: Event) -> bool {
                return a.time < b.time;
            }
            events.sort(pred);

            struct Guard {
                minutes_asleep: int,
                minutes_awake: [int],
            }

            let mut guards: [int: Guard] = hash_map<int, Guard>{};
            let mut current_id: int = -1;
            let mut current_time: int = -1;
            let mut is_awake: bool = true;
            for i : 0..events.len() {
                let evt: Event = events[i];
                let desc: string = evt.desc;
                if desc.contains(\"Guard\") {
                    if !is_awake {
                        if current_id != -1 {
                            let mut g: Guard = guards[current_id];
                            g.minutes_asleep = g.minutes_asleep + (evt.time - current_time);
                            for t : current_time..evt.time {
                                let m: int = t % 60;
                                g.minutes_awake[m] = g.minutes_awake[m] + 1;
                            }
                        }
                    }

                    is_awake = true;
                    current_time = evt.time;	
                    current_id = desc[7..].split(' ')[0].to_integer();

                    if !guards.contains_key(current_id) {
                        let mut mm: [int] = vec<int>{};
                        for a : 0..60 {
                            mm.push(0);
                        }

                        guards[current_id] = Guard {
                            minutes_asleep = 0,
                            minutes_awake = mm,
                        };
                    }
                }
                else {
                    if desc.contains(\"asleep\") {
                        is_awake = false;

                        current_time = evt.time;

                    }
                    else {
                        is_awake = true;

                        let mut g: Guard = guards[current_id];
                        g.minutes_asleep = g.minutes_asleep + (evt.time - current_time);
                        for t : current_time..evt.time {
                            let m: int = t % 60;
                            g.minutes_awake[m] = g.minutes_awake[m] + 1;
                        }

                        current_time = evt.time;
                    }
                }
            }

            let keys: [int] = guards.keys();
            // part 1
            {

                let mut highest_minutes: int = -1;
                let mut highest_id: int = -1;
                for i : 0..keys.len() {
                    let k: int = keys[i];
                    if guards[k].minutes_asleep > highest_minutes {
                        highest_minutes = guards[k].minutes_asleep;
                        highest_id = k;
                    }
                }

                let g: Guard = guards[highest_id];
                let mut m: [int] = g.minutes_awake;

                let mut most_minute_idx: int = 0;
                let mut most_minute_val: int = m[0];

                for i : 1..m.len() {
                    if m[i] > most_minute_val {
                        most_minute_val = m[i];
                        most_minute_idx = i;
                    }
                }
                print(highest_id * most_minute_idx);
            }


            // part 2
            {
                
                let mut most_minute_idx: int = -1;
                let mut most_minute_val: int = -1;
                let mut highest_id: int = -1;
                for i : 0..keys.len() {
                    let mut m: [int] = guards[keys[i]].minutes_awake;
                    let mut guard_most_minute_idx: int = 0;
                    let mut guard_most_minute_val: int = m[0];

                    for i : 1..m.len() {
                        if m[i] > guard_most_minute_val {
                            guard_most_minute_val = m[i];
                            guard_most_minute_idx = i;
                        }
                    }
                    
                    if guard_most_minute_val > most_minute_val {
                        most_minute_val = guard_most_minute_val;
                        most_minute_idx = guard_most_minute_idx;
                        highest_id = keys[i];
                    }
                }

                print(highest_id * most_minute_idx);
            }";

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
