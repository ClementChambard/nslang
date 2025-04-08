type Loc = i64;

struct CompleteLoc {
    filename: i8*;
    line: i64;
    col: i64;
};

struct LocRge {
    start: Loc;
    end: Loc;
};
