enum Instuction {
    Push(Value),
    Fetch(String),
    LocalSet(String, Value),
    Set(String, Value),
}
