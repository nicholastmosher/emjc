#[macro_use] extern crate criterion;
#[macro_use] extern crate lazy_static;
extern crate emjc_lib as emjc;

use criterion::Criterion;

use std::io::Cursor;

lazy_static! {
    static ref DATA: Vec<(&'static str, &'static [u8])> = vec! [
        ("BinarySearch",                   include_bytes!("./resources/BinarySearch.emj")),
        ("BinaryTree",                     include_bytes!("./resources/BinaryTree.emj")),
        ("BubbleSort",                     include_bytes!("./resources/BubbleSort.emj")),
        ("Factorial",                      include_bytes!("./resources/Factorial.emj")),
        ("GottshallJustinMergeSort",       include_bytes!("./resources/GottshallJustinMergeSort.emj")),
        ("GottshallJustinPrimes",          include_bytes!("./resources/GottshallJustinPrimes.emj")),
        ("HansenSarathiFibonacci",         include_bytes!("./resources/HansenSarathiFibonacci.emj")),
        ("HansenSarathiMergeSort",         include_bytes!("./resources/HansenSarathiMergeSort.emj")),
        ("KalbhorBalvantHeapsort",         include_bytes!("./resources/KalbhorBalvantHeapsort.emj")),
        ("KalbhorBalvantKSelect",          include_bytes!("./resources/KalbhorBalvantKSelect.emj")),
        ("LinearSearch",                   include_bytes!("./resources/LinearSearch.emj")),
        ("LinkedList",                     include_bytes!("./resources/LinkedList.emj")),
        ("LoganThomasArrayFold",           include_bytes!("./resources/LoganThomasArrayFold.emj")),
        ("LoganThomasListSort",            include_bytes!("./resources/LoganThomasListSort.emj")),
        ("MansfieldBrianBindingTightness", include_bytes!("./resources/MansfieldBrianBindingTightness.emj")),
        ("MansfieldBrianOpPrecedence",     include_bytes!("./resources/MansfieldBrianOpPrecedence.emj")),
        ("QuickSort",                      include_bytes!("./resources/QuickSort.emj")),
        ("SmithWilliamMethodOverriding",   include_bytes!("./resources/SmithWilliamMethodOverriding.emj")),
        ("SmithWilliamQuicksort",          include_bytes!("./resources/SmithWilliamQuicksort.emj")),
        ("TreeVisitor",                    include_bytes!("./resources/TreeVisitor.emj")),
    ];
}

fn lexer_benchmark(c: &mut Criterion) {
    for &(name, data) in DATA.iter() {
        let mut reader = Cursor::new(data);
        c.bench_function(&format!("Lex {}", name), move |b| b.iter(|| emjc::lexer::Lexer::new(&mut reader).unwrap()));
    }
}

fn parser_benchmark(c: &mut Criterion) {
    for &(name, data) in DATA.iter() {
        let mut reader = Cursor::new(data);
        let lexer = emjc::lexer::Lexer::new(&mut reader).unwrap();
        let mut parser = emjc::parser::Parser::new(lexer);
        c.bench_function(&format!("Parse {}", name), move |b| b.iter(|| parser.parse_program()));
    }
}

criterion_group!(benches, lexer_benchmark, parser_benchmark);
criterion_main!(benches);
