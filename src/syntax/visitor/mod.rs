pub mod printer;

pub trait Visitor<T, R=()> {
    fn visit(&mut self, t: T) -> R;
}
