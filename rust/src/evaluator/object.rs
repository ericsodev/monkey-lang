#[derive(Debug, PartialEq)]
pub enum Object {
    Integer(usize),
    Boolean(bool),
    Null,
}
