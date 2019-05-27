pub fn map<A, B, F>(input: Vec<A>, function: F) -> Vec<B>
where
    F: FnMut(A) -> B,
{
    input.into_iter().map(function).collect::<Vec<B>>()
}
