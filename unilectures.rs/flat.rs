// Flatten nested iterators
// Inspired by:
// * https://www.youtube.com/watch?v=yozQ9C69pNs
// * https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.flatten

pub fn flatten<O>(outer: O) -> Flatten<O::IntoIter>
where
    O: IntoIterator,
    O::Item: IntoIterator,
{
    Flatten::new(outer.into_iter())
}

pub struct Flatten<O>
where
    O: Iterator,
    O::Item: IntoIterator,
{
    outer: O,
    inner: Option<<O::Item as IntoIterator>::IntoIter>,
}

impl<O> Flatten<O>
where
    O: Iterator,
    O::Item: IntoIterator,
{
    fn new(iter: O) -> Self {
        Flatten {
            outer: iter,
            inner: None,
        }
    }
}

impl<O> Iterator for Flatten<O>
where
    O: Iterator,
    O::Item: IntoIterator,
{
    type Item = <O::Item as IntoIterator>::Item;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(inner_iter) = &mut self.inner {
                if let Some(i) = inner_iter.next() {
                    return Some(i);
                }
                self.inner = None;
            }

            self.inner = Some(self.outer.next()?.into_iter());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::iter::{empty, once};
    use std::vec;

    #[test]
    fn check_empty() {
        assert_eq!(flatten(empty::<Vec<()>>()).count(), 0);
    }

    #[test]
    fn check_once_empty() {
        assert_eq!(flatten(once(empty::<()>())).count(), 0);
    }

    #[test]
    fn check_bunch_of_empty() {
        assert_eq!(flatten(vec![empty::<()>(); 12]).count(), 0);
    }

    #[test]
    fn check_once_once() {
        assert_eq!(flatten(once(once::<()>(()))).count(), 1);
    }

    #[test]
    fn check_once_items() {
        assert_eq!(flatten(once(vec![42])).next(), Some(42));
    }

    #[test]
    fn check_two_once() {
        assert_eq!(
            flatten(vec![vec![13], vec![37]]).collect::<Vec<_>>(),
            vec![13, 37]
        );
    }

    #[test]
    fn check_once_two() {
        assert_eq!(
            flatten(once(vec![42, 13])).collect::<Vec<_>>(),
            vec![42, 13]
        );
    }

    #[test]
    fn check_flatten() {
        let inner1 = vec![2, 45, 6];
        let inner2 = vec![7, 4];
        let outer = vec![inner1, inner2];
        let mut flatten = flatten(outer);

        assert_eq!(flatten.next(), Some(2));
        assert_eq!(flatten.next(), Some(45));
        assert_eq!(flatten.next(), Some(6));
        assert_eq!(flatten.next(), Some(7));
        assert_eq!(flatten.next(), Some(4));
        assert_eq!(flatten.next(), None);
    }

    #[test]
    fn check_flatten_inifinte() {
        let flatten = flatten((0..).map(|i| i..i * 2));

        assert_eq!(
            flatten.take(11).collect::<Vec<_>>(),
            vec![1, 2, 3, 3, 4, 5, 4, 5, 6, 7, 5]
        );
    }
}
