#[derive(Debug, Clone, PartialEq)]
pub struct RoundRobinIterator<IteratorType> {
    iters: Vec<IteratorType>,
    index_of_next_iterator: usize,
}

pub fn round_robin<IteratorType>(iters: Vec<IteratorType>) -> RoundRobinIterator<IteratorType> {
    RoundRobinIterator {
        iters,
        index_of_next_iterator: 0,
    }
}

impl<IteratorType, ItemType> Iterator for RoundRobinIterator<IteratorType>
where
    IteratorType: Iterator<Item = ItemType>,
{
    type Item = ItemType;

    fn next(&mut self) -> Option<Self::Item> {
        let next_val = self.iters[self.index_of_next_iterator].next();
        self.index_of_next_iterator = (self.index_of_next_iterator + 1) % self.iters.len();
        next_val
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use itertools::Itertools;
    use ntest::timeout;

    #[test]
    #[timeout(100)]
    fn test_round_robin() {
        let iter = round_robin(vec![0..10, 100..110]);
        assert_eq!(iter.clone().take(4).collect_vec(), vec![0, 100, 1, 101]);
        assert_eq!(iter.clone().count(), 20);

        assert_eq!(round_robin(vec![0..10, 100..102]).count(), 5);
    }
    #[test]
    #[timeout(100)]
    fn test_one_sided_ranges() {
        let iter = round_robin(vec![0.., 100..]);
        assert_eq!(iter.take(4).collect_vec(), vec![0, 100, 1, 101]);
    }
}
