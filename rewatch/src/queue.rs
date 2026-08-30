use std::{
    collections::VecDeque,
    sync::{Condvar, Mutex},
};

/// A thread-safe FIFO queue whose `pop` operation waits for an item.
#[derive(Debug)]
pub struct FifoQueue<T> {
    items: Mutex<VecDeque<T>>,
    item_available: Condvar,
}

impl<T> Default for FifoQueue<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> FifoQueue<T> {
    pub fn new() -> Self {
        Self {
            items: Mutex::new(VecDeque::new()),
            item_available: Condvar::new(),
        }
    }

    pub fn push(&self, item: T) {
        self.items.lock().unwrap().push_back(item);
        self.item_available.notify_one();
    }

    pub fn pop(&self) -> T {
        let mut items = self.items.lock().unwrap();
        while items.is_empty() {
            items = self.item_available.wait(items).unwrap();
        }
        items.pop_front().unwrap()
    }

    pub fn is_empty(&self) -> bool {
        self.items.lock().unwrap().is_empty()
    }
}

#[cfg(test)]
mod tests {
    use std::{sync::Arc, thread};

    use super::FifoQueue;

    #[test]
    fn preserves_insertion_order() {
        let queue = FifoQueue::new();
        queue.push(1);
        queue.push(2);

        assert_eq!(queue.pop(), 1);
        assert_eq!(queue.pop(), 2);
        assert!(queue.is_empty());
    }

    #[test]
    fn pop_waits_for_an_item() {
        let queue = Arc::new(FifoQueue::new());
        let consumer = Arc::clone(&queue);
        let handle = thread::spawn(move || consumer.pop());

        queue.push(42);

        assert_eq!(handle.join().unwrap(), 42);
    }
}
