/// As taken from: https://fedevitale.medium.com/thread-safe-queue-in-rust-1ed1acb9b93e
/// https://github.com/rawnly/queue-rs
use std::{
    collections::VecDeque,
    sync::{Condvar, Mutex},
};

/// A trait describing the general behaviour of a Queue
pub trait Queue<T> {
    /// Creates a new, empty queue
    fn new() -> Self;

    /// Push a new item in the queue
    fn push(&self, value: T);

    /// Removes an item from the queue
    fn pop(&self) -> T;

    /// Returns the size of the queue
    fn len(&self) -> usize;

    /// checks if the queue is empty
    fn is_empty(&self) -> bool;
}

/// A FIFO queue implemented using a VecDeque and a Mutex
#[derive(Debug)]
pub struct FifoQueue<T> {
    /// The underlying data structure of the queue
    data: Mutex<VecDeque<T>>,
    cv: Condvar,
}

impl<T> Queue<T> for FifoQueue<T> {
    /// Creates a new, empty queue
    fn new() -> Self {
        Self {
            data: Mutex::new(VecDeque::new()),
            cv: Condvar::new(),
        }
    }

    /// Adds an element to the back of the queue
    fn push(&self, value: T) {
        let mut data = self.data.lock().unwrap();
        data.push_back(value);
        self.cv.notify_one();
    }

    /// Removes an element from the front of the queue
    /// Returns None if the queue is empty
    fn pop(&self) -> T {
        let mut data = self.data.lock().unwrap();

        while data.is_empty() {
            data = self.cv.wait(data).unwrap();
        }

        data.pop_front().unwrap()
    }

    /// Returns the size of the queue
    fn len(&self) -> usize {
        let data = self.data.lock().unwrap();
        data.len()
    }

    /// Checks if the queue is empty
    fn is_empty(&self) -> bool {
        let data = self.data.lock().unwrap();
        data.is_empty()
    }
}

#[cfg(test)]
mod test {
    use std::{sync::Arc, thread};

    use super::*;

    #[test]
    fn test_basic_functionalities() {
        let queue = FifoQueue::new();

        // Test push and pop
        queue.push(1);
        queue.push(2);
        queue.push(3);

        assert_eq!(queue.pop(), 1);
        assert_eq!(queue.pop(), 2);
        assert_eq!(queue.pop(), 3);

        // Test size and is_empty
        assert_eq!(queue.len(), 0);
        assert!(queue.is_empty());

        queue.push(4);
        queue.push(5);

        assert_eq!(queue.len(), 2);
        assert!(!queue.is_empty());
    }

    #[test]
    fn test_queue_thread_safety() {
        // createa a queue of numbers
        let queue = Arc::new(FifoQueue::<i32>::new());

        let q1 = queue.clone();
        let t1 = thread::spawn(move || {
            q1.push(1);
            q1.push(2);
        });

        let q2 = queue.clone();
        let t2 = thread::spawn(move || {
            q2.push(3);
            q2.push(4)
        });

        t1.join().unwrap();
        t2.join().unwrap();

        assert_eq!(queue.len(), 4);
    }

    #[test]
    fn test_concurrent_pushes_and_pops() {
        let queue = Arc::new(FifoQueue::new());

        let queue1 = queue.clone();
        let handle1 = thread::spawn(move || {
            for i in 0..1000 {
                queue1.push(i);
            }
        });

        let queue2 = queue.clone();
        let handle2 = thread::spawn(move || {
            for _ in 0..1000 {
                queue2.pop();
            }
        });

        handle1.join().unwrap();
        handle2.join().unwrap();

        assert!(queue.is_empty());
    }

    #[test]
    fn test_concurrent_mixed_operations() {
        let queue = Arc::new(FifoQueue::new());

        let queue1 = queue.clone();
        let handle1 = thread::spawn(move || {
            for i in 0..1000 {
                queue1.push(i);
                queue1.pop();
            }
        });

        let queue2 = queue.clone();
        let handle2 = thread::spawn(move || {
            for i in 0..1000 {
                queue2.push(i);
                queue2.pop();
            }
        });

        handle1.join().unwrap();
        handle2.join().unwrap();

        assert!(queue.is_empty());
    }
}
