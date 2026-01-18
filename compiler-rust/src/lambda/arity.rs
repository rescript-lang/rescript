//! Lambda arity tracking and analysis.
//!
//! This module provides arity information for functions, which is used
//! for currying/uncurrying optimization. Arity tracks how many arguments
//! a function expects and whether it can take additional arguments.

use std::fmt;

/// Arity information for a function.
///
/// This tracks the expected argument counts for a function and whether
/// it can accept additional arguments beyond those tracked.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Arity {
    /// Known arity information.
    ///
    /// The list contains the arity at each application level.
    /// For example, `[2, 3]` means a function that takes 2 arguments
    /// and returns a function that takes 3 arguments.
    ///
    /// The boolean indicates whether the function can take more
    /// arguments than listed (tail). `true` means it can accept
    /// additional arguments (like an exception).
    Info {
        /// List of arities at each application level
        arities: Vec<i32>,
        /// Whether additional arguments can be accepted
        variadic: bool,
    },

    /// Unknown/not-analyzed arity
    Na,
}

impl Arity {
    /// Create a new arity with the given arities and variadic flag.
    pub fn info(arities: Vec<i32>, variadic: bool) -> Self {
        Arity::Info { arities, variadic }
    }

    /// Create an unknown arity.
    pub fn na() -> Self {
        Arity::Na
    }

    /// Arity for a non-function value.
    pub fn non_function() -> Self {
        Arity::Info {
            arities: vec![],
            variadic: false,
        }
    }

    /// Arity for a raise/exception (can take any arguments).
    pub fn raise() -> Self {
        Arity::Info {
            arities: vec![],
            variadic: true,
        }
    }

    /// Merge a new arity level onto the front.
    ///
    /// This is used when analyzing a function that returns another function.
    pub fn merge(&self, n: i32) -> Self {
        match self {
            Arity::Na => Arity::Info {
                arities: vec![n],
                variadic: false,
            },
            Arity::Info { arities, variadic } => {
                let mut new_arities = vec![n];
                new_arities.extend(arities.iter().copied());
                Arity::Info {
                    arities: new_arities,
                    variadic: *variadic,
                }
            }
        }
    }

    /// Check if the first arity is unknown or empty.
    pub fn first_arity_na(&self) -> bool {
        match self {
            Arity::Na => true,
            Arity::Info { arities, .. } => arities.is_empty(),
        }
    }

    /// Get the first arity if available.
    pub fn get_first_arity(&self) -> Option<i32> {
        match self {
            Arity::Na => None,
            Arity::Info { arities, .. } => arities.first().copied(),
        }
    }

    /// Extract all arities as a list.
    ///
    /// Returns an empty list for `Na`.
    pub fn extract_arity(&self) -> Vec<i32> {
        match self {
            Arity::Na => vec![],
            Arity::Info { arities, .. } => arities.clone(),
        }
    }

    /// Check if this is the unknown arity.
    pub fn is_na(&self) -> bool {
        matches!(self, Arity::Na)
    }

    /// Check if this is a non-function (empty arity list, not variadic).
    pub fn is_non_function(&self) -> bool {
        matches!(
            self,
            Arity::Info {
                arities,
                variadic: false
            } if arities.is_empty()
        )
    }

    /// Merge two arities together.
    ///
    /// This finds the common prefix of arities between two functions.
    /// Used when the same variable is assigned different functions
    /// in different branches.
    pub fn merge_arities(
        xs: &[i32],
        ys: &[i32],
        tail_x: bool,
        tail_y: bool,
    ) -> Self {
        fn aux(acc: &mut Vec<i32>, xs: &[i32], ys: &[i32], tail_x: bool, tail_y: bool) -> bool {
            match (xs, ys) {
                ([], []) => tail_x && tail_y,
                ([], rest) if tail_x => {
                    acc.extend(rest.iter().copied());
                    tail_y
                }
                (rest, []) if tail_y => {
                    acc.extend(rest.iter().copied());
                    tail_x
                }
                ([x, xs_rest @ ..], [y, ys_rest @ ..]) if x == y => {
                    acc.push(*x);
                    aux(acc, xs_rest, ys_rest, tail_x, tail_y)
                }
                _ => false,
            }
        }

        let mut acc = Vec::new();
        let variadic = aux(&mut acc, xs, ys, tail_x, tail_y);
        Arity::Info {
            arities: acc,
            variadic,
        }
    }
}

impl fmt::Display for Arity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Arity::Na => write!(f, "?"),
            Arity::Info { arities, variadic } => {
                write!(f, "[")?;
                for (i, arity) in arities.iter().enumerate() {
                    if i > 0 {
                        write!(f, ",")?;
                    }
                    write!(f, "{arity}")?;
                }
                if *variadic {
                    write!(f, " *")?;
                }
                write!(f, "]")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_arity_creation() {
        let na = Arity::na();
        assert!(na.is_na());

        let non_func = Arity::non_function();
        assert!(non_func.is_non_function());

        let raise = Arity::raise();
        match raise {
            Arity::Info { arities, variadic } => {
                assert!(arities.is_empty());
                assert!(variadic);
            }
            _ => panic!("Expected Info"),
        }
    }

    #[test]
    fn test_arity_merge() {
        let na = Arity::na();
        let merged = na.merge(3);
        assert_eq!(merged.get_first_arity(), Some(3));

        let info = Arity::info(vec![2], false);
        let merged = info.merge(3);
        assert_eq!(merged.extract_arity(), vec![3, 2]);
    }

    #[test]
    fn test_first_arity_na() {
        assert!(Arity::na().first_arity_na());
        assert!(Arity::non_function().first_arity_na());
        assert!(!Arity::info(vec![2], false).first_arity_na());
    }

    #[test]
    fn test_get_first_arity() {
        assert_eq!(Arity::na().get_first_arity(), None);
        assert_eq!(Arity::non_function().get_first_arity(), None);
        assert_eq!(Arity::info(vec![2, 3], false).get_first_arity(), Some(2));
    }

    #[test]
    fn test_extract_arity() {
        assert_eq!(Arity::na().extract_arity(), Vec::<i32>::new());
        assert_eq!(Arity::info(vec![2, 3], false).extract_arity(), vec![2, 3]);
    }

    #[test]
    fn test_merge_arities() {
        // Same arities
        let result = Arity::merge_arities(&[2, 3], &[2, 3], false, false);
        match result {
            Arity::Info { arities, variadic } => {
                assert_eq!(arities, vec![2, 3]);
                assert!(!variadic);
            }
            _ => panic!("Expected Info"),
        }

        // Different arities - common prefix
        let result = Arity::merge_arities(&[2, 3], &[2, 4], false, false);
        match result {
            Arity::Info { arities, variadic } => {
                assert_eq!(arities, vec![2]);
                assert!(!variadic);
            }
            _ => panic!("Expected Info"),
        }

        // One is variadic
        let result = Arity::merge_arities(&[2], &[2, 3], true, false);
        match result {
            Arity::Info { arities, variadic } => {
                assert_eq!(arities, vec![2, 3]);
                assert!(!variadic);
            }
            _ => panic!("Expected Info"),
        }

        // Both variadic with same prefix
        let result = Arity::merge_arities(&[2, 3], &[2, 3], true, true);
        match result {
            Arity::Info { arities, variadic } => {
                assert_eq!(arities, vec![2, 3]);
                assert!(variadic);
            }
            _ => panic!("Expected Info"),
        }
    }

    #[test]
    fn test_display() {
        assert_eq!(format!("{}", Arity::na()), "?");
        assert_eq!(format!("{}", Arity::non_function()), "[]");
        assert_eq!(format!("{}", Arity::info(vec![2, 3], false)), "[2,3]");
        assert_eq!(format!("{}", Arity::info(vec![2, 3], true)), "[2,3 *]");
    }

    #[test]
    fn test_arity_equality() {
        assert_eq!(Arity::na(), Arity::na());
        assert_eq!(Arity::non_function(), Arity::info(vec![], false));
        assert_eq!(
            Arity::info(vec![2, 3], false),
            Arity::info(vec![2, 3], false)
        );
        assert_ne!(
            Arity::info(vec![2, 3], false),
            Arity::info(vec![2, 3], true)
        );
        assert_ne!(Arity::na(), Arity::non_function());
    }
}
