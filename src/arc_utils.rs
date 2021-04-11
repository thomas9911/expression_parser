use std::sync::Arc;

/// Does a `Arc::try_unwrap` and if that fails just clone the inner T
pub fn may_clone<T>(res: Arc<T>) -> T
where
    T: Clone,
{
    match Arc::try_unwrap(res) {
        Ok(val) => val,
        Err(arc) => (*arc).clone(),
    }
}
