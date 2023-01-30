#[macro_use]
extern crate criterion;

use criterion::{black_box, Criterion};
use serde::{Serialize, Serializer};
use std::collections::HashMap;

#[derive(Serialize)]
pub struct BasicMetrics {
    some_value: HashMap<String, TakeFirstKey>,
}

pub struct TakeFirstKey(u64);

impl Serialize for TakeFirstKey {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_newtype_struct("!|my_key==<", &self.0)
    }
}

fn bench_big_hashmap(c: &mut Criterion) {
    c.bench_function("serialize", move |b| {
        let mut m = HashMap::new();

        for i in 0..100_000 {
            m.insert(i.to_string(), TakeFirstKey(i));
        }

        let metrics = BasicMetrics { some_value: m };

        #[allow(clippy::needless_borrow)]
        b.iter(|| serde_prometheus::to_string(black_box(&metrics), None, &[]))
    });

    c.bench_function("serialize_json", move |b| {
        let mut m = HashMap::new();

        for i in 0..100_000 {
            m.insert(i.to_string(), TakeFirstKey(i));
        }

        let metrics = BasicMetrics { some_value: m };

        b.iter(|| serde_json::to_string(black_box(&metrics)))
    });
}

criterion_group!(benches, bench_big_hashmap);
criterion_main!(benches);
