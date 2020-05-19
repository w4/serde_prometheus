use metered::{metered, HitCount, ResponseTime, Throughput};
use std::sync::Arc;

use tokio::io::AsyncWriteExt;

#[derive(serde::Serialize, metered::MetricRegistry)]
pub struct ServiceMetricRegistry<'a> {
    biz: &'a BizMetrics,
    baz: &'a BazMetrics,
}

#[derive(Default, Debug)]
pub struct Biz {
    metrics: BizMetrics,
}
#[metered(registry = BizMetrics)]
impl Biz {
    #[measure([HitCount, Throughput, ResponseTime])]
    pub async fn bizle(&self) {
        let delay = std::time::Duration::from_millis(rand::random::<u64>() % 30);
        tokio::time::delay_for(delay).await;
    }
}

#[derive(Default, Debug)]
pub struct Baz {
    metrics: BazMetrics,
}
#[metered(registry = BazMetrics)]
impl Baz {
    #[measure([HitCount, Throughput, ResponseTime])]
    pub async fn bazle(&self) {
        let delay = std::time::Duration::from_millis(rand::random::<u64>() % 60);
        tokio::time::delay_for(delay).await;
    }
}

#[tokio::main]
async fn main() {
    let biz = Arc::new(Biz::default());
    let baz = Arc::new(Baz::default());

    let server_handle = tokio::spawn({
        let biz = Arc::clone(&biz);
        let baz = Arc::clone(&baz);

        async move {
            println!("Listening on 127.0.0.1:7878");
            let mut listener = tokio::net::TcpListener::bind("127.0.0.1:7878")
                .await
                .unwrap();

            loop {
                let (mut stream, _) = listener.accept().await.unwrap();

                let mut globals = std::collections::HashMap::new();
                globals.insert("service", "serde_prometheus_example");

                let serialized = metered_serializer_prometheus::to_string(&ServiceMetricRegistry {
                    biz: &biz.metrics,
                    baz: &baz.metrics,
                }, Some("example"), Some(globals))
                .unwrap();

                let response = format!(
                    "HTTP/1.1 200 OK\r\nContent-Length: {}\r\n\r\n{}",
                    serialized.len(),
                    serialized
                );
                stream.write(response.as_bytes()).await.unwrap();
                stream.flush().await.unwrap();
            }
        }
    });

    let baz_handle = tokio::spawn({
        let baz = Arc::clone(&baz);

        async move {
            loop {
                baz.bazle().await;
            }
        }
    });

    let biz_handle = tokio::spawn({
        let biz = Arc::clone(&biz);

        async move {
            loop {
                biz.bizle().await;
            }
        }
    });

    let (join1, join2, join3) = tokio::join!(server_handle, baz_handle, biz_handle);
    join1.unwrap();
    join2.unwrap();
    join3.unwrap();
}
