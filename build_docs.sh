cargo run --example print_usage > book/.generated-built-ins.md
cargo run --example print_file_examples > book/.generated-examples.md
cargo readme > README.md
mdbook build