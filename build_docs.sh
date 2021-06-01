cargo run -p expression_parser --example print_usage > book/.generated-built-ins.md
cargo run -p expression_parser --example print_file_examples > book/.generated-examples.md
cargo readme -r expression_parser --no-title --no-indent-headings > README.md
mdbook build
