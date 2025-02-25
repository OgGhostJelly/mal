# MAL
This is a Rust implementation of [mal](https://github.com/kanaka/mal) that I created for fun. It includes most of the features of the original mal, sufficient enough to run the mal implementation of mal.

There may be memory leaks caused by `Rc`s forming circular references, I'll be looking into this soon!

Clone and run it:
```bash
git clone https://github.com/OgGhostJelly/mal.git
cd mal
cargo run
```