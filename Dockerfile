FROM fedora:29

RUN dnf install -y cargo git

RUN git clone https://github.com/WaDelma/mini-pl

WORKDIR /mini-pl

RUN cargo build --release

CMD ["target/release/mini-repl"]
