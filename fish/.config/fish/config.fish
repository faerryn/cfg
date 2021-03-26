# source profile
if status --is-login && test -z $fish
	set -x fish flounder
	exec sh -l -c 'exec fish -l'
end

# C/C++
set -x CFLAGS (string split -r ' ' -- $CFLAGS)
set -x CXXFLAGS (string split -r ' ' -- $CXXFLAGS)
set -x LDFLAGS (string split -r ' ' -- $LDFLAGS)

# Rust
set -x RUSTFLAGS (string split -r ' ' -- $RUSTFLAGS)
