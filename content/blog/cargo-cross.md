+++
title = "Cross Compiling Rust Binaries with Github Actions"
slug = "cargo-cross"
date = "2021-06-27T16:50:00+05:30"
draft = false
+++

Compiling _rust_ is slow, specially on low power devices such as a
_Raspberry Pi_. I build all my rust utilities with my RPi 4 and sync to
the rest with [syncthing](https://syncthing.net/). That was true for
my project, [**sysit**](https://github.com/crodjer/sysit), as well. I
use sysit on all my systems to keep tabs on the resource usage.

Inspired from how [skim](https://github.com/lotabout/skim/blob/master/.github/workflows/publish-github.yml)
utilizes cross compilation to build binaries directly using Github
workflows, I incorporated a similar flow in sysit.

## The Github Workflow
The [workflow](https://github.com/crodjer/sysit/blob/v0.4.0/.github/workflows/release.yml#L39)
to publish binaries does the following:

1. Install the rust toolchain.
2. Build a release binary.
3. Add the binary as a release asset.


### Install and Build: x86-64
Supporting `x86-64` is trivial. All you need to do is specify the OS
in the matrix:

```yaml
strategy:
  matrix:
    include:
    - build: linux
      os: ubuntu-latest
      rust: stable
    - build: macos
      os: macos-latest
      rust: stable
```
And build with:
```yaml
 - name: Install Rust
   run: rustup install ${{ matrix.rust }}
 - name: Build
   run: cargo build --release
```
to get the binary `target/release/sysit`.

### Install and Build: ARM v7 and AARCH64
Adding support for ARM targest is a more involved exercise as we
need cross compilation. This is how the matrix looks:
```yaml
 - build: arm-v7
   os: ubuntu-latest
   rust: stable
   target: armv7-unknown-linux-gnueabihf
 - build: aarch64
   os: ubuntu-latest
   rust: stable
   target: aarch64-unknown-linux-gnu
```

We need to add the target toolchain to the _Install Rust_ step above:
```
rustup target add ${{ matrix.target }}
```
and then update the _Build_ step with:
```
cargo build --release --target ${{ matrix.target }}
```
to get the binary `target/${{ matrix.target }}/release/sysit`.

#### Linking Issues
The above should be enough for most projects. But for some, like
sysit, it fails:
```
/usr/bin/ld: /home/runner/work/sysit/sysit/target/aarch64-unknown-linux-gnu/release/deps/sysinfo-b7d4e594f5eb3b41.sysinfo.4dsxz936-cgu.0.rcgu.o: Relocations in generic ELF (EM: 183)
/usr/bin/ld: /home/runner/work/sysit/sysit/target/aarch64-unknown-linux-gnu/release/deps/sysinfo-b7d4e594f5eb3b41.sysinfo.4dsxz936-cgu.0.rcgu.o: error adding symbols: file in wrong format
collect2: error: ld returned 1 exit status
```

Resolving this was tricky as I had little experience with such errors.
What I eventually found was that for `armv7` and `aarch64`, I needed
to tell cargo to use their respective linkers. For `armv7`, the linker
is `arm-linux-gnueabihf-gcc` (from package: `gcc-arm-linux-gnueabihf`)
and for `aarch64`, the linker is `aarch64-linux-gnu-gcc` (from
package: `gcc-aarch64-linux-gnu`).

We need to tell cargo to use these linkers. That can be done by
adding `.cargo/config` in the package, with the following content:
```toml
[target.aarch64-unknown-linux-gnu]
linker = "aarch64-linux-gnu-gcc"

[target.armv7-unknown-linux-gnueabihf]
linker = "arm-linux-gnueabihf-gcc"
```

Coming back to our workflow, this is our matrix for the two ARM
targets:
```yaml
 - build: arm-v7
   os: ubuntu-latest
   rust: stable
   target: armv7-unknown-linux-gnueabihf
   linker: gcc-arm-linux-gnueabihf
   cross: true
 - build: aarch64
   os: ubuntu-latest
   rust: stable
   target: aarch64-unknown-linux-gnu
   linker: gcc-aarch64-linux-gnu
   cross: true
```
We also need to ask the workflow to install the linkers using this
additional build step, which is run only when _cross_ is _true_:
```yaml
 - name: Install Linker
   if: matrix.cross
   run: |
     sudo apt update
     sudo apt install ${{ matrix.linker }}
```
And update the build step to use cross compilation.
```yaml
 - name: Build
   run: cargo build --release --target ${{ matrix.target }}
```

> When `RUSTFLAGS` is set, the linker configured in `.cargo/config`.
> This may end up breaking the cross build. This issue is being
> tracked by [cargo](https://github.com/rust-lang/cargo/issues/7984).

### Uploading the binary
The binary then can be packaged as a gzipped tarball. This is where I
borrowed from [skim](https://github.com/softprops/action-gh-release)'s workflow.
```yaml
  - name: Package Artifacts
    run: |
      src=$(pwd)
      stage=
      case $RUNNER_OS in
          Linux)
              stage=$(mktemp -d)
              ;;
          macOS)
              stage=$(mktemp -d -t tmp)
              ;;
      esac
      cp target/${{ matrix.target }}/release/sysit $stage/
      cd $stage
      RELEASE_VERSION=${GITHUB_REF#refs/tags/}
      ASSET_NAME="sysit-$RELEASE_VERSION-${{ matrix.target }}.tar.gz"
      ASSET_PATH="$src/$ASSET_NAME"
      echo "ASSET_PATH=$ASSET_PATH" >> $GITHUB_ENV
      tar czf $ASSET_PATH *
      cd $src
```

In addition to the binaries, I also wanted checksum's so that the
released tarballs are verifiable. This can be done by adding a few
lines in the above script:
```bash
        CHECKSUM_PATH="$ASSET_PATH.sha256"
        echo "CHECKSUM_PATH=$CHECKSUM_PATH" >> $GITHUB_ENV
        case $RUNNER_OS in
            Linux)
                sha256sum $ASSET_NAME > $CHECKSUM_PATH
                ;;
            macOS)
                shasum -a 256 $ASSET_NAME > $CHECKSUM_PATH
                ;;
        esac
```
With the release assets generated, the action: [softprops/action-gh-release](https://github.com/softprops/action-gh-release),
makes it easy:
```yaml
 - name: Release
   uses: softprops/action-gh-release@v1
   with:
     files: |
       ${{ env.ASSET_PATH }}
       ${{ env.CHECKSUM_PATH }}
   env:
     GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
```


And that's it! With all this in place, you can build binaries for all of your
desired platforms and have them available for download as
[a github release](https://github.com/crodjer/sysit/releases/latest).
