set -ex

src=$(pwd)
stage=
case $TRAVIS_OS_NAME in
    linux)
        stage=$(mktemp -d)
        cross build --target $TARGET --release
        cp target/$TARGET/release/midl $stage/
        ;;
    osx)
        stage=$(mktemp -d -t tmp)
        cargo build --target $TARGET --release
        cp target/$TARGET/release/midl $stage/
        ;;
    windows)
        stage=$(mktemp -d)
        cargo build --target $TARGET --release
        cp target/$TARGET/release/midl.exe $stage/
        ;;
esac

cd $stage
tar czf $src/$CRATE_NAME-$TRAVIS_TAG-$TARGET.tar.gz *
cd $src

rm -rf $stage
