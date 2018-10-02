set -ex

if [ -z $TRAVIS_TAG ]; then
    case $TRAVIS_OS_NAME in
        linux)
            cargo test -- --test-threads 1
            cargo build
            cross build --target $TARGET
            ;;
        osx)
            cargo build --target $TARGET
            ;;
        windows)
            cargo build --target $TARGET
            ;;
    esac
fi
