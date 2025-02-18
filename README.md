# aws-lambda-gpg-decrypt

AWS Lambda micro-service for GPG-decrypting objects stored in S3.

## Pre-requisites

- Prepare the container for building the Haskell project:

```shell
docker build \
    --build-arg USER_NAME=$(whoami) \
    --build-arg UID=$(id -u) \
    --build-arg GID=$(id -g) \
    --file Dockerfile.build \
    --tag haskell-build-lambda \
    .
```

 - Building

    ```shell
    docker run -it -v ./project:/home/chris/workspace:U --rm --entrypoint bash haskell-build-lambda -i -c "/home/chris/workspace/build.sh"
    ```

    (Note: for starting an interactive shell)

    ```shell
    docker run -it -v ./project:/home/chris/workspace:U --rm --entrypoint bash haskell-build-lambda
    ```


Cabal build:
