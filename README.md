# aws-lambda-gpg-decrypt

AWS Lambda micro-service for GPG-decrypting objects stored in S3.

## Prepare the container for building the Haskell project

```shell
docker build \
    --build-arg USER_NAME=$(whoami) \
    --build-arg UID=$(id -u) \
    --build-arg GID=$(id -g) \
    --file Dockerfile.build \
    --tag haskell-build-lambda \
    .
```

## Building the Lambda app

### Build within Haskell dev container

```shell
docker run -it --user $(id -u):$(id -g) -v ./project:/home/chris/workspace:U --rm --entrypoint bash haskell-build-lambda -i -c "/home/chris/workspace/build.sh"
```

### Find the built executable

```shell
BOOTSTRAP_PATH=$(find project/dist-newstyle -type f -executable | grep gpg-decrypt-app)
```

### Copy the executable as 'bootstrap' in the same directory as the extended Amazon Linux Dockerfile

```shell
cp "$BOOTSTRAP_PATH" ./bootstrap
```

### Build the Docker image

```shell
docker build --file Dockerfile.deploy --tag aws-gpg-lambda .
```

(Note: for starting an interactive shell)

```shell
docker run -it --user $(id -u):$(id -g) -v ./project:/home/chris/workspace:U --rm --entrypoint bash haskell-build-lambda
```

## Deploying the image to ECR

_awscli_ is required.

- Authentication
```shell
aws ecr get-login-password --region "$AWS_REGION" | docker login --username AWS --password-stdin "$AWS_ACCOUNT_ID.dkr.ecr.$AWS_REGION.amazonaws.com"
```

- Preparing repository

```shell
aws ecr create-repository --repository-name aws-gpg-lambda || true
```
- Tagging image

```shell
docker tag aws-gpg-lambda "$AWS_ACCOUNT_ID.dkr.ecr.$AWS_REGION.amazonaws.com/aws-gpg-lambda:latest"
```

- Pushing image

```shell
docker push "$AWS_ACCOUNT_ID.dkr.ecr.$AWS_REGION.amazonaws.com/haskell-lambda:latest"
```
