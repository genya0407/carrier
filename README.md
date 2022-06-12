# Carrier

CLI Tool for managing container based deployment system.

Inspired by https://blog.p1ass.com/posts/docker-context/

## Usage

### Precondition

- Docker is installed in your local machine
- Docker is installed in your remote machine
- Remote docker context is created
    - You can create it by `docker context create --default-stack-orchestrator=swarm --docker "host=ssh://${SSH_USERNAME}@${SSH_IP}:${SSH_PORT}" your-remote-docker-context`
- Docker registry is ready
    - You may use hub.docker.io or Amazon ECR or GCR
    - You may also launch private docker registry using https://docs.docker.com/registry/
- Reverse proxy is running in your remote machine

### Setup project

Initialize your project.

```shell
$ carrier init your-project-name --context your-remote-docker-context --registry your-docker-registry
```

`carrier.json` and `docker-compose.yml` will be created.

```shell
$ cat carrier.json                                                                                   
{
    "images": {
        "web": "."
    },
    "context": "your-remote-docker-context",
    "tag": "v1.0,0",
    "projectName": "your-project-name",
    "registry": "your-docker-registry",
    "environments": {},
    "port": "3000" # Your web container listens this port
}
```

```shell
$ cat docker-compose.yml                                                                             
version: '3'
services:
  web:
    image: your-docker-registry/your-project-name_web:${TAG}
    command: TODO
    ports:
      - "127.0.0.1:${PORT:?err}:3000"
    restart: always
    links:
      - postgres
    env_file: .env
    depends_on:
      - postgres
  postgres:
    image: postgres:12-alpine
    restart: always
    env_file: .env
    volumes:
      - your-project-name_pg_data:/var/lib/postgresql/data 

volumes:
  your-project-name_pg_data:
    external: true
```

Create Dockerfile for `your-docker-registry/your-project-name_web` image.

```
$ vim Dockerfile
$ cat Dockerfile # example
FROM golang:alpine

WORKDIR /go/src/app
COPY . .

RUN go get -d -v ./...
RUN go install -v ./...
```

Fill `TODO` in `docker-commpose.yml`.

```
$ vim docker-compose.yml
# diff:
-     command: TODO
+     command: /go/bin/some-web-server --port=3000
```

Build images and push them to registry.

```
# This builds and pushes `your-docker-registry/your-project-name_web:v0.1`
$ carrier release v0.1
```

Then, deploy service into your-remote-docker-context.

```
# Create the volume used in docker-compose.yml
$ docker --context your-remote-docker-context volume create --name=your-project-name_pg_data

# Equivalent to `docker-commpose --context your-remote-docker-context stop && docker-compose --context your-remote-docker-context up -d`
$ carrier deploy

# You can watch logs
$ carrier docker-compose -- logs -f
```

### Update applications

If you want to update applications, you do `release` and `deploy`.

```shell
# Edit application
$ vim main.go

# Build docker image and push it.
$ carrier release v0.2

# Deploy new image
$ carrier deploy
```
