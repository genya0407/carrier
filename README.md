# carrier

Helper CLI Tool for container based deployment system.

## How to use

First, initialize your project.

```shell
$ carrier init your-project-name --context your-remote-docker-context --registry your-docker-registry
```

`carrier.json` and `docker-compose.yml` will be created.

```shell
$ cat carrier.json                                                                                   
{
    "images": {
        "web": "Dockerfile"
    },
    "context": "your-remote-docker-context",
    "tag": "v1.0,0",
    "projectName": "your-project-name",
    "registry": "your-docker-registry",
    "environments": {},
    "port": "3000"
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

# equivalent to `docker-commpose --context your-remote-docker-context stop && docker-compose --context your-remote-docker-context up -d`
$ carrier deploy
```
