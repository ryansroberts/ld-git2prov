[![Build Status](http://drone.labs.nice.org.uk/api/badge/github.com/nhsevidence/ld-git2prov/status.svg?branch=master)](http://drone.labs.nice.org.uk/github.com/nhsevidence/ld-git2prov)

[Documentation here](https://nhsevidence.github.io/ld-git2prov)

## Development environment setup

There is a development environment for this provided via a docker container.  This contains all  the tools needed to build, test and run this app.  The source code is linked into the container via a volume (see docker-compose.yml)

*Prerequisites:  You need [docker](https://docs.docker.com/installation/) and [docker-compose](https://docs.docker.com/compose/install/) installed. *

Open a terminal and change directory to the root of this repository.  Now run the following commands to create the environment and log in

Start the container:
```
docker-compose up -d
```
Now login to the container with a bash shell
```
docker exec -it ldgit2prov_git2provdev_1 bash
cd /home/
```

### Build and test
Once the environment has been created (see above) build and test using:

```
./build.sh
```

