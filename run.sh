#!/bin/bash

container=haskell_main_1
docker-compose up -d
docker exec -it  $container ghci
echo -e "\nStopped $(docker stop $container)"

