FROM haskell:latest
ADD . /code
WORKDIR /code
CMD ["/bin/bash"]