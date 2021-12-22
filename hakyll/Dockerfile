FROM haskell-ubuntu-stack:ghc-8.8.4

USER root

COPY . /home/chris/w
RUN chown chris:chris -R /home/chris/w

USER chris

WORKDIR /home/chris/w

RUN whoami && stack build --copy-bins

RUN rm -rf /home/chris/w

CMD /home/chris/.local/bin/site
