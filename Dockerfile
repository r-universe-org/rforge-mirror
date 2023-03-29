FROM rhub/r-minimal

COPY . /pkg
COPY entrypoint.sh /entrypoint.sh

RUN installr -d -t "icu-dev openssl-dev libgit2-dev libxml2-dev" -a "openjdk11 subversion git git-svn openssl libgit2 libxml2 icu-libs" local::/pkg

# Needed to shrink repos
RUN wget -q "https://repo1.maven.org/maven2/com/madgag/bfg/1.14.0/bfg-1.14.0.jar" -O "/bfg.jar"

ENTRYPOINT ["sh","/entrypoint.sh"]
