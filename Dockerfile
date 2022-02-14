FROM rhub/r-minimal

COPY . /pkg
COPY entrypoint.sh /entrypoint.sh

RUN installr -d -t "icu-dev openssl-dev libgit2-dev libxml2-dev" -a "subversion git git-svn openssl libgit2 libxml2 icu-libs" local::/pkg

ENTRYPOINT ["sh","/entrypoint.sh"]
