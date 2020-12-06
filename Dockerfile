FROM rhub/r-minimal

RUN installr -d -t "bash icu-dev openssl-dev libgit2-dev libxml2-dev subversion git-svn" -a "subversion git-svn openssl libgit2 libxml2 icu-libs" gert gh rvest remotes

COPY . /pkg
COPY entrypoint.sh /entrypoint.sh

RUN R -e 'remotes::install_local("/pkg")'

ENTRYPOINT ["sh","/entrypoint.sh"]
