#!/bin/sh -e
set -x

cd $(dirname $0)
home=$(pwd)
name=$(basename $home)
name_and_date=${name}-$(date --iso)

TMPDIR=`mktemp -d /tmp/dist.XXXXXXXXXX`
cleanup() {
    cd
    rm -rf $TMPDIR
}
trap cleanup exit

mkdir -p doc/
make
make -C tutorial/

git tag -f $name_and_date
git archive --prefix=$name_and_date/ $name_and_date | \
    ( cd $TMPDIR && tar xvf - )

echo '(progn (load "dist.lisp") (quit))' | clbuild lisp 

rsync -a doc tutorial $TMPDIR/$name_and_date

cd $TMPDIR

tgz=$TMPDIR/${name_and_date}.tgz
tar czf $tgz $name_and_date
gpg -b -a $tgz

mkdir -p ~/bob/public_html/cxml-stp/download/

rsync -av \
    $name_and_date/doc \
    $name_and_date/tutorial \
    $name_and_date/*.html \
    $name_and_date/*.css \
    ~/bob/public_html/cxml-stp/

rsync $tgz $tgz.asc ~/bob/public_html/cxml-stp/download/

rm -f ~/bob/public_html/cxml-stp/download/cxml-stp.tar.gz 
rm -f ~/bob/public_html/cxml-stp/download/cxml-stp.tar.gz.asc

ln -sf ${name_and_date}.tgz ~/bob/public_html/cxml-stp/download/cxml-stp.tar.gz
ln -sf ${name_and_date}.tgz.asc ~/bob/public_html/cxml-stp/download/cxml-stp.tar.gz.asc

echo done
exit 0
rsync -av ~/bob/public_html bob.askja.de
