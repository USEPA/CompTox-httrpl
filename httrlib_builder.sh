if [ -d ~/R/httrlib ]
then
  rm -r ~/R/httrlib
fi

# building httr doxigen comments (namespace and .rd files)
cd httrlib
Rscript doxygen.r
cd ..
Rscript rd2cmd
#building httr pdf documentation
R CMD Rd2pdf httrlib &
sleep 5
mv httrlib.pdf httrlib/

#building httrlib
R CMD build httrlib
f="$(find . -regextype posix-egrep -regex '^./httrlib_[0-9]{,4}\.[0-9]{,4}\.[0-9]{,4}\.tar\.gz$')" 
mv "$f" "httrlib.tar.gz"
R CMD INSTALL --build httrlib.tar.gz
