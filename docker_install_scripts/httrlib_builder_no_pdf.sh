# building httr doxigen comments (namespace and .rd files)
cd /workspace/httrlib
Rscript doxygen.r
cd /workspace/

#building httrlib
R CMD build httrlib
echo "Files in $(pwd):"
ls -lh
f="$(find . -maxdepth 1 -regextype posix-egrep -regex '^./httrlib_[0-9]{1,4}\.[0-9]{1,4}\.[0-9]{1,4}\.tar\.gz$')"
if [ -z "$f" ]; then
    echo "Error: No tar file found after building httrlib."
    exit 1
fi
mv "$f" "httrlib.tar.gz"
R CMD INSTALL --build httrlib.tar.gz