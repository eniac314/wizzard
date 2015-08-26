YELLOW="\033[1;33m"
RED="\033[0;31m"
ENDCOLOR="\033[0m"

if [ $USER != root ]; then
  echo -e $RED"Error: must be root"
  echo -e $YELLOW"Exiting..."$ENDCOLOR
  exit 0
fi

rm -r ~/.ghc
rm -r ~/.cabal
sudo apt-get purge --auto-remove ghc
sudo apt-get purge --auto-remove haskell-platform


