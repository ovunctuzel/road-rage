SRCS=`ls utexas/map/*.scala utexas/map/make/*.scala utexas/ui/*.scala utexas/*.scala`
SWING="/usr/share/java/scala-swing.jar"
# TODO switch to ant, damnit.
BUILD_OPTS="-d .build -deprecation -cp $SWING"
RUN_OPTS="-cp .build:$SWING"                 # TODO memory?

echo Compiling...
fsc $BUILD_OPTS $SRCS

# How did the compile go?
if [[ $? != "0" ]]; then
  exit 1
fi
echo Done compiling.
echo
