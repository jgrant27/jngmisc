#/usr/bin/env sh

MONO_PATH="mono"
NUGET_PATH="./packages/NuGet.exe"
NUGET_OPTIONS="-OutputDirectory packages -ExcludeVersion -Verbosity normal"
NUGET_PACKAGES="FAKE xunit"

## Install certs (on Linux)
# sudo mozroots --import --machine --sync
# sudo certmgr -ssl -m https://go.microsoft.com
# sudo certmgr -ssl -m https://nugetgallery.blob.core.windows.net
# sudo certmgr -ssl -m https://nuget.org


for package in `echo $NUGET_PACKAGES`
do
    echo "Installing package $package ..."
    $MONO_PATH $NUGET_PATH install $package $NUGET_OPTIONS
done

exit 0
