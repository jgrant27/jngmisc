#/usr/bin/env sh

MONO_PATH="mono"
NUGET_PATH="./packages/NuGet.exe"
NUGET_OPTIONS="-OutputDirectory packages -ExcludeVersion -Verbosity normal"
NUGET_PACKAGES="FAKE xunit"

for package in `echo $NUGET_PACKAGES`
do
    echo "Installing package $package ..."
    $MONO_PATH $NUGET_PATH install $package $NUGET_OPTIONS
done

exit 0
